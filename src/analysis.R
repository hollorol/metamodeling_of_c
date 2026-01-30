# Main analysis function to perform metamodeling for different variables and sites
# Trains and evaluates multiple models: Best individual model, MMM, MLR, RF, XGB, XGB+
# Uses leave-one-year-out cross-validation for temporal data integrity

perform_analysis_for_variables <- function(dataList, sites, variable, wb, filename, 
                                          validation_method = "loyo_full",
                                          xgb_n_evals = 50,
                                          xgb_nrounds = 1000){
    # validation_method: 
    #   "loyo_full" = leave each year out, average results (default, most robust)
    #   "loyo_single" = leave one random year out (faster but less robust)
    #   "random" = random 70/30 split (not recommended for time series)
    # xgb_n_evals: number of hyperparameter sets to evaluate (50 is good balance)
    # xgb_nrounds: max boosting rounds (1000 with early stopping is usually enough)
    
    to_ret <- list()
    
    for(site in sites){
        # Skip if no observations available
        if(is.null(dataList[[variable]][[site]]) || 
           all(is.na(dataList[[variable]][[site]]$value$Obs))){
            next
        }

        print(paste("=============== Site:", site, "====================="))
        
        # Merge variable data with meteorological data
        data <- dataList[[variable]][[site]]$value
        meteo_for_data <- dataList[[variable]][[site]]$meteo
        data <- merge(data, meteo_for_data, by = "Date")
        
        # Remove columns with all NA values
        data <- data[, !apply(data, 2, function(x) {all(is.na(x))})]
        
        # Remove rows with any NA values
        data <- data[!apply(data, 1, function(x) {any(is.na(x))}), ]
        
        # Prepare data subsets
        data_for_linear_model <- data[, grep("(M.*)|(Obs)", colnames(data))]
        models <- data[, grep("M.*", colnames(data))]
        
        # Find best individual model
        best_model <- which.min(apply(models, 2, function(x){
            if(all(is.na(x))){
                return(Inf)
            }
            x <- x[!is.na(x)]
            y <- data$Obs[!is.na(x)]
            if(length(x) < 10){
                return(Inf)
            }
            rrmse(x, y)
        }))

        # Leave-one-year-out cross-validation
        # Get unique years from the data
        unique_years <- unique(data$Year)
        n_years <- length(unique_years)
        
        # Determine years to test based on validation method
        if(validation_method == "loyo_full" && n_years > 1){
            # Test on each year (up to max 10) and average results
            n_folds <- min(10, n_years)
            if(n_folds < n_years){
                set.seed(123)
                test_years <- sample(unique_years, n_folds)
            } else {
                test_years <- unique_years
            }
            print(paste("  Validation: Leave-One-Year-Out (k-fold, k =", n_folds, ")"))
            print(paste("  Testing years:", paste(test_years, collapse = ", ")))
            print(paste("  Total samples:", nrow(data)))
        } else if(validation_method == "loyo_single" && n_years > 1){
            # Leave one random year out
            set.seed(123)
            test_years <- sample(unique_years, 1)
            print(paste("  Validation: Leave-One-Year-Out (single)"))
            print(paste("  Test year:", test_years, "| Training years:", 
                       paste(unique_years[unique_years != test_years], collapse = ", ")))
        } else if(validation_method == "random" || n_years == 1){
            # Random split (fallback or explicit choice)
            if(n_years == 1){
                warning(paste("Only one year available for site", site, "- using random 70/30 split"))
            } else {
                print(paste("  Validation: Random 70/30 split"))
            }
            train_index <- sort(sample(seq_len(nrow(data)), size = round(0.7 * nrow(data)), replace = FALSE))
            selected_for_training <- seq_len(nrow(data)) %in% train_index
            test_years <- NULL  # Signal for single fold
        } else {
            stop(paste("Unknown validation method:", validation_method))
        } 
        
        best_model_name <- names(best_model)
        best_model_value <- models[, best_model]
        
        # Multi-Model Median (MMM)
        mmm <- apply(models, 1, median, na.rm = TRUE)
        names(mmm) <- NULL
        
        # Initialize storage for k-fold results
        if(is.null(test_years)){
            # Single fold (random split)
            n_folds <- 1
            fold_test_years <- list(NULL)
            fold_train_masks <- list(selected_for_training)
        } else {
            # Multiple folds (LOYO)
            n_folds <- length(test_years)
            fold_test_years <- as.list(test_years)
            fold_train_masks <- lapply(test_years, function(ty) data$Year != ty)
        }
        
        # Storage for fold-wise predictions and performance
        fold_performance <- array(NA, dim = c(3, 6, n_folds))
        dimnames(fold_performance) <- list(
            c("RMSE", "RRMSE", "R"),
            c("BEST_individual", "MMM", "MLR", "RF", "XGB", "XGB+"),
            paste0("Fold", 1:n_folds)
        )
        
        all_predictions <- list()
        final_models <- list()
        
        # Loop through folds
        for(fold_idx in 1:n_folds){
            if(n_folds > 1){
                print(paste("  --- Fold", fold_idx, "/", n_folds, ": test year =", fold_test_years[[fold_idx]], "---"))
            }
            
            selected_for_training <- fold_train_masks[[fold_idx]]
            n_train <- sum(selected_for_training)
            n_test <- sum(!selected_for_training)
            
            if(n_folds > 1){
                print(paste("    Training samples:", n_train, "| Test samples:", n_test))
            }

            # Train and evaluate Multiple Linear Regression (MLR)
            linear_model <- lm(Obs ~ ., data = data_for_linear_model[selected_for_training, ])
            lin_model_pred <- predict(linear_model, data_for_linear_model)
            fold_performance[, 3, fold_idx] <- performance_row(data$Obs[!selected_for_training], 
                                                      lin_model_pred[!selected_for_training])
            
            # Train and evaluate Random Forest (RF)
            random_forest_model <- randomForest(Obs ~ ., 
                                               data = data_for_linear_model[selected_for_training, ], 
                                               importance = TRUE)
            rf_model_pred <- predict(random_forest_model, data_for_linear_model)
            fold_performance[, 4, fold_idx] <- performance_row(data$Obs[!selected_for_training], 
                                                      rf_model_pred[!selected_for_training])
            
            # Train and evaluate XGBoost (XGB)
            if(fold_idx == 1 || n_folds > 1){
                print(paste("    Training XGBoost model..."))
            }
            xgb_set <- get_xgb(models, data$Obs, selected_for_training,
                              n_evals = xgb_n_evals,
                              nrounds = xgb_nrounds,
                              verbose = (fold_idx == 1))  # Only verbose for first fold
            xgb_model <- xgb_set$model
            xgb_model_pred <- xgb_set$prediction
            fold_performance[, 5, fold_idx] <- performance_row(data$Obs[!selected_for_training], 
                                                      xgb_model_pred[!selected_for_training])
            
            # Train and evaluate extended XGBoost with meteorological features (XGB+)
            if(fold_idx == 1 || n_folds > 1){
                print(paste("    Training XGBoost+ model..."))
            }
            extended_features <- cbind.data.frame(models, data$Prec, data$Tmean)
            extended_xgb_set <- get_xgb(extended_features, data$Obs, selected_for_training,
                                        n_evals = xgb_n_evals,
                                        nrounds = xgb_nrounds,
                                        verbose = (fold_idx == 1))  # Only verbose for first fold
            extended_xgb_model <- extended_xgb_set$model
            extended_xgb_model_pred <- extended_xgb_set$prediction
            fold_performance[, 6, fold_idx] <- performance_row(data$Obs[!selected_for_training], 
                                                      extended_xgb_model_pred[!selected_for_training])
            
            # Evaluate best individual model and MMM (don't need retraining)
            fold_performance[, 1, fold_idx] <- performance_row(data$Obs[!selected_for_training], 
                                                      best_model_value[!selected_for_training])
            fold_performance[, 2, fold_idx] <- performance_row(data$Obs[!selected_for_training], 
                                                      mmm[!selected_for_training])
            
            # Store predictions for this fold (last fold's predictions for output)
            if(fold_idx == n_folds){
                final_models <- list(
                    linear = linear_model,
                    rf = random_forest_model,
                    xgb = xgb_model,
                    xgb_plus = extended_xgb_model
                )
                all_predictions <- list(
                    best = best_model_value,
                    mmm = mmm,
                    mlr = lin_model_pred,
                    rf = rf_model_pred,
                    xgb = xgb_model_pred,
                    xgb_plus = extended_xgb_model_pred,
                    test_mask = !selected_for_training
                )
            }
        }
        
        # Average performance across folds
        performance_table <- apply(fold_performance, c(1, 2), mean, na.rm = TRUE)
        
        if(n_folds > 1){
            print(paste("  Averaged performance across", n_folds, "folds:"))
            print(paste("    RMSE:", paste(colnames(performance_table), round(performance_table[1,], 6), sep="=", collapse=" | ")))
        } else {
            print("  Test set performance (RMSE):")
            print(paste("    ", paste(colnames(performance_table), round(performance_table[1,], 6), sep=": ", collapse=" | ")))
        }
        
        # Prepare predictions output (from last fold)
        to_write <- cbind.data.frame(
            data$Date, 
            data$Obs, 
            all_predictions$mmm, 
            all_predictions$best, 
            all_predictions$mlr, 
            all_predictions$rf, 
            all_predictions$xgb, 
            all_predictions$xgb_plus
        )[all_predictions$test_mask, ]
        colnames(to_write) <- c("Date", "Obs", "MMM", best_model_name, "MLR", "RF", "XGB", "XGB+")
        
        # Write predictions to Excel
        addWorksheet(wb, site)
        addWorksheet(wb, paste0(site, "_performance"))
        writeData(wb, sheet = site, x = to_write)
        writeData(wb, sheet = paste0(site, "_performance"), x = performance_table)
        saveWorkbook(wb, file = filename, overwrite = TRUE)

        # Calculate SHAP values for feature importance (using models from last fold)
        shap_matrix <- matrix(ncol = (length(models) + 2), nrow = 4)
        colnames(shap_matrix) <- c(colnames(models), "Precip", "Tmean")
        rownames(shap_matrix) <- c("LinearModel", "RandomForest", "XGBoost", "XGBoostPlus")

        # SHAP for Linear Model
        shap_matrix[1, ] <- c(mean_shp_vals_linear(models, 
                                                   final_models$linear$coefficients[2:length(final_models$linear$coefficients)]), 
                             NA, NA)
        
        # SHAP for Random Forest
        shap_matrix[2, ] <- c(mean_shp_vals_rf(final_models$rf, data_for_linear_model, 
                                              response_name = "Obs"), 
                             NA, NA)
        
        # SHAP for XGBoost
        shap_matrix[3, ] <- c(mean_shp_vals_xgb(final_models$xgb, as.matrix(models))[1:length(models)], 
                             NA, NA)
        
        # SHAP for XGBoost+ (with meteorological features)
        shap_matrix[4, ] <- compute_shap_xgb(final_models$xgb_plus, 
                                            as.matrix(cbind.data.frame(models, data$Prec, data$Tmean)))[1:(length(models) + 2)]
        
        # Write SHAP values to Excel
        addWorksheet(wb, paste0(site, "_shap"))
        writeData(wb, sheet = paste0(site, "_shap"), x = shap_matrix)
        saveWorkbook(wb, file = filename, overwrite = TRUE)
        
        # Store results
        to_ret[[site]] <- list(model_runs = to_write, 
                              performance_table = performance_table, 
                              shap_matrix = shap_matrix)
    }
    
    return(to_ret)
}

