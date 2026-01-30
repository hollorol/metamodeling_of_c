# XGBoost model training with random search hyperparameter tuning
# Uses parallel processing for efficient hyperparameter search
library(xgboost)
library(parallel)

get_xgb <- function(features,
                    obs,
                    selected_for_training,
                    n_evals = 50,            # Total number of random evaluations
                    nrounds = 1000,          # Max rounds for xgb.train
                    early_stop = 20,         # Early stopping for xgb.train
                    nthread = 1,             # Threads for each xgb.train call
                    n_cores = 15,            # Number of parallel cores
                    seed = 123,
                    verbose = TRUE) {
    
    if(verbose){
        message(sprintf("XGBoost hyperparameter search: %d evaluations", n_evals))
    }
    
    # --- 1) Build FULL training + validation matrices/DMatrix ---
    message("Setting up data...")
    Xtr_full <- as.matrix(features[selected_for_training, , drop = FALSE])
    ytr_full <- obs[selected_for_training]
    Xva <- as.matrix(features[!selected_for_training, , drop = FALSE])
    yva <- obs[!selected_for_training]

    dtrain_full <- xgb.DMatrix(Xtr_full, label = ytr_full)  # For final training
    dvalid <- xgb.DMatrix(Xva, label = yva)                 # For validation during tuning
    watchlist <- list(eval = dvalid)

    # --- 1b) Split training data into tuning train/validation sets ---
    # This creates a validation set WITHIN the training data (not using test data!)
    set.seed(seed)
    n_train_full <- nrow(Xtr_full)
    
    # Use 80/20 split of training data for hyperparameter tuning
    tuning_split <- 0.8
    n_tuning_train <- floor(tuning_split * n_train_full)
    
    if (n_tuning_train < 20) {
        warning("Training data is very small. Using all for tuning without internal validation.")
        X_tuning_train <- Xtr_full
        y_tuning_train <- ytr_full
        X_tuning_valid <- Xtr_full
        y_tuning_valid <- ytr_full
    } else {
        tuning_indices <- sample(1:n_train_full, size = n_tuning_train)
        X_tuning_train <- Xtr_full[tuning_indices, , drop = FALSE]
        y_tuning_train <- ytr_full[tuning_indices]
        X_tuning_valid <- Xtr_full[-tuning_indices, , drop = FALSE]
        y_tuning_valid <- ytr_full[-tuning_indices]
        
        if(verbose){
            message(sprintf("  Tuning split: %d train / %d validation (from %d training samples)",
                          nrow(X_tuning_train), nrow(X_tuning_valid), n_train_full))
        }
    }

    # --- 2) Define bounds and generate random parameter sets ---
    if(verbose){
        message(sprintf("  Generating %d hyperparameter sets...", n_evals))
    }
    set.seed(seed)
    bounds <- list(
        eta              = c(0.01, 0.3),
        max_depth        = c(3L, 10L),
        subsample        = c(0.6, 1.0),
        colsample_bytree = c(0.6, 1.0),
        min_child_weight = c(1L, 10L),
        lambda           = c(0.01, 10.0),
        alpha            = c(0.0, 10.0)
    )

    random_params_list <- vector("list", n_evals)
    for (i in 1:n_evals) {
        random_params_list[[i]] <- list(
            eta              = runif(1, bounds$eta[1], bounds$eta[2]),
            max_depth        = sample(bounds$max_depth[1]:bounds$max_depth[2], 1),
            subsample        = runif(1, bounds$subsample[1], bounds$subsample[2]),
            colsample_bytree = runif(1, bounds$colsample_bytree[1], bounds$colsample_bytree[2]),
            min_child_weight = sample(bounds$min_child_weight[1]:bounds$min_child_weight[2], 1),
            lambda           = runif(1, bounds$lambda[1], bounds$lambda[2]),
            alpha            = runif(1, bounds$alpha[1], bounds$alpha[2])
        )
    }

    # --- 3) Objective function for Random Search evaluation ---
    # Pass raw matrices instead of DMatrix to avoid fork issues
    xgb_objective_rs <- function(i, param_list, X_tr, y_tr, X_val, y_val, max_rounds, early, seed_base) {
        set.seed(seed_base + i)
        
        # Create DMatrix inside the forked process
        dtrain_local <- xgb.DMatrix(X_tr, label = y_tr)
        dvalid_local <- xgb.DMatrix(X_val, label = y_val)
        
        params_xgb <- list(
            objective        = "reg:squarederror",
            eval_metric      = "rmse",
            tree_method      = "exact",
            eta              = param_list$eta,
            max_depth        = as.integer(param_list$max_depth),
            subsample        = param_list$subsample,
            colsample_bytree = param_list$colsample_bytree,
            min_child_weight = as.integer(param_list$min_child_weight),
            lambda           = param_list$lambda,
            alpha            = param_list$alpha
        )

        bst <- tryCatch({
            xgb.train(
                params                = params_xgb,
                data                  = dtrain_local,
                watchlist             = list(eval = dvalid_local),
                nrounds               = max_rounds,
                early_stopping_rounds = early,
                verbose               = 0,
                nthread               = 1
            )
        }, error = function(e) {
            return(NULL)
        })

        if (is.null(bst) || is.null(bst$best_iteration) || bst$best_iteration < 1) {
            return(list(Score = -Inf, nrounds = NA, Params = param_list))
        } else {
            return(list(Score = -bst$best_score,
                 nrounds = bst$best_iteration,
                 Params = param_list))
        }
    }

    # --- 4) Evaluate parameter sets in parallel ---
    if(verbose){
        message(sprintf("  Evaluating %d parameter sets with %d cores...", n_evals, n_cores))
        message(sprintf("  Each evaluation trains up to %d rounds with early stopping", nrounds))
    }
    start_time <- Sys.time()

    # Use PSOCK cluster instead of forking to avoid BLAS/LAPACK thread deadlocks
    # PSOCK is slower but more reliable across systems
    cl <- makeCluster(n_cores, type = "PSOCK")
    on.exit(stopCluster(cl), add = TRUE)
    
    # Export necessary objects to cluster
    clusterExport(cl, c("xgb_objective_rs", "random_params_list", "X_tuning_train", "y_tuning_train",
                        "X_tuning_valid", "y_tuning_valid", "nrounds", "early_stop", "seed"), 
                  envir = environment())
    
    # Load xgboost in each worker
    clusterEvalQ(cl, library(xgboost))
    
    # Run evaluations in parallel - using INTERNAL validation split (not test data!)
    results_list <- parLapply(cl, 1:n_evals, function(i) {
        xgb_objective_rs(i, random_params_list[[i]], X_tuning_train, y_tuning_train, 
                        X_tuning_valid, y_tuning_valid, nrounds, early_stop, seed)
    })

    end_time <- Sys.time()
    time_taken <- end_time - start_time
    if(verbose){
        message(sprintf("  Completed in %.1f seconds", as.numeric(time_taken, units = "secs")))
    }

    # --- 5) Find best result ---
    best_score <- -Inf
    best_result <- NULL
    valid_results_count <- 0

    for (res in results_list) {
        if (!is.null(res) && !is.na(res$Score) && is.finite(res$Score)) {
            valid_results_count <- valid_results_count + 1
            if (res$Score > best_score) {
                best_score <- res$Score
                best_result <- res
            }
        }
    }

    if (is.null(best_result)) {
        stop("Random search failed to produce any valid results. Check objective function or parameters.")
    }
    
    if(verbose){
        message(sprintf("  Found best from %d valid evaluations", valid_results_count))
    }

    best_params_values <- best_result$Params
    best_nrounds <- best_result$nrounds
    best_valid_rmse <- -best_score  # Convert back to positive RMSE

    if(verbose){
        message(sprintf("  Best validation RMSE: %.6f", best_valid_rmse))
        message(sprintf("  Best nrounds: %d", best_nrounds))
    }

    # --- 6) Retrain final model on FULL training data ---
    if(verbose){
        message("  Retraining on full training data...")
    }
    final_params <- best_params_values
    final_params$objective   <- "reg:squarederror"
    final_params$eval_metric <- "rmse"
    final_params$tree_method <- "exact"

    final_mod <- xgb.train(
        params  = final_params,
        data    = dtrain_full,  # Use FULL training data
        nrounds = best_nrounds,
        verbose = 0,  # Suppress xgboost output
        nthread = nthread
    )

    # --- 7) Predict on every row ---
    all_preds <- predict(final_mod, as.matrix(features))

    # --- 8) Return results ---
    list(
        model         = final_mod,
        prediction    = all_preds,
        best_params   = best_params_values,
        best_nrounds  = best_nrounds,
        valid_rmse    = best_valid_rmse,
        tuning_time   = time_taken,
        all_results   = results_list
    )
}

