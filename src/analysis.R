perform_analysis_for_variables <- function(dataList, sites, variable, wb, filename){
    to_ret <- list()
    for(site in sites){
        if(all(is.na(dataList[[variable]][[site]]$value$Obs))){
            next()
        }

        print(paste("===============Site:", site,"====================="))
        data <- dataList[[variable]][[site]]$value
        meteo_for_data <- dataList[[variable]][[site]]$meteo
        data <- merge(data, meteo_for_data, by="Date")
        data <- data[,!apply(data, 2, function(x) {all(is.na(x))})] # remove all columns with all NA
        data <- data[!apply(data, 1, function(x) {any(is.na(x))}),] # remove all rows with any NA
        data_for_linear_model <- data[,grep("(M.*)|(Obs)",colnames(data))]
        models <- data[,grep("M.*",colnames(data))]
        best_model <- which.min(apply(models,2,function(x){
                                          if(all(is.na(x))){
                                              return()
                                          }
                                          x <- x[!is.na(x)]
                                          y <- data$Obs[!is.na(x)]
                                          if(length(x) < 10){
                                              return()
                                          }
                                          rrmse(x,y)
        }))

        train_index <- sort(sample(1:nrow(data), size = round(0.7*nrow(data)), replace = FALSE))
        selected_for_training <- 1:nrow(data) %in% train_index 
        best_model_name <- names(best_model)
        best_model_value <- models[,best_model]
        performance_table <- matrix(ncol=6, nrow=3)
        rownames(performance_table) <- c("RMSE","RRMSE","R")
        colnames(performance_table) <- c("BEST_individual","MMM","MLR", "RF","XGB", "XBG+")
        mmm <- apply(models,1,median, na.rm=TRUE)
        names(mmm) <- NULL

        performance_table[,1] <- performance_row(data$Obs[!selected_for_training],best_model_value[!selected_for_training])
        performance_table[,2] <- performance_row(data$Obs[!selected_for_training],mmm[!selected_for_training])

        linear_model <- lm(Obs ~ ., data=data_for_linear_model[selected_for_training,])
        lin_model_pred <- predict(linear_model, data_for_linear_model)
        performance_table[,3] <- performance_row(data$Obs[!selected_for_training],lin_model_pred[!selected_for_training])
        random_forest_model <- randomForest(Obs ~ ., data=data_for_linear_model[selected_for_training,], importance=TRUE)
        rf_model_pred <- predict(random_forest_model, data_for_linear_model)
        performance_table[,4] <- performance_row(data$Obs[!selected_for_training],rf_model_pred[!selected_for_training])
        xgb_set <- get_xgb(models, data$Obs, selected_for_training)
        xgb_model <- xgb_set$model
        xgb_model_pred <- xgb_set$prediction
        performance_table[,5] <- performance_row(data$Obs[!selected_for_training],xgb_model_pred[!selected_for_training])
        extended_xgb_set <- get_xgb(cbind.data.frame(models, data$Prec, data$Tmean), data$Obs, selected_for_training)
        extended_xgb_model <- extended_xgb_set$model
        extended_xgb_model_pred <- extended_xgb_set$prediction
        performance_table[,6] <- performance_row(data$Obs[!selected_for_training],extended_xgb_model_pred[!selected_for_training])
        # browser()
        to_write <- cbind.data.frame(data$Date, data$Obs, mmm, best_model_value, lin_model_pred, rf_model_pred, xgb_model_pred, extended_xgb_model_pred)[!selected_for_training,]
        colnames(to_write) <- c("Date","Obs","MMM",best_model_name,"MLR","RF","XGB","XGB+")
        addWorksheet(wb, site)
        addWorksheet(wb, paste0(site,"_performance"))
        writeData(wb, sheet = site, x = to_write)
        writeData(wb, sheet = paste0(site,"_performance"), x = performance_table)
        # write to excel
        saveWorkbook(wb, file = filename, overwrite = TRUE)

        shap_matrix <- matrix(ncol=(length(models) + 2), nrow=4)
        colnames(shap_matrix) <- c(colnames(models),"Precip","Tmean")
        rownames(shap_matrix) <- c("LinearModel","RandomForest","XGBoost","XGBoostPlus")

        shap_matrix[1,] <- c(mean_shp_vals_linear(models,linear_model$coefficients[2:length(linear_model$coefficients)]),NA,NA)
        shap_matrix[2,] <- c(mean_shp_vals_rf(random_forest_model, data_for_linear_model, response_name = "Obs"),NA,NA)
        shap_matrix[3,] <- c(mean_shp_vals_xgb(xgb_model, as.matrix(models))[1:length(models)],NA,NA)
        shap_matrix[4,] <- compute_shap_xgb(extended_xgb_model, as.matrix(cbind.data.frame(models, data$Prec, data$Tmean)))[1:(length(models)+2)]
        addWorksheet(wb, paste0(site,"_shap"))
        writeData(wb, sheet = paste0(site,"_shap"), x = shap_matrix)
        saveWorkbook(wb, file = "first.xlsx", overwrite = TRUE)
        to_ret[[site]] <- list(model_runs = to_write, performance_table=performance_table, shap_matrix=shap_matrix)
    }
        return(to_ret)
}
