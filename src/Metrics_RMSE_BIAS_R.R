library(dplyr)

rds_file_path <- "C:/Users/PadFl/Desktop/DokThori/metamodel/data/results_1000.rds"
output_directory <- "C:/Users/PadFl/Desktop/DokThori/metamodel/metrics"



dat <- readRDS(file = rds_file_path)

variables <- c("NEE", "GPP", "RECO")
sites <- c("C1", "C2", "G3", "G4")


rmse <- function(predicted, observed) {
  sqrt(mean((predicted - observed)^2, na.rm = TRUE))
}

bias <- function(predicted, observed) {
  mean(predicted - observed, na.rm = TRUE)
}



for (variable in variables) {
  
  output_filename <- file.path(output_directory, paste0(variable, "_metrics.csv"))
  
  file_conn <- file(output_filename, "w")
  cat(paste("Opened", output_filename, "for writing.\n"))
  
    for (site in sites) {
    
    cat(paste("Processing Variable:", variable, "- Site:", site, "\n"))
    
    # Check if data exists for the current variable/site combination to avoid errors
    if (!is.null(dat[[variable]]) && !is.null(dat[[variable]][[site]])) {
      
      model_runs <- dat[[variable]][[site]][['model_runs']]
      best_model_name <- colnames(model_runs)[4]
      model_names <- c(best_model_name, "MMM", "MLR", "RF", "XGB", "XGB_plus")
      # rewriting the column names cause the + sign is AAAAAAAAAAAAAAAA
      colnames(model_runs) <- c("Date", "Obs", "MMM", best_model_name, "MLR", "RF", "XGB", "XGB_plus")
      
      observed_data <- model_runs$Obs
      
      all_metrics <- list()
    
      for (model_name in model_names) {
        
        predicted_data <- model_runs[[model_name]]
        
        rmse_val <- round(rmse(predicted_data, observed_data), 3)
        bias_val <- round(bias(predicted_data, observed_data), 3)
        cor_val <- tryCatch({
          round(cor(predicted_data, observed_data, use = "pairwise.complete.obs", method = "pearson"), 3)
        }, error = function(e) {
          return(NA) # Return NA if correlation cannot be computed
        })
        
        model_metrics <- data.frame(
          Model = model_name,
          RMSE = rmse_val,
          BIAS = bias_val,
          Pearson_Correlation = cor_val
        )
        all_metrics[[model_name]] <- model_metrics
      }
 
      final_metrics_df <- do.call(rbind, all_metrics)
      
      rownames(final_metrics_df) <- final_metrics_df$Model
      final_metrics_df$Model <- NULL # Remove the model name column
      transposed_df <- as.data.frame(t(final_metrics_df))
      
      header_row <- paste(site, paste(colnames(transposed_df), collapse = ","), sep = ",")
      writeLines(header_row, file_conn)
      
      # Loop through the metrics (rows of the transposed df) and write each one
      for (i in 1:nrow(transposed_df)) {
        metric_name <- rownames(transposed_df)[i]
        metric_values <- paste(transposed_df[i, ], collapse = ",")
        metric_row <- paste(metric_name, metric_values, sep = ",")
        writeLines(metric_row, file_conn)
      }
      
      # Write a blank line to separate the data for the next site
      writeLines("", file_conn)
      
      cat(paste("   -> Metrics for site", site, "written to file.\n"))
      
    } else {
    # AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAa
      
      cat(paste("   -> WARNING: Data not found for Variable:", variable, "Site:", site, "\n\n"))
    }
  }
  
  close(file_conn)
  cat(paste("-> All metrics for", variable, "saved to:", output_filename, "\n\n"))
}

