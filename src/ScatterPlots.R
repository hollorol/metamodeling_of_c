library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

################################################################################
################################## OPTIONS #####################################
################################################################################ 
# results rds file
dat <-readRDS(file = "C:/Users/PadFl/Desktop/DokThori/metamodel/data/results_1000.rds")

title_size <- 15 # (the site and the method at the top left)
R2size <- 15 
axis_title_size <- 15 
axis_tick_size <- 15 
diag_line_size <- 1 
dot_size <- 2 

transpose <- TRUE # originally it was a 4x6 grid for the images but this option if TRUE transposes it to 6x4
                  # you will need to probably adjust the width and the height of the image

# output_width <- 20
# output_height <- 13
output_width <- 13
output_height <- 20

output_dpi <- 300  
output_path <- "C:/Users/PadFl/Desktop/DokThori/metamodel/plots/scatter/" 




################################################################################
################################# PLOTTING #####################################
################################################################################


# Define the variables we want to plot
variables_to_plot <- c("GPP", "RECO", "NEE")
#variables_to_plot <- c("GPP")

# Loop through each variable (GPP, RECO, NEE) to create a separate PNG for each
for (variable in variables_to_plot) {
  
  cat(paste0("\nProcessing variable: ", variable, "\n"))
  
  # Get the list of sites for the current variable
  site_names <- names(dat[[variable]])
  
  # Consolidate data from all sites for the current variable into a single long-format data frame
  
  all_sites_data_long_list <- list()
  
  # Use "XGB." for matching, as R converts the "+" to a "."
  model_names_standard <- c("MMM", "MLR", "RF", "XGB", "XGB.")
  
  for (site in site_names) {
    site_list <- dat[[variable]][[site]][['model_runs']]
    
    if (is.null(site_list)) {
      cat(paste0("  Warning: 'model_runs' not found for site '", site, "'. Skipping.\n"))
      next
    }
    
    max_length <- max(sapply(site_list, length))
    padded_list <- lapply(site_list, function(col) {
      if (length(col) < max_length) {
        return(c(col, rep(NA, max_length - length(col))))
      } else {
        return(col)
      }
    })
    
    site_data_df <- as.data.frame(padded_list)
    
    # R might convert 'XGB+' to 'XGB.' in column names. We'll check for both. It prolly did
    if ("XGB+" %in% colnames(site_data_df)) {
      colnames(site_data_df)[colnames(site_data_df) == "XGB+"] <- "XGB."
    }
    
    best_model_name <- colnames(site_data_df)[4]
    models_to_plot <- unique(c(best_model_name, model_names_standard))
    
    models_present <- models_to_plot[models_to_plot %in% colnames(site_data_df)]
    if (length(models_present) < length(models_to_plot)) {
      cat(paste0("  Warning: For site '", site, "', not all models were found. Skipping missing ones.\n"))
    }
    
    site_data_long <- site_data_df %>%
      select(Obs, all_of(models_present)) %>%
      pivot_longer(
        cols = -Obs,
        names_to = "model_name",
        values_to = "model_value"
      ) %>%
      mutate(site_name = site)
    
    all_sites_data_long_list[[site]] <- site_data_long
  }
  
  variable_df <- bind_rows(all_sites_data_long_list)
  
  # Global Axis Range
  range_df <- variable_df
  
  if (variable %in% c("GPP", "RECO")) {
    range_df <- range_df %>% filter(Obs >= 0, model_value >= 0)
  }
  
  min_val <- min(c(range_df$Obs, range_df$model_value), na.rm = TRUE)
  max_val <- max(c(range_df$Obs, range_df$model_value), na.rm = TRUE)
  
  plot_range <- if (variable %in% c("GPP", "RECO")) {
    c(0, max_val)
  } else {
    c(min_val, max_val)
  }
  
  cat(paste0("  Global axis range for ", variable, " set to: [", round(plot_range[1], 2), ", ", round(plot_range[2], 2), "]\n"))
  
 
 
  plots_by_site <- list()
  other_models_for_ordering <- c("MMM", "MLR", "RF", "XGB", "XGB.")
  
  for (current_site in site_names) {
    
    site_model_runs <- dat[[variable]][[current_site]][['model_runs']]
    if (is.null(site_model_runs) || ncol(as.data.frame(site_model_runs)) < 4) {
      next
    }
    
    site_model_runs_df <- as.data.frame(site_model_runs)
    if ("XGB+" %in% names(site_model_runs_df)) {
      names(site_model_runs_df)[names(site_model_runs_df) == "XGB+"] <- "XGB."
    }
    
    best_model_for_this_site <- names(site_model_runs_df)[4]
    model_order_for_site <- unique(c(best_model_for_this_site, other_models_for_ordering))
    
    site_plot_list <- list() # A temporary list for the current site's plots
    
    for (current_model in model_order_for_site) {
      
      plot_data <- variable_df %>%
        filter(site_name == current_site, model_name == current_model)
      
      display_model_name <- gsub("XGB\\.", "XGB+", current_model)
      
      if (nrow(plot_data) > 0) {
        
        r2_label <- bquote(italic(R^2) ~ ": n/a")
        
        if (sum(complete.cases(plot_data[, c("Obs", "model_value")])) > 1) {
          model_fit <- lm(model_value ~ Obs, data = plot_data)
          r_squared <- summary(model_fit)$r.squared
          r2_label <- bquote(italic(R^2) == .(format(r_squared, digits = 3, nsmall = 3)))
        }
        
        p <- ggplot(plot_data, aes(x = model_value, y = Obs)) +
          geom_point(alpha = 0.4, shape = 16, na.rm = TRUE, size = dot_size) +
          geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = diag_line_size) +
          coord_fixed(ratio = 1, xlim = plot_range, ylim = plot_range) +
          labs(
            title = paste(current_site, display_model_name, sep = " - "),
            tag = r2_label, # Use tag for R2 value
            x = "Model",
            y = "Observation"
          ) +
          theme_bw() +
          theme(
            plot.title = element_text(size = title_size, face = "bold", hjust = 0), 
            axis.text = element_text(size = axis_tick_size),
            plot.tag = element_text(size = R2size, hjust = 1), 
            plot.tag.position = c(1, 0.98), 
            axis.title = element_text(size = axis_title_size) 
          )
      } else {
        # Create a blank plot for missing data
        p <- ggplot() + theme_void() + labs(title = paste(current_site, display_model_name, "\n(No Data)", sep=" - "))
      }
      
      site_plot_list[[length(site_plot_list) + 1]] <- p
    }
    plots_by_site[[current_site]] <- site_plot_list
  }
  
  
  output_filename <- file.path(output_path, paste0(variable, "_scatter_plots.png"))
  
 
  if (transpose) {
   
    # Reorder the plots to be model-by-model across sites
    final_plot_list <- list()
    num_models_to_iterate <- 6 
    num_cols <- 4              # For a 6x4 grid
    
    for (model_idx in 1:num_models_to_iterate) {
      for (current_site in site_names) {
        site_plots <- plots_by_site[[current_site]]
        if (model_idx <= length(site_plots)) {
          final_plot_list[[length(final_plot_list) + 1]] <- site_plots[[model_idx]]
        } else {
         # just in case but this wasn't used in the end, every model was available
          final_plot_list[[length(final_plot_list) + 1]] <- ggplot() + theme_void()
        }
      }
    }
  } else {
  
    final_plot_list <- unlist(plots_by_site, recursive = FALSE)
    num_cols <- 6 # For a 4x6 grid
  }
  
  arranged_plots <- do.call("grid.arrange", c(final_plot_list, ncol = num_cols))
  
  
  ggsave(
    filename = output_filename,
    plot = arranged_plots,
    width = output_width,
    height = output_height,
    dpi = output_dpi
  )
  
  cat(paste0("  Successfully saved plots to: ", output_filename, "\n"))
}
