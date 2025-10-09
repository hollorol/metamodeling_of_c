library(ggplot2)
library(dplyr)
library(scales)


rds_file_path <- "C:/Users/PadFl/Desktop/DokThori/metamodel/data/results_1000.rds"

output_directory <- "C:/Users/PadFl/Desktop/DokThori/metamodel/plots/shap/"

use_global_x_axis <- TRUE

x_text_size <- 20
y_text_size <- 20
x_title_size <- 20
x_axis_title_margin <- 10

x_axis_n_breaks <- 6 # well this doesn't quite work as intended, whatever, scales is always too stubborn but I like it that way

width <- 10
height <- 8
dpi <- 300


###############################################################################
#################################### PLOTTING #################################
###############################################################################


dat <- readRDS(file = rds_file_path)

# Get the names of the top-level variables ("GPP", "RECO", "NEE").
variables <- names(dat)

cat("Starting plot generation...\n")

# Loop through each variable in the dataset.
for (variable in variables) {
  
  # Global x Axis Calculation
  # This block calculates the min and max SHAP values across all sites for the current variable
  global_x_lims <- NULL
  if (use_global_x_axis) {
    # Collect all SHAP values for the current variable into a single vector
    all_values_for_var <- unlist(lapply(dat[[variable]], function(site_data) {
      shap_matrix <- site_data[["shap_matrix"]]
      if (!is.null(shap_matrix) && nrow(shap_matrix) > 0 && ncol(shap_matrix) > 0) {
        return(as.numeric(shap_matrix[nrow(shap_matrix), ]) * 1000)
      }
      return(NULL)
    }))
    
    # If we found any values, calculate the min and max to set the global axis limits
    if (length(all_values_for_var) > 0) {
      global_x_lims <- range(all_values_for_var, na.rm = TRUE)
    }
  }
  
  
  sites <- names(dat[[variable]])
  
  # Loop through each site to create a plot
  for (site in sites) {
    
    cat(paste("Processing:", variable, "-", site, "\n"))
    
    shap_matrix <- dat[[variable]][[site]][["shap_matrix"]]
    
    if (!is.null(shap_matrix) && nrow(shap_matrix) > 0 && ncol(shap_matrix) > 0) {
      
      plot_data <- data.frame(
        model = colnames(shap_matrix),
        value = as.numeric(shap_matrix[nrow(shap_matrix), ]) * 1000
      )
      
      
      p <- ggplot(plot_data, aes(y = reorder(model, value), x = value)) +
        geom_col(fill = "#0072B2", color = "black", alpha = 0.8) +
        
        # Control the x-axis ticks.
        scale_x_continuous(breaks = pretty_breaks(n = x_axis_n_breaks)) +
        
        labs(
          title = "",
          y = "",
          x = "SHAP Value"
        ) +
        
        
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          #plot.subtitle = element_text(hjust = 0.5, size = 12),
          plot.margin = margin(t=5,r=20,b=5,l=5,unit = "pt"),
          axis.text.y = element_text(size = y_text_size),
          axis.text.x = element_text(size = x_text_size),
          axis.title.x = element_text(size = x_title_size, face = "bold", margin = margin(t = x_axis_title_margin, r = 0, b = 0, l = 0)),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)
        )
      
      # If global axis is enabled, apply the calculated limits.
      # Using coord_cartesian zooms the view without removing any data
      if (!is.null(global_x_lims)) {
        p <- p + coord_cartesian(xlim = global_x_lims)
      }
      
      filename <- file.path(output_directory, paste0(variable, "_", site, "_shap_plot.png"))
      ggsave(filename, plot = p, width = width, height = height, dpi = dpi)
      
      cat(paste("  -> Saved plot to:", filename, "\n"))
      
    } else {
      cat(paste("  -> SKIPPING: 'shap_matrix' not found or is empty for", variable, site, "\n"))
    }
  }
}