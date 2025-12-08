library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

################################################################################
################################## OPTIONS #####################################
################################################################################ 

# CHANGE THIS TO THE RDS DATA
dat <-readRDS(file = "C:/Users/PadFl/Desktop/DokThori/metamodel/data/results_1000.rds")

output_width <- 15
output_height <- 8 
output_dpi <- 600   

# CHANGE THIS TO YOUR DESIRED LOCATION
output_path <- "C:/Users/PadFl/Desktop/DokThori/metamodel/plots/" 

# How many years to plot together for G3 and G4 sites when date list is NULL.
# For example, 3 means years will be plotted in groups of 3 (2003-2005, 2006-2008, etc.)
# okay so it doesn't quite work (list being NULL) but we'll give the date ranges anyway so it doesn't matter i guess
year_separation_forGrass <- 3 # so this option doesn't work but we prolly don't need it



# adjust the colors of each result
plot_colors <- c(
  "Obs"   = "#e41a1c",
  "MMM"   = "lightgreen", 
  "M09"   = "black",      #the name "M09" will be dynamically replaced
  "MLR"   = "#8EE3E6", 
  "RF"    = "orange", 
  "XGB"   = "#F4AFFC", 
  "XGB+"  = "royalblue"  
)
# adjust the linetype of each result except "Obs"
plot_linetypes <- c(
  "Obs"  = "blank", # Obs will be a dot, don't edit this
  "MMM"   = "solid",
  "M09"   = "solid",
  "MLR"   = "twodash",
  "RF"    = "dashed",
  "XGB"   = "longdash",
  "XGB+"  = "solid"
)
# adjust the width of the lines 
plot_linewidths <- c(
  "Obs"   = 3, #in this case dot size
  "MMM"   = 0.8,
  "M09"   = 0.7,
  "MLR"   = 1.2,
  "RF"    = 1.2,
  "XGB"   = 1,
  "XGB+"  = 1
)

# Define legend-specific sizes
legend_line_width <- c(
  "Obs"   = 5,   # Larger point size in legend
  "MMM"   = 2,   # Larger line size in legend
  "M09" = 2,  # Dynamic model name
  "MLR"   = 2,
  "RF"    = 2,
  "XGB"   = 2,
  "XGB+"  = 2
)

# This determines how transparent the measurement points will be. It's here for making the plots potentially more visible
# it ranges from 0 to 1, 0 being totally transparent, 1 is fully visible
plot_transparencies <- c(
  "Obs"   = 1,   # Not used for lines, edit this below at the 'obs_transparency'
  "MMM"   = 1,
  "M09"   = 1, # This will be dynamically renamed
  "MLR"   = 1,
  "RF"    = 1,
  "XGB"   = 1,
  "XGB+"  = 0.8
)
obs_transparency <- 0.8 # transparency for the observations

# CUStom dates we would like to plot.
# So we can give custom date ranges for the sites we want our plot to show
# multiple date ranges can be given that's why we use a list
C1_dates <- list(
  "wheat_2007" = c("2007-05-19", "2007-09-04"), # you can give actual names as well like "winter_wheat" = c(...) so the file will be called that
  "wheat_2011" = c("2011-05-10", "2011-08-29")
)

C2_dates <- list(
  "maize_wheat_2008_2009" = c("2008-04-27","2009-07-31"),
  "maize_wheat_2011_2012" = c("2011-04-20","2012-08-03")
)

G3_dates <- list(
  "G3_2003_2004" = c("2003-01-01","2004-12-31"),
  "G3_2005_2006" = c("2005-01-01","2006-12-31"),
  "G3_2007_2008" = c("2007-01-01","2008-12-31"),
  "G3_2009_2011" = c("2009-01-01","2011-12-31")
)

G4_dates <- list(
  "G4_2002_2003" = c("2002-01-01","2003-12-31"),
  "G4_2004_2005" = c("2004-01-01","2005-12-31"),
  "G4_2006_2007" = c("2006-01-01","2007-12-31"),
  "G4_2008_2010" = c("2008-01-01","2010-12-31")
)
#G4_dates <- list(NULL)

y_axis_title_size <- 20 # title size of the y axis
y_axis_tick_size  <- 20 # tick size of the y axis
y_axis_title_margin <- 10 # y axis title distance from the axis
y_axis_tick_frequency <- 2 # frequency of y axis ticks 
global_y_axis <- FALSE # if TRUE the y axis will be globally the same between sites for each variable (GPP,RECO,NEE)

x_axis_tick_size  <- 20
legend_text_size <- 20

# Frequency of x-axis ticks for C1 and C2 sites (in months)
# For example, 1 means every month, 2 means every second month
x_axis_tick_frequency_C <- 1

# Frequency of x-axis ticks for G3 and G4 sites WHEN using specific date ranges (in months). And since the other version doesn't work we'll use specific dates 
# For example, 1 means every month, 2 means every second month
x_axis_tick_frequency_G <- 3

isEPS <- FALSE # If TRUE, the output will be in EPS format, otherwise PNG






###############################################################################
############################### Plotting ######################################
###############################################################################

#  top-level variable names ("GPP", "NEE", "RECO")
variables <- names(dat)
# Loop over each variable
for (current_variable in variables) {
  
  # Pre-calculate global Y-axis ranges if enabled
  global_min <- NULL
  global_max <- NULL
  if (global_y_axis) {
    cat(paste0("Calculating global Y-axis range for: ", current_variable, "\n"))
    # Combine all values for the current variable across all sites to find the true min/max
    all_values <- unlist(lapply(dat[[current_variable]], function(site_data) {
      # Select all columns except the 'Date' column and unlist them
      if (!is.null(site_data$model_runs)) {
        select(site_data$model_runs, -Date)
      }
    }))
    global_min <- min(all_values, na.rm = TRUE)
    global_max <- max(all_values, na.rm = TRUE)
  }
  
  # Get the site names for the current variable ("C1", "C2"...)
  sites <- names(dat[[current_variable]])
  
  # Loop over each site
  for (current_site in sites) {
    
    cat(paste0("--- Processing Variable: ", current_variable, ", Site: ", current_site, " ---\n"))
    
    model_runs_df <- dat[[current_variable]][[current_site]]$model_runs
    
    if (is.null(model_runs_df) || nrow(model_runs_df) == 0) {
      cat(paste0("Skipping ", current_variable, " at ", current_site, " due to no data.\n\n"))
      next
    }
    
    #Dynamically set model names for aesthetics 
    model_names <- names(model_runs_df)[-1] # Exclude 'Date' column
    reference_model_name <- model_names[3] # Get the 3rd model name ("M01", "M10"...)
    
    # Create dynamic copies of aesthetic mappings
    dynamic_colors <- plot_colors
    names(dynamic_colors)[names(dynamic_colors) == "M09"] <- reference_model_name
    
    dynamic_linetypes <- plot_linetypes
    names(dynamic_linetypes)[names(dynamic_linetypes) == "M09"] <- reference_model_name
    
    dynamic_linewidths <- plot_linewidths
    names(dynamic_linewidths)[names(dynamic_linewidths) == "M09"] <- reference_model_name
    
    df_processed <- model_runs_df %>%
      mutate(Date = as.Date(Date), Year = year(Date))
    
    df_long <- df_processed %>%
      pivot_longer(cols = -c(Date, Year), names_to = "Model", values_to = "Value") %>%
      mutate(Model = factor(Model, levels = c("Obs", "MMM", reference_model_name, "MLR", "RF", "XGB", "XGB+")))
    
    # logic switch for different site types
    if (current_site %in% c("G3", "G4")) {
      
      # Select the appropriate date list based on the site
      date_list <- if (current_site == "G3") G3_dates else G4_dates
      
      if (is.null(date_list)) {
        # Use original year-chunking logic if date list is NULL
        unique_years <- unique(df_long$Year)
        num_years <- length(unique_years)
        
        # Input Validation 
        # Stop if the user-defined separation value is invalid.
        if (year_separation_forGrass > num_years || year_separation_forGrass < 1) {
          stop(paste0("For site ", current_site, ", 'year_separation_forGrass' (", year_separation_forGrass, ") must be between 1 and the number of available years (", num_years, ")."))
        }
        
        # Group years into chunks 
        year_chunks <- split(unique_years, ceiling(seq_along(unique_years) / year_separation_forGrass))
        
        # Loop through each chunk of years and create a plot
        for (year_chunk in year_chunks) {
          first_year <- min(year_chunk)
          last_year <- max(year_chunk)
          
          cat(paste0("  Generating multi-year plot for years: ", paste(year_chunk, collapse=", "), "\n"))
          
          df_chunk <- df_long %>% filter(Year %in% year_chunk)
          
          # Set y-axis limits for the chunk
          y_min <- if (global_y_axis) global_min else min(df_chunk$Value, na.rm = TRUE)
          y_max <- if (global_y_axis) global_max else max(df_chunk$Value, na.rm = TRUE)
          
          # Dynamic shapes for legend
          dynamic_shapes <- c(16, rep(NA, length(levels(df_chunk$Model)) - 1))
          names(dynamic_shapes) <- levels(df_chunk$Model)
          dynamic_transparencies <- plot_transparencies
          names(dynamic_transparencies)[names(dynamic_transparencies) == "M09"] <- reference_model_name
          
          # Create the plot with a continuous x-axis spanning the years in the chunk
          gpp_plot <- ggplot(df_chunk, aes(x = Date, y = Value, color = Model, linetype = Model, size = Model, group = Model, alpha = Model)) +
            geom_line(data = . %>% filter(Model != "Obs")) +
            geom_point(data = . %>% filter(Model == "Obs"),alpha = obs_transparency) +
            scale_color_manual(values = dynamic_colors, breaks = names(dynamic_colors)) +
            scale_linetype_manual(values = dynamic_linetypes, breaks = names(dynamic_linetypes)) +
            scale_size_manual(values = dynamic_linewidths, breaks = names(dynamic_linewidths)) +
            scale_alpha_manual(values = dynamic_transparencies, breaks = names(dynamic_transparencies)) +
            
            scale_x_date(date_labels = "%Y-%d-%m", date_breaks = "3 months",expand = expansion(mult = 0.01)) + 
            scale_y_continuous(
              breaks = seq(
                from = floor(y_min / y_axis_tick_frequency) * y_axis_tick_frequency,
                to = ceiling(y_max / y_axis_tick_frequency) * y_axis_tick_frequency,
                by = y_axis_tick_frequency
              )
            ) +
            coord_cartesian(ylim = c(y_min, y_max)) +
            guides(
              color = guide_legend(title = "", override.aes = list(shape = dynamic_shapes, linetype = dynamic_linetypes, size = legend_line_width,linewidth = legend_line_width )),
              linetype = "none", size = "none", alpha = "none"
            ) +
            labs(x = "", y = paste0(current_variable, " [gC/m\u00B2/day]"), color = NULL, linetype = NULL, size = NULL) +
            theme_bw() +
            theme(
              legend.position = "bottom",
              legend.title.align = 0.5,
              legend.key.width = unit(3, "cm"),
              legend.margin = margin(t = -10, unit = "pt"),
              legend.text = element_text(size = legend_text_size),
              plot.margin = margin(t=5,r=40,b=5,l=5,unit = "pt"),
              axis.title.y = element_text(size = y_axis_title_size, margin = margin(t = 0, r = y_axis_title_margin, b = 0, l = 0)),
              axis.text.y = element_text(size = y_axis_tick_size),
              axis.text.x = element_text(size = x_axis_tick_size, angle = 0, vjust = 0.5, hjust = 0.5)
            )
          
          # Define output filename for the year chunk
          year_range_str <- if (first_year == last_year) as.character(first_year) else paste0(first_year, "-", last_year)
          output_filename <- paste0(output_path, current_variable, "_", current_site, "_Plot_", year_range_str, ".png")
          
          # Save the plot with the original dimensions
          ggsave(filename = output_filename, plot = gpp_plot, width = output_width, height = output_height, dpi = output_dpi)
          
          cat(paste0("  Saved multi-year plot to: ", output_filename, "\n"))
        }
        
      } else {
        # Use specific date ranges if date list is provided
        for (date_name in names(date_list)) {
          
          cat(paste0("  Generating plot for date range: ", date_name, "\n"))
          
          # Get start and end dates for the period
          start_date <- as.Date(date_list[[date_name]][1])
          end_date <- as.Date(date_list[[date_name]][2])
          
          # Filter data for the specified date range
          df_date <- df_long %>% filter(Date >= start_date & Date <= end_date)
          
          if (nrow(df_date) == 0) {
            cat(paste0("  No data available for ", date_name, " at ", current_site, ". Skipping.\n"))
            next
          }
          
          # Set y-axis limits for the date range
          y_min <- if (global_y_axis) global_min else min(df_date$Value, na.rm = TRUE)
          y_max <- if (global_y_axis) global_max else max(df_date$Value, na.rm = TRUE)
          
          # Dynamic shapes for legend
          dynamic_shapes <- c(16, rep(NA, length(levels(df_date$Model)) - 1))
          names(dynamic_shapes) <- levels(df_date$Model)
          
          dynamic_transparencies <- plot_transparencies
          names(dynamic_transparencies)[names(dynamic_transparencies) == "M09"] <- reference_model_name
          
          
          # Create the plot
          gpp_plot <- ggplot(df_date, aes(x = Date, y = Value, color = Model, linetype = Model, size = Model, group = Model, alpha = Model)) +
            geom_line(data = . %>% filter(Model != "Obs")) +
            geom_point(data = . %>% filter(Model == "Obs"),alpha = obs_transparency) +
            scale_color_manual(values = dynamic_colors, breaks = names(dynamic_colors)) +
            scale_linetype_manual(values = dynamic_linetypes, breaks = names(dynamic_linetypes)) +
            scale_size_manual(values = dynamic_linewidths, breaks = names(dynamic_linewidths)) +
            scale_alpha_manual(values = dynamic_transparencies, breaks = names(dynamic_transparencies)) +
            scale_x_date(date_labels = "%d-%m-%Y", date_breaks = paste0(x_axis_tick_frequency_G, " months"),expand = expansion(mult = 0.01)) +
            scale_y_continuous(
              breaks = seq(
                from = floor(y_min / y_axis_tick_frequency) * y_axis_tick_frequency,
                to = ceiling(y_max / y_axis_tick_frequency) * y_axis_tick_frequency,
                by = y_axis_tick_frequency
              )
            ) +
            coord_cartesian(ylim = c(y_min, y_max)) +
            guides(
              color = guide_legend(title = "", override.aes = list(shape = dynamic_shapes, linetype = dynamic_linetypes, size = legend_line_width,linewidth = legend_line_width)),
              linetype = "none", size = "none",alpha = "none"
            ) +
            labs(x = "", y=bquote(.(current_variable) ~ "[gC"*m^{-2}*day^{-1}*"]"), color = NULL, linetype = NULL, size = NULL) +
            theme_bw() +
            theme(
              legend.position = "bottom",
              legend.title.align = 0.5,
              legend.key.width = unit(3, "cm"),
              legend.margin = margin(t = -10, unit = "pt"),
              legend.text = element_text(size = legend_text_size),
              #legend.background = element_rect(colour = "black", linewidth = 0.5),
              plot.margin = margin(t=5,r=40,b=5,l=5,unit = "pt"),
              axis.title.y = element_text(size = y_axis_title_size, margin = margin(t = 0, r = y_axis_title_margin, b = 0, l = 0)),
              axis.text.y = element_text(size = y_axis_tick_size),
              axis.text.x = element_text(size = x_axis_tick_size, angle = 0, vjust = 0.5, hjust = 0.5)
            )
          
          if (isEPS){
            output_filename <- paste0(output_path, current_variable, "_", current_site, "_Plot_", date_name, ".eps")
            
            # Save the plot
            ggsave(filename = output_filename, plot = gpp_plot, width = output_width, height = output_height, device = "eps")
            cat(paste0("  Saved plot to: ", output_filename, "\n"))
          }
          else{
          # Define output filename for the date range
          output_filename <- paste0(output_path, current_variable, "_", current_site, "_Plot_", date_name, ".png")
          
          # Save the plot
          ggsave(filename = output_filename, plot = gpp_plot, width = output_width, height = output_height, dpi = output_dpi)
          
          cat(paste0("  Saved plot to: ", output_filename, "\n"))
          }
        }
      }
      
    } else if (current_site %in% c("C1", "C2")) {
      
      # Select the appropriate date list based on the site
      date_list <- if (current_site == "C1") C1_dates else C2_dates
      
      # Loop over each crop period
      for (crop_name in names(date_list)) {
        
        cat(paste0("  Generating plot for crop: ", crop_name, "\n"))
        
        # Get start and end dates for the crop
        start_date <- as.Date(date_list[[crop_name]][1])
        end_date <- as.Date(date_list[[crop_name]][2])
        
        # Filter data for the specified date range
        df_crop <- df_long %>% filter(Date >= start_date & Date <= end_date)
        
        if (nrow(df_crop) == 0) {
          cat(paste0("  No data available for ", crop_name, " at ", current_site, ". Skipping.\n"))
          next
        }
        
        # Set y-axis limits for the crop period
        y_min <- if (global_y_axis) global_min else min(df_crop$Value, na.rm = TRUE)
        y_max <- if (global_y_axis) global_max else max(df_crop$Value, na.rm = TRUE)
        
        # Dynamic shapes for legend
        dynamic_shapes <- c(16, rep(NA, length(levels(df_crop$Model)) - 1))
        names(dynamic_shapes) <- levels(df_crop$Model)
        dynamic_transparencies <- plot_transparencies
        names(dynamic_transparencies)[names(dynamic_transparencies) == "M09"] <- reference_model_name
        
        # Create the plot
        gpp_plot <- ggplot(df_crop, aes(x = Date, y = Value, color = Model, linetype = Model, size = Model, group = Model, alpha = Model)) +
          geom_line(data = . %>% filter(Model != "Obs")) +
          geom_point(data = . %>% filter(Model == "Obs"),alpha = obs_transparency) +
          scale_color_manual(values = dynamic_colors, breaks = names(dynamic_colors)) +
          scale_linetype_manual(values = dynamic_linetypes, breaks = names(dynamic_linetypes)) +
          scale_size_manual(values = dynamic_linewidths, breaks = names(dynamic_linewidths)) +
          scale_alpha_manual(values = dynamic_transparencies, breaks = names(dynamic_transparencies)) +
          scale_x_date(date_labels = "%d-%m-%Y", date_breaks = paste0(x_axis_tick_frequency_C, " months"),expand = expansion(mult = 0.01)) +
          scale_y_continuous(
            breaks = seq(
              from = floor(y_min / y_axis_tick_frequency) * y_axis_tick_frequency,
              to = ceiling(y_max / y_axis_tick_frequency) * y_axis_tick_frequency,
              by = y_axis_tick_frequency
            )
          ) +
          coord_cartesian(ylim = c(y_min, y_max)) +
          guides(
            color = guide_legend(title = "", override.aes = list(shape = dynamic_shapes, linetype = dynamic_linetypes, size = legend_line_width,linewidth = legend_line_width)),
            linetype = "none", size = "none",alpha = "none"
          ) +
          labs(x = "", y = bquote(.(current_variable) ~ "[gC"*m^{-2}*day^{-1}*"]"), color = NULL, linetype = NULL, size = NULL) +
          theme_bw() +
          theme(
            legend.position = "bottom",
            legend.title.align = 0.5,
            legend.key.width = unit(3, "cm"),
            legend.margin = margin(t = -10, unit = "pt"),
            legend.text = element_text(size = legend_text_size),
            
            #legend.background = element_rect(colour = "black", linewidth = 0.5),
            plot.margin = margin(t=5,r=40,b=5,l=5,unit = "pt"),
            axis.title.y = element_text(size = y_axis_title_size, margin = margin(t = 0, r = y_axis_title_margin, b = 0, l = 0)),
            axis.text.y = element_text(size = y_axis_tick_size),
            axis.text.x = element_text(size = x_axis_tick_size, angle = 0, vjust = 0.5, hjust = 0.5)
          )
        
        if (isEPS) {
          # Define output filename for the crop period in EPS format
          output_filename <- paste0(output_path, current_variable, "_", current_site, "_Plot_", crop_name, ".eps")
          
          # Save the plot in EPS format
          ggsave(filename = output_filename, plot = gpp_plot, width = output_width, height = output_height, device = "eps")
          
          cat(paste0("  Saved plot to: ", output_filename, "\n"))
        } else {
        # Define output filename for the crop period
        output_filename <- paste0(output_path, current_variable, "_", current_site, "_Plot_", crop_name, ".png")
        
        # Save the plot
        ggsave(filename = output_filename, plot = gpp_plot, width = output_width, height = output_height, dpi = output_dpi)
        
        cat(paste0("  Saved plot to: ", output_filename, "\n"))
        }
      
      }
      cat("\n")
    }
  }
  cat(paste0("Finished processing variable: ", current_variable, "\n\n"))
}