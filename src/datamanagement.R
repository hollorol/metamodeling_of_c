# Function to load data for multiple sites and variables
get_data <- function(sites, variables, selected_years, stage = "Stage3", data_dir = "../data"){
    dataList <- list()
    
    for(j in variables){
        dataList[[j]] <- list()
        for(i in sites){
            dataList[[j]][[i]] <- list()
            
            # Load variable data with proper file path
            data_file <- file.path(data_dir, paste0(stage, "_", i, "_", j, "_daily.csv"))
            if(!file.exists(data_file)){
                warning(paste("File not found:", data_file))
                next
            }
            dataList[[j]][[i]]$value <- read.csv(data_file, sep=";")
            
            # Create date sequence based on years in the data
            data_dates <- do.call("c", lapply(unique(as.numeric(dataList[[j]][[i]]$value$Year)), function(y){
                seq(from = as.Date(sprintf("%d-01-01", y)), length.out = 365, by = "days")
            }))
            dataList[[j]][[i]]$value$Date <- data_dates
            
            # Filter selected years for C1 site
            if(i == "C1" && !is.null(selected_years)){
                selected_year <- dataList[[j]][[i]]$value$Year %in% selected_years
                dataList[[j]][[i]]$value <- dataList[[j]][[i]]$value[selected_year, ]
                data_dates <- dataList[[j]][[i]]$value$Date
            }
            
            # Load meteorological data with proper file path
            meteo_file <- file.path(data_dir, paste0(i, "_meteo.csv"))
            if(!file.exists(meteo_file)){
                warning(paste("Meteo file not found:", meteo_file))
                next
            }
            dataList[[j]][[i]]$meteo <- read.csv(meteo_file, sep = ",")
            dataList[[j]][[i]]$meteo$Date <- as.Date(dataList[[j]][[i]]$meteo[, 1], format = "%Y.%m.%d")
            dataList[[j]][[i]]$meteo <- dataList[[j]][[i]]$meteo[match(data_dates, dataList[[j]][[i]]$meteo$Date), ]
        }
    }
    return(dataList)
}


# Function to plot baseline observations for a variable across sites
plot_baseline <- function(dataList, sites, varName, output_file = NULL){
    if(!is.null(output_file)){
        pdf(output_file, width = 12, height = 8)
    }
    
    for(site in sites){
        if(is.null(dataList[[varName]][[site]]) || 
           all(is.na(dataList[[varName]][[site]]$value$Obs))){
            next
        }
        plot(dataList[[varName]][[site]]$value$Date, 
             dataList[[varName]][[site]]$value$Obs, 
             type = "l", 
             col = "black", 
             xlab = "Date", 
             ylab = varName, 
             main = paste(site, "-", varName))
    }
    
    if(!is.null(output_file)){
        dev.off()
    }
}



