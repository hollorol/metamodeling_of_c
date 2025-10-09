

dataList <- vector(mode="list")
sites <- c("C1","C2","G3","G4") # C3 does not have enough data
variables <- c("GPP","RECO")

selected_years <- c("2007","2011")

get_data  <- function(sites, variables, selected_years){
    for(j in variables){
        for(i in sites){
            dataList[[j]][[i]]$value <- read.csv(paste0("Stage3_",i,"_",j,"_daily.csv"),sep=";")
            data_dates <- do.call("c", lapply(unique(as.numeric(dataList[[j]][[i]]$value$Year)),function(y){
                                                seq(from = as.Date(sprintf("%d-01-01",y)), length.out=365, by="days") }))
            dataList[[j]][[i]]$value$Date <- data_dates
            if(i == "C1"){
               selected_year <- dataList[[j]][[i]]$value$Year %in% selected_years
               dataList[[j]][[i]]$value <- dataList[[j]][[i]]$value[selected_year,]
            }
            dataList[[j]][[i]]$meteo <- read.csv(paste0(i,"_meteo.csv"),sep=",")
            dataList[[j]][[i]]$meteo$Date <- as.Date(dataList[[j]][[i]]$meteo[,1], format="%Y.%m.%d")
            dataList[[j]][[i]]$meteo<- dataList[[j]][[i]]$meteo[match(data_dates,dataList[[j]][[i]]$meteo$Date),]
        }
    }
    return(dataList)
}

plot_baseline <- function(dataList, sites, varName){
    pdf("Stage3_GPP_daily_2.pdf", width=12, height=8)
    for(site in sites){

        if(all(is.na(dataList[[varName]][[site]]$value$Obs))){
            next()
        }
        plot(dataList[[varName]][[site]]$value$Date, dataList[[varName]][[site]]$value$Obs, type="l", col="black", xlab="Date", ylab=varName, main=site)
    }
    dev.off()
}


