packages_used_in_this_project <- c("randomForest","xgboost","ggplot2","patchwork","openxlsx","remotes","rBayesianOptimization")

for(pkg in packages_used_in_this_project) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

print("Packages loaded")
source("metrics.R")
source("datamanagement.R")
source("metamodels.R")
source("shp_calc.R")
source("analysis.R")


sites <- c("C1","C2","G3","G4") # C3 does not have enough data
variables <- c("GPP","RECO","NEE")

selected_years <- c("2007","2011")
dataList <- get_data(sites, variables, selected_years)

# Plot GPP
for(variab in variables){
    plot_baseline(dataList, sites, variab)
}


results <- list()
wb <- createWorkbook()
results[["GPP"]] <- perform_analysis_for_variables(dataList, sites, "GPP", wb, filename = "talanyos.xlsx")

# open a new xls file
wb <- createWorkbook()
# add a worksheet

shapeVals <- list()

for(site in sites){
    if(all(is.na(dataList$GPP[[site]]$value$Obs))){
        next()
    }

    # addWorksheet(wb, site)
    print(paste("===============Site:", site,"====================="))
    data <- dataList$GPP[[site]]$value
    meteo_for_data <- dataList$GPP[[site]]$meteo
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
    to_write <- cbind.data.frame(data$Date, data$Obs, mmm, best_model_value, lin_model_pred, rf_model_pred, xgb_model_pred, extended_xgb_model_pred)[!selected_for_training,]
    colnames(to_write) <- c("Date","Obs","MMM",best_model_name,"MLR","RF","XGB","XGB+")
    addWorksheet(wb, site)
    addWorksheet(wb, paste0(site,"_performance"))
    writeData(wb, sheet = site, x = to_write)
    writeData(wb, sheet = paste0(site,"_performance"), x = performance_table)
    # write to excel
    saveWorkbook(wb, file = "first.xlsx", overwrite = TRUE)

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

    lin_shp <- mean_shp_vals_linear(models,linear_model$coefficients[2:length(linear_model$coefficients)])
    rf_shp <- mean_shp_vals_rf(random_forest_model, data_for_linear_model, response_name = "Obs")
    xgb_shp <- mean_shp_vals_xgb(xgb_model, as.matrix(models))
    lin_shp <- c(lin_shp,"Precip" = 0,"Tmean" = 0)
    rf_shp <- c(rf_shp,"Precip" = 0,"Tmean" = 0)
    xgb_shp <- c(xgb_shp,"Precip" = 0,"Tmean" = 0)
    xgbp_shp <- compute_shap_xgb(extended_xgb_model, as.matrix(cbind.data.frame(models, data$Prec, data$Tmean)))
    xgb_shp <- xgb_shp[-length(xgb_shp)]
    colnames(xgb_shp) <- colnames(rf_shp)
    rbind.data.frame("LinearModel" = lin_shp,"RandomForest"=rf_shp,XGBoost=xgb_shp, XGBoostPlus=xgbp_shp)


barplot(lin_shp, main = "SHAP Values for Linear Model", xlab = "SHAP Value", ylab = "Feature")
barplot(shap_vals_rf, main = "SHAP Values for Random Forest Model", xlab = "SHAP Value", ylab = "Feature")

par(mfrow=c(1,2))
barplot(shap_vals_xgb[-length(shap_vals_xgb)], main = "SHAP Values for XGBoost Model", xlab = "SHAP Value", ylab = "Feature")

shap_vals_xgb <- compute_shap_xgb(extended_xgb_model, as.matrix(cbind.data.frame(models, data$Prec, data$Tmean)))
barplot(shap_vals_xgb[-length(shap_vals_xgb)], main = "SHAP Values for XGBoost Model", xlab = "SHAP Value", ylab = "Feature")




install.packages("shiny", dependencies = TRUE)


    
    browser()

    # browser()
    # data <- data[!is.na(data$Prec),]
    # data <- data[!is.na(data$Tmean),]
    # data <- data[!is.na(data$Tmax),]
    # data <- data[!is.na(data$Tmin),]
    # data <- data[!is.na(data$VPD),]
    # data <- data[!is.na(data$RH),]
}

calclimits <- function(x,y){
    return(c(min(min(x,na.rm=TRUE),min(y, na.rm=TRUE)),max(max(x,na.rm=TRUE),max(y,na.rm=TRUE))))
}


limit_df <- sapply(3:(ncol(data)-1), function(i) calclimits(data$Obs,data[i]))
limits <- c(min(limit_df[1,]),max(limit_df[2,]))


plot(data$Obs,data$M01,col=cols[1], xlim=limits, ylim=limits)
abline(0,1)

par(mfrow=c(3,3))
for(i in 3:(ncol(data)-1)){
    x <- data$Obs
    y <- data[,i]
    if(all(is.na(y))){
        next()
    }
    limits <- calclimits(x,y)
    plot(x,y,pch=19, main=colnames(data)[i],xlim=limits,ylim=limits)
    abline(0,1)
}





to_mlr <- cbind.data.frame(data[,c("Obs","M01","M05","M07", "M09", "M14", "M19")], meteo_for_data$Prec, meteo_for_data$Tmean)
to_mlr <- data[,c("Obs","M01","M05","M07", "M09", "M14", "M19")]
not_empty <- !apply(to_mlr,1,function(x) {any(is.na(x))})
to_mlr2 <- to_mlr[not_empty,]

res <- matrix(ncol=5, nrow=3)
colnames(res) <- c("M01", "MMM", "MLR","RF","XGB")
rownames(res) <- c("RMSE","RRMSE","R")
mlr <- lm(Obs ~ ., data= to_mlr)
res[1,3] <- rmse(predict(mlr,data),to_mlr$Obs)
res[2,3] <- rrmse(predict(mlr,data),to_mlr$Obs)
res[3,3] <- rval(predict(mlr,data),to_mlr$Obs)
res[1,1] <- rmse(to_mlr$M01,to_mlr$Obs)
res[2,1] <- rrmse(to_mlr$M01,to_mlr$Obs)
res[3,1] <- rval(to_mlr$M01,to_mlr$Obs)

rf <- randomForest(Obs ~ ., data= to_mlr2)
res[1,4] <- rmse(predict(rf,to_mlr2),to_mlr2$Obs)
res[2,4] <- rrmse(predict(rf,to_mlr2),to_mlr2$Obs)
res[3,4] <- rval(predict(rf,to_mlr2),to_mlr2$Obs)
xgb_m <- xgboost(data= as.matrix(to_mlr2[,-1]), label=as.vector(to_mlr2[,1]),objective = "reg:squarederror",nrounds=1000)
res[1,5] <- rmse(predict(xgb_m,as.matrix(to_mlr2[,-1])),to_mlr2$Obs)
res[2,5] <- rrmse(predict(xgb_m,as.matrix(to_mlr2[,-1])),to_mlr2$Obs)
res[3,5] <- rval(predict(xgb_m,as.matrix(to_mlr2[,-1])),to_mlr2$Obs)

to_mlr$mlr <- NA
to_mlr$mlr[not_empty] <- predict(mlr)
to_mlr$rf <- NA
to_mlr$rf[not_empty] <- predict(rf)
to_mlr$xgb <- NA
to_mlr$xgb[not_empty] <- predict(xgb_m,as.matrix(to_mlr2[,-1]))
to_mlr$mmm <- apply(to_mlr[,2:7],1,median, na.rm=TRUE)
res[1,2] <- rmse(to_mlr$mmm,to_mlr$Obs)
res[2,2] <- rrmse(to_mlr$mmm,to_mlr$Obs)
res[3,2] <- rval(to_mlr$mmm,to_mlr$Obs)
round(res,4)
plot(to_mlr$gxb,predict(xgb_m,as.matrix(to_mlr2[,-1])))
feature_names <- colnames(to_mlr2[,-1])
feature_names[7] <- "Prec"
feature_names[8] <- "Tmean"
importance_matrix <- xgb.importance(feature_names = feature_names, model = xgb_m)
xgb.plot.importance(importance_matrix)

par(mfrow=c(3,4))
for(i in 2:(ncol(to_mlr))){
    x <- to_mlr$Obs
    y <- to_mlr[,i]
    if(all(is.na(y))){
        next()
    }
    limits <- calclimits(x,y)
    plot(x,y,pch=19, main=colnames(to_mlr)[i],xlim=c(0,0.03),ylim=c(0,0.03))
    mtext(cor(x[!(is.na(x) | is.na(y))],y[!(is.na(x) | is.na(y))]),side=3)
    abline(0,1)
}

library(ggplot2)
library(patchwork)

plots <- list()

for (i in 2:(ncol(to_mlr))) {
    x <- to_mlr$Obs
    y <- to_mlr[, i]
    if (all(is.na(y))) {
        next()
    }
    df <- data.frame(x = x, y = y)
    corr <- round(cor(x[!(is.na(x) | is.na(y))], y[!(is.na(x) | is.na(y))]), 2)
    p <- ggplot(df, aes(x = x, y = y)) +
        geom_point(alpha = 0.6, size = 1.2) +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
        labs(title = colnames(to_mlr)[i], subtitle = paste("Correlation:", corr),
             x = "Observed", y = "Predicted") +Stage5_G4_Yield_daily.csv
        theme_minimal(base_size = 10) +
        theme(plot.margin = margin(5, 5, 5, 5),
              plot.title = element_text(size = 12, face = "bold"),
              plot.subtitle = element_text(size = 10),
              axis.text = element_text(size = 9),
              axis.title = element_text(size = 10)) +
        coord_fixed(ratio = 1, xlim = c(0, 0.03), ylim = c(0, 0.03))
    plots[[i - 1]] <- p
}

# Adjust the number of columns dynamically
ncol_dynamic <- ceiling(sqrt(length(plots))) # Optimal columns based on total plots

# Arrange plots tightly without empty rows
final_plot <- wrap_plots(plots, ncol = ncol_dynamic) +
  plot_annotation(title = "Model Performance Comparisons",
                  subtitle = "Observed vs Predicted with Correlation Coefficients",
                  theme = theme(plot.title = element_text(size = 14, face = "bold"),
                                plot.subtitle = element_text(size = 12)))
final_plot

# Save without empty rows
ggsave("final_plot_compact.png", final_plot, dpi = 300, width = 12, height = 8)


str(to_mlr)
to_mlr$Date <- data$Date
plot(to_mlr$Date,to_mlr$M01,type="l",col="blue", xlab="Date", ylab="GPP")
lines(to_mlr$Date,to_mlr$mmm,col="green")
lines(to_mlr$Date,to_mlr$xgb,col="red")
points(to_mlr$Date,to_mlr$Obs,col="black", pch=19, cex=0.5)
legend("topright", legend=c("Observed","Best individual model","Multi-Model Median","XGBoost"), col=c("black","blue","green","red"), lty=1:1)


library(ggplot2)
library(patchwork)
library(grid)    # for unit()

plots <- list()

for (i in 2:ncol(to_mlr)) {
  x <- to_mlr$Obs
  y <- to_mlr[[i]]
  
  # skip any all-NA column
  if (all(is.na(y))) next
  
  df <- data.frame(x = x, y = y)
  corr <- round(cor(x, y, use = "complete.obs"), 2)
  
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_point(alpha = 0.6, size = 1.2) +
    geom_abline(intercept = 0, slope = 1,
                color = "red", linetype = "dashed") +
    labs(
      title    = names(to_mlr)[i],
      subtitle = paste("Correlation:", corr),
      x        = "Observed",
      y        = "Predicted"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.margin    = unit(c(5, 5, 5, 5), "pt"),
      plot.title     = element_text(size = 12, face = "bold"),
      plot.subtitle  = element_text(size = 10),
      axis.text      = element_text(size = 9),
      axis.title     = element_text(size = 10)
    ) +
    coord_fixed(
      ratio = 1,
      xlim  = c(0, 0.03),
      ylim  = c(0, 0.03)
    )
  
  plots[[i - 1]] <- p
}

# To stitch them all together:
plotikak <- wrap_plots(plots)

ggsave(
  filename = "combined_plot.svg", 
  plot     = plotikak, 
  device   = "svg", 
  width    = 10, 
  height   =  7,
  units    = "in"
)

