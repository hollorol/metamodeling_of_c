# SHAP value calculation functions for different model types

# Calculate mean absolute SHAP values for linear models
mean_shp_vals_linear <- function(xs, beta){
    xdist <- t(apply(xs, 2, function(x){
        x <- x - mean(x)
        return(x)
    }))
    apply(abs(xdist), 1, mean) * abs(beta)
}

# Calculate mean absolute SHAP values for random forest models
mean_shp_vals_rf <- function(model, data, response_name, nsim = 10) {
    if (!requireNamespace("fastshap", quietly = TRUE)) {
        stop("Please install the 'fastshap' package with install.packages('fastshap')")
    }
    
    # Separate features (X)
    X <- data[, setdiff(names(data), response_name), drop = FALSE]
    
    # Define prediction wrapper
    pred_fun <- function(object, newdata) {
        as.numeric(predict(object, newdata = newdata))
    }
    
    # Compute SHAP values
    shap_vals <- fastshap::explain(
        object = model,
        X = X,
        pred_wrapper = pred_fun,
        nsim = nsim
    )
    
    feature_importance <- colMeans(abs(shap_vals))
    return(feature_importance)
}

# Calculate mean absolute SHAP values for XGBoost models
mean_shp_vals_xgb <- function(xgb_model, data_matrix) {
    # data_matrix should be a numeric matrix or dgCMatrix (not data.frame)
    shap_values <- predict(xgb_model, data_matrix, predcontrib = TRUE)
    shap_df <- as.data.frame(shap_values)
    feature_importance <- colMeans(abs(shap_df))
    return(feature_importance)
}

# Compute SHAP values for XGBoost models (alias for mean_shp_vals_xgb)
compute_shap_xgb <- function(xgb_model, data_matrix) {
    mean_shp_vals_xgb(xgb_model, data_matrix)
}


