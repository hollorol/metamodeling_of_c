# Main visualization and execution script for carbon metamodeling project
# This script loads required packages, sources modules, and runs the analysis

# ============================================================================
# 0. THREADING CONFIGURATION
# ============================================================================
# CRITICAL: Disable all threading to prevent fork() deadlocks
# When R uses threaded BLAS/LAPACK and you fork with mclapply, it can deadlock
Sys.setenv(OMP_NUM_THREADS = "1")
Sys.setenv(MKL_NUM_THREADS = "1")
Sys.setenv(OPENBLAS_NUM_THREADS = "1")
Sys.setenv(VECLIB_MAXIMUM_THREADS = "1")
Sys.setenv(NUMEXPR_NUM_THREADS = "1")

# Force single-threaded BLAS if using RhpcBLASctl
if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
    RhpcBLASctl::blas_set_num_threads(1)
    RhpcBLASctl::omp_set_num_threads(1)
}

# ============================================================================
# 1. PACKAGE MANAGEMENT
# ============================================================================
packages_used_in_this_project <- c(
    "randomForest", 
    "xgboost", 
    "ggplot2", 
    "patchwork", 
    "openxlsx", 
    "fastshap",
    "parallel",
    "reshape2"
)

for(pkg in packages_used_in_this_project) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg, dependencies = TRUE)
        library(pkg, character.only = TRUE)
    }
}

print("All packages loaded successfully")

# ============================================================================
# 2. SOURCE MODULES
# ============================================================================
source("metrics.R")
source("datamanagement.R")
source("metamodels.R")
source("shp_calc.R")
source("analysis.R")

# ============================================================================
# 3. CONFIGURATION
# ============================================================================
sites <- c("C1", "C2", "C3", "G3", "G4")
variables <- c("GPP", "RECO", "NEE", "N2O", "Yield")
selected_years <- c("2007", "2011")
stage <- "Stage3"
data_dir <- "../data"

# Validation method:
#   "loyo_full" = leave one year out for each year, average results (default, most robust)
#   "loyo_single" = leave one random year out (faster but less robust)
#   "random" = random 70/30 split (not recommended for time series)
validation_method <- "loyo_full"

# XGBoost hyperparameter search configuration:
#   n_evals: number of random hyperparameter sets to try
#     - 10-20 = very fast, less optimal (good for testing)
#     - 50-100 = balanced (default, recommended)
#     - 200+ = slower but more thorough
xgb_n_evals <- 50           # Number of hyperparameter evaluations  
xgb_nrounds <- 1000         # Max boosting rounds

# ============================================================================
# 4. LOAD DATA
# ============================================================================
print("Loading data...")
dataList <- get_data(sites, variables, selected_years, stage = stage, data_dir = data_dir)

# ============================================================================
# 5. PLOT BASELINE DATA
# ============================================================================
print("Creating baseline plots...")
for(variab in variables){
    output_file <- paste0(stage, "_", variab, "_baseline.pdf")
    plot_baseline(dataList, sites, variab, output_file = output_file)
}

# ============================================================================
# 6. RUN ANALYSIS FOR EACH VARIABLE
# ============================================================================
results <- list()

for(variab in variables){
    print(paste("==================", variab, "=================="))
    wb <- createWorkbook()
    filename <- paste0(stage, "_", variab, "_results.xlsx")
    results[[variab]] <- perform_analysis_for_variables(
        dataList, 
        sites, 
        variab, 
        wb, 
        filename,
        validation_method = validation_method,
        xgb_n_evals = xgb_n_evals,
        xgb_nrounds = xgb_nrounds
    )
    print(paste("Results saved to:", filename))
}

print("================== Analysis complete ==================")
print("Results stored in 'results' list")
