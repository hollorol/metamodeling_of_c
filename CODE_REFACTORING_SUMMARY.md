# Code Refactoring Summary

## Overview
The code has been completely refactored and cleaned up from the old version that was "piled together from old code". All files now follow proper structure and work with the actual database structure.

## Changes Made

### 1. **datamanagement.R**
**Fixed:**
- Updated `get_data()` function to use correct file paths with `data/` directory prefix
- Added proper error handling for missing files
- Made the function more flexible with parameters for `stage` and `data_dir`
- Fixed data loading to match actual database structure (semicolon-separated CSV files for variables, comma-separated for meteo)
- Improved `plot_baseline()` function with configurable output file paths

**Key improvements:**
```r
get_data(sites, variables, selected_years, stage = "Stage3", data_dir = "../data")
```

### 2. **metamodels.R**
**Fixed:**
- **Removed 5 duplicate definitions** of `get_xgb()` function
- Kept only the final, most advanced version with:
  - Random search hyperparameter tuning
  - Parallel processing using `future` and `future.apply`
  - Progress bar for tracking
  - Proper validation using hold-out set
  - Retraining on full training data with best hyperparameters
- Removed dependencies on `rBayesianOptimization` (was causing issues with `browser()` call)
- Clean, well-documented code with clear sections

**Key features of final `get_xgb()`:**
- 10,000 random hyperparameter evaluations (configurable)
- Parallel execution using all available cores
- Early stopping to prevent overfitting
- Returns model, predictions, best parameters, and validation RMSE

### 3. **analysis.R**
**Fixed:**
- Updated `perform_analysis_for_variables()` to use correct SHAP function names
- Fixed inconsistent function calls
- Improved documentation and code structure
- Added proper error handling for sites with no data
- Fixed typo in column name ("XBG+" -> "XGB+")
- Ensured consistency with other modules

**Models evaluated:**
1. Best individual model
2. Multi-Model Median (MMM)
3. Multiple Linear Regression (MLR)
4. Random Forest (RF)
5. XGBoost (XGB)
6. XGBoost+ with meteorological features (XGB+)

### 4. **shp_calc.R**
**Fixed:**
- Cleaned up `mean_shp_vals_rf()` function
- Removed debugging code (`browser()` calls)
- Fixed prediction wrapper for Random Forest
- **Added missing `compute_shap_xgb()` function** that was referenced but not defined
- Made it an alias for `mean_shp_vals_xgb()` for backward compatibility

### 5. **visualize.R**
**Fixed:**
- Completely rewrote to remove all duplicate code sections
- Organized into clear sections:
  1. Package management
  2. Module sourcing
  3. Configuration
  4. Data loading
  5. Baseline plotting
  6. Analysis execution
- Removed hundreds of lines of redundant visualization code
- Made configuration variables clear and easy to modify
- Added proper package dependencies including `reshape2`

**Configuration variables:**
```r
sites <- c("C1", "C2", "C3", "G3", "G4")
variables <- c("GPP", "RECO", "NEE", "N2O", "Yield")
selected_years <- c("2007", "2011")
stage <- "Stage3"  # or "Stage5"
data_dir <- "../data"
```

### 6. **metrics.R**
**No changes needed** - This file was already clean and functional.

## Database Structure Understanding

The code now correctly handles:

### Meteorological Data Files:
- Format: `{site}_meteo.csv` (e.g., `C1_meteo.csv`)
- Separator: comma (`,`)
- Columns: Date, Type, Prec, Tmean, Tmin, Tmax, Rad, Wind, dewp, vprs
- Date format: `YYYY.MM.DD`

### Variable Data Files:
- Format: `{stage}_{site}_{variable}_daily.csv` (e.g., `Stage3_C1_GPP_daily.csv`)
- Separator: semicolon (`;`)
- Columns: Year, Type, M01-M26 (model outputs), Obs (observations)

### Available Variables:
- GPP (Gross Primary Production)
- RECO (Ecosystem Respiration)
- NEE (Net Ecosystem Exchange)
- N2O (Nitrous Oxide)
- Yield

### Available Sites:
- C1, C2, C3 (crop sites)
- G3, G4 (grassland sites)

### Available Stages:
- Stage3
- Stage5

## How to Use

1. **Set working directory to src/**:
   ```r
   setwd("path/to/metamodeling_of_c/src")
   ```

2. **Run the main script**:
   ```r
   source("visualize.R")
   ```

3. **Results will be saved as**:
   - Excel files: `Stage3_{variable}_results.xlsx`
   - PDF plots: `Stage3_{variable}_baseline.pdf`

4. **Access results in R**:
   ```r
   # View available variables
   names(results)
   
   # Access specific site results
   results$GPP$C1$performance_table
   results$GPP$C1$shap_matrix
   ```

## Dependencies

Required R packages (automatically installed if missing):
- `randomForest` - Random Forest models
- `xgboost` - XGBoost models
- `ggplot2` - Plotting
- `patchwork` - Combining plots
- `openxlsx` - Excel file writing
- `fastshap` - SHAP value calculation
- `future` - Parallel processing framework
- `future.apply` - Parallel apply functions
- `reshape2` - Data reshaping

## Performance Improvements

1. **Parallel hyperparameter tuning**: Uses all available CPU cores
2. **Efficient data loading**: Reads only necessary files
3. **Progress tracking**: Progress bar for long-running operations
4. **Early stopping**: Prevents overfitting in XGBoost training

## File Organization

```
metamodeling_of_c/
├── data/                    # Data files (CSV)
├── src/                     # Source code
│   ├── metrics.R           # Performance metrics (RMSE, RRMSE, R)
│   ├── datamanagement.R    # Data loading and preprocessing
│   ├── metamodels.R        # XGBoost model training
│   ├── shp_calc.R          # SHAP value calculations
│   ├── analysis.R          # Main analysis workflow
│   └── visualize.R         # Main execution script
└── README.md               # This file

```

## Notes

- The old `visualize.R` has been backed up to `visualize.R.bak`
- All functions are now properly documented
- Code follows consistent R styling conventions
- Error handling has been improved throughout
