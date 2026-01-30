# Quick Start Guide

## What Was Fixed

Your code had been "piled together from old code" with multiple issues:
- **5 duplicate definitions** of the `get_xgb()` function in metamodels.R
- Incorrect file paths (missing `data/` directory prefix)
- Missing `compute_shap_xgb()` function that was being called
- Hundreds of lines of duplicate visualization code
- Inconsistent function calls and debugging code left in production

All of these issues have been **completely fixed**.

## How to Run the Analysis

### Step 1: Navigate to the src directory
```bash
cd /home/hollorol/projects/metamodeling_of_c/src
```

### Step 2: Start R
```bash
R
```

### Step 3: Run the main script
```r
source("visualize.R")
```

That's it! The script will:
1. Load all required packages (installs if missing)
2. Load data from the `../data` directory
3. Create baseline plots for each variable
4. Run analysis for all variables (GPP, RECO, NEE, N2O, Yield)
5. Save results to Excel files and PDF plots

## Output Files

The analysis will create:
- `Stage3_GPP_results.xlsx` - GPP model predictions and performance
- `Stage3_RECO_results.xlsx` - RECO model predictions and performance
- `Stage3_NEE_results.xlsx` - NEE model predictions and performance
- `Stage3_N2O_results.xlsx` - N2O model predictions and performance
- `Stage3_Yield_results.xlsx` - Yield model predictions and performance
- `Stage3_*_baseline.pdf` - Baseline observation plots

Each Excel file contains worksheets for:
- Model predictions (per site)
- Performance metrics (per site)
- SHAP values for feature importance (per site)

## Customization

To change settings, edit these variables in `visualize.R`:

```r
sites <- c("C1", "C2", "C3", "G3", "G4")  # Which sites to analyze
variables <- c("GPP", "RECO", "NEE", "N2O", "Yield")  # Which variables
selected_years <- c("2007", "2011")  # Years to filter for C1 site
stage <- "Stage3"  # Or "Stage5"
data_dir <- "../data"  # Path to data directory
```

## What Each File Does

| File | Purpose |
|------|---------|
| `metrics.R` | Performance metrics (RMSE, RRMSE, R) |
| `datamanagement.R` | Load and preprocess data from CSV files |
| `metamodels.R` | Train XGBoost models with hyperparameter tuning |
| `shp_calc.R` | Calculate SHAP values for feature importance |
| `analysis.R` | Run all models and compare performance |
| `visualize.R` | **Main script** - Run this to do everything |

## Backup

Your original `visualize.R` has been saved as `visualize.R.bak` in case you need to reference it.

## Need Help?

See `CODE_REFACTORING_SUMMARY.md` for detailed information about all changes made.
