# Parallelization Fix

## Problem
The hyperparameter optimization was blocking with 0% CPU load. The issue was with the `future` and `future.apply` packages not working properly on this system.

## Solution
Replaced `future`/`future.apply` with base R `parallel` package using `mclapply()`.

## Changes Made

### 1. metamodels.R
- **Removed**: `library(future)`, `library(future.apply)`
- **Added**: `library(parallel)`
- **Changed**: `plan(multicore, workers = n_cores)` → removed (not needed for mclapply)
- **Changed**: `future_lapply(1:n_evals, xgb_objective_rs)` → `mclapply(1:n_evals, xgb_objective_rs, mc.cores = n_cores)`
- **Fixed**: Reorganized code so `random_params_list` is generated BEFORE `xgb_objective_rs` function definition (proper scoping)
- **Removed**: Progress bar (`pb`) references that were causing scope issues

### 2. visualize.R
- **Removed**: `"future"`, `"future.apply"` from packages list
- **Added**: `"parallel"` to packages list

## Technical Details

### Why `mclapply` is better here:
1. **Built-in**: Part of base R, no external dependencies
2. **Simpler**: Direct forking without complex futures infrastructure
3. **Reliable**: Works consistently across Linux systems
4. **Efficient**: Lower overhead than future_lapply

### Function reorganization:
```r
# BEFORE (didn't work - scope issue):
xgb_objective_rs <- function(i) { 
  # tries to use random_params_list 
}
# Generate random_params_list here

# AFTER (works - proper scope):
# Generate random_params_list first
random_params_list <- vector("list", n_evals)
for (i in 1:n_evals) { ... }

# Then define function that uses it
xgb_objective_rs <- function(i) {
  params_list <- random_params_list[[i]]  # Now in scope!
  ...
}
```

## Testing
Both files pass R syntax validation:
- ✅ `metamodels.R` - Valid syntax
- ✅ `visualize.R` - Valid syntax

## Expected Behavior
When running the analysis, you should now see:
1. **CPU utilization > 0%** during hyperparameter search
2. Progress messages from XGBoost training
3. Completion of all 50 evaluations per model
4. Results written to Excel files

## Performance
With 24 cores and 50 evaluations:
- Previous: Blocked indefinitely (0% CPU)
- Expected now: ~1-2 minutes per variable (depending on data size)

## Notes
- `mclapply` uses forking on Linux, which is very efficient
- Each fork gets a copy of `random_params_list` in its scope
- No shared state means no race conditions
- Results are collected automatically when all workers finish
