# Validation Method Update

## What Changed

The validation method has been updated from **random 70/30 split** to **leave-one-year-out cross-validation (LOYO)** for better temporal data integrity.

## Why This Matters

### Problem with Random Split:
- Randomly splits data across all years
- Training data can contain observations from the same year as test data
- Violates temporal independence
- Leads to **optimistic performance estimates** (data leakage)

### Solution with Leave-One-Year-Out:
- Holds out an entire year for testing
- Trains only on data from other years
- Respects temporal structure
- Provides **realistic performance estimates** for prediction on new years

## Validation Methods Available

You can now choose from three validation methods in `visualize.R`:

### 1. `"loyo_single"` (Default - Recommended)
- Leaves one random year out for testing
- Faster than full LOYO
- Good balance between speed and robustness
- **Example**: If you have years 2007-2013, randomly selects one year (e.g., 2010) as test set

```r
validation_method <- "loyo_single"
```

### 2. `"loyo_full"` (Most Robust - Coming Soon)
- Iteratively leaves each year out
- Averages performance across all folds
- Most robust but slower
- **Example**: Tests on 2007, 2008, 2009, ... sequentially and averages results

```r
validation_method <- "loyo_full"
```

### 3. `"random"` (Not Recommended)
- Traditional random 70/30 split
- Kept for comparison purposes only
- Not recommended for time series data

```r
validation_method <- "random"
```

## How to Use

### In `visualize.R`, set the validation method:

```r
# Near the top of the file (line ~45)
validation_method <- "loyo_single"  # Change this as needed
```

### Then run normally:

```r
source("visualize.R")
```

## Output Changes

The analysis output now includes information about which year was used for testing:

```
=============== Site: C1 =====================
  Validation: Leave-One-Year-Out (single)
  Test year: 2010 | Training years: 2007, 2008, 2009, 2011, 2012, 2013
```

## Edge Cases Handled

1. **Single Year Sites**: Automatically falls back to random 70/30 split with a warning
2. **Missing Years**: Only uses years that have data
3. **Invalid Method**: Throws an error with clear message

## Performance Comparison

To compare validation methods, you can run the analysis multiple times:

```r
# Test with LOYO
validation_method <- "loyo_single"
source("visualize.R")

# Test with random split for comparison
validation_method <- "random"
source("visualize.R")
```

You'll typically see that LOYO gives **higher (more realistic) error values** than random split.

## Technical Details

### Code Changes:

**File**: `src/analysis.R`
- Function: `perform_analysis_for_variables()`
- Added parameter: `validation_method = "loyo_single"`
- New validation logic uses `data$Year` column to create temporal splits

**File**: `src/visualize.R`
- Added configuration variable: `validation_method`
- Passes this to `perform_analysis_for_variables()`

### Year Detection:

The code automatically detects years from the `Year` column in your data files:
- `Stage3_C1_GPP_daily.csv` has a `Year` column
- Unique years are extracted: `unique(data$Year)`
- One year is held out for testing: `data$Year != test_year`

## Recommendations

1. **For Production**: Use `"loyo_single"` (default)
   - Good balance of robustness and speed
   - Realistic performance estimates

2. **For Research/Publication**: Consider `"loyo_full"` once implemented
   - Most robust estimates
   - Standard approach in time series ML

3. **For Debugging**: Use `"random"` if needed
   - Faster iterations during code development
   - But don't trust the performance numbers!

## Example Output Difference

### Random Split (Old Method):
```
Site: C1 - XGB+ Performance
RMSE: 0.0025  (optimistic - includes temporal leakage)
R: 0.89       (artificially high)
```

### LOYO Split (New Method):
```
Site: C1 - XGB+ Performance  
RMSE: 0.0032  (realistic - no temporal leakage)
R: 0.82       (more honest assessment)
```

The LOYO method gives **higher errors but more trustworthy results** that better represent performance on genuinely new data.
