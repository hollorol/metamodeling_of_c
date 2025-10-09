get_xgb <- function(features, obs, selected_for_training){
    data_for_xgb <- features[selected_for_training,]
    label_for_xgb <- obs[selected_for_training]
    dtrain <- xgb.DMatrix(data = as.matrix(data_for_xgb), label = as.vector(label_for_xgb))

    # Create a validation set from the remaining data (not selected for training)
    data_for_valid <- features[!selected_for_training, ]
    label_for_valid <- obs[!selected_for_training]
    dvalid <- xgb.DMatrix(data = as.matrix(data_for_valid), label = as.vector(label_for_valid))

    # Define watchlist
    watchlist <- list(train = dtrain, eval = dvalid)

    
    params <- list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      max_depth = 7
    )

    # Train model with early stopping
    xgb_model <- xgb.train(
      params = params,
      data = dtrain,
      watchlist = watchlist,
      nrounds = 10000,
      early_stopping_rounds = 20,
      verbose = 1
    )
    list(model=xgb_model,prediction=predict(xgb_model, as.matrix(features)))
}

  require(xgboost)
  require(parallel)
get_xgb <- function(features, obs, selected_for_training,
                    n_trials = 50,        # how many random sets to try
                    n_threads = 24,       # number of parallel workers
                    nrounds = 5000,       # max boosting rounds per trial
                    early_stop = 20       # early stopping
) {
  # dependencies

  # Prepare DMatrix objects once
  X_train <- as.matrix(features[selected_for_training, ])
  y_train <- obs[selected_for_training]
  X_valid <- as.matrix(features[!selected_for_training, ])
  y_valid <- obs[!selected_for_training]
  dtrain <- xgb.DMatrix(X_train, label = y_train)
  dvalid <- xgb.DMatrix(X_valid, label = y_valid)
  watchlist <- list(train = dtrain, eval = dvalid)

  # Function to sample one set of hyperparams
  sample_params <- function() {
    list(
      objective          = "reg:squarederror",
      eval_metric        = "rmse",
      eta                = runif(1, 0.01, 0.2),        # learning rate
      max_depth          = sample(3:10, 1),           # tree depth
      subsample          = runif(1, 0.6, 1.0),         # row sampling
      colsample_bytree   = runif(1, 0.6, 1.0),         # feature sampling
      lambda             = 10^runif(1, -2, 1),         # L2 regularization
      alpha              = 10^runif(1, -2, 1)          # L1 regularization
    )
  }

  # One trial of training + eval
  train_trial <- function(i) {
    set.seed(i)
    params <- sample_params()
    model <- xgb.train(
      params                = params,
      data                  = dtrain,
      watchlist             = watchlist,
      nrounds               = nrounds,
      early_stopping_rounds = early_stop,
      verbose               = 0,
      nthread               = 1   # each worker uses 1 thread
    )
    list(rmse = model$best_score, model = model, params = params)
  }

  print("Training trials in parallel...")
  # Run trials in parallel
  trials <- mclapply(1:n_trials, train_trial, mc.cores = n_threads)

  # Select the best trial (lowest validation RMSE)
  best_idx    <- which.min(sapply(trials, `[[`, "rmse"))
  best_trial  <- trials[[best_idx]]
  best_model  <- best_trial$model
  best_params <- best_trial$params
  best_rmse   <- best_trial$rmse

  # Final predictions on all data
  full_preds <- predict(best_model, as.matrix(features))

  list(
    model       = best_model,
    predictions = full_preds,
    best_rmse   = best_rmse,
    best_params = best_params
  )
}

get_best_xgb <- function(features, obs, selected_for_training,
                         n_trials   = 50,
                         n_threads  = 20,
                         nrounds    = 5000,
                         early_stop = 20) {

  # 1) build your DMatrix objects
  X_train <- as.matrix(features[selected_for_training, ])
  y_train <- obs[selected_for_training]
  X_valid <- as.matrix(features[!selected_for_training, ])
  y_valid <- obs[!selected_for_training]

  dtrain <- xgb.DMatrix(X_train, label = y_train)
  dvalid <- xgb.DMatrix(X_valid, label = y_valid)
  watchlist <- list(train = dtrain, eval = dvalid)

  # 2) hyperparameter sampler
  sample_params <- function() {
    list(
      objective        = "reg:squarederror",
      eval_metric      = "rmse",
      eta              = runif(1, 0.01, 0.2),
      max_depth        = sample(3:10, 1),
      subsample        = runif(1, 0.6, 1.0),
      colsample_bytree = runif(1, 0.6, 1.0),
      lambda           = 10^runif(1, -1, 1),
      alpha            =  10^runif(1, -1, 1)
    )
  }

  # 3) single‐trial trainer
  train_trial <- function(i) {
    set.seed(i)
    params <- sample_params()
    model <- xgb.train(
      params                = params,
      data                  = dtrain,
      watchlist             = watchlist,
      nrounds               = nrounds,
      early_stopping_rounds = early_stop,
      verbose               = 0,
      nthread               = 1  # ← enforce single‐threaded
    )
    list(rmse = model$best_score, model = model, params = params)
  }

  # 4) spin up a PSOCK cluster
  cl <- makeCluster(n_threads, type = "PSOCK")
  clusterEvalQ(cl, {
    library(xgboost)
    Sys.setenv(OMP_NUM_THREADS = "1")
  })
  clusterExport(cl,
    c("dtrain","dvalid","watchlist",
      "sample_params","nrounds","early_stop","train_trial"),
    envir = environment()
  )

  # 5) run and collect
  trials <- parLapply(cl, 1:n_trials, train_trial)
  stopCluster(cl)

  # 6) pick best
  best_idx   <- which.min(sapply(trials, `[[`, "rmse"))
  best_trial <- trials[[best_idx]]
  best_model <- best_trial$model

  # 7) final preds
  full_pred <- predict(best_model, as.matrix(features))

  list(
    model       = best_model,
    predictions = full_pred,
    best_rmse   = best_trial$rmse,
    best_params = best_trial$params
  )
}

get_xgb <- function(features,
                    obs,
                    selected_for_training,
                    tune_length     = 20,    # number of random hyper‐parameter sets
                    cv_folds        = 5) { # for reproducible tuning

  # ─── Dependencies ───────────────────────────────────────────────────────────

  # ─── 1) Split into train / validation (for final prediction only) ───────────
  X_all <- as.data.frame(features)
  y_all <- obs

  train_idx <- selected_for_training
  X_train <- X_all[train_idx, , drop = FALSE]
  y_train <- y_all[train_idx]
  X_full  <- X_all
  
  # ─── 2) set up caret::trainControl with random search and parallelism ─────
  cl <- makeCluster( min(tune_length, parallel::detectCores()) )
  registerDoParallel(cl)

  ctrl <- trainControl(
    method          = "cv",
    number          = cv_folds,
    search          = "random",
    verboseIter     = FALSE,
    allowParallel   = TRUE
  )

  # ─── 3) bundle into one data.frame for caret ──────────────────────────────
  train_df <- cbind(y = y_train, X_train)
  colnames(train_df)[1] <- "y"

  # ─── 4) launch the random‐search XGBoost fitting ───────────────────────────
  fit <- train(
    y ~ .,
    data     = train_df,
    method   = "xgbTree",
    trControl= ctrl,
    tuneLength = tune_length,
    verbose    = FALSE
  )

  stopCluster(cl)
  registerDoSEQ()

  # ─── 5) predict on full dataset ────────────────────────────────────────────
  preds <- predict(fit, newdata = X_full)

  # ─── 6) return in your original format ────────────────────────────────────
  list(
    model      = fit,
    prediction = preds
  )
}

library(xgboost)
library(rBayesianOptimization)

get_xgb <- function(features,
                                  obs,
                                  selected_for_training,
                                  init_points   = 8,     # random starting points
                                  n_iter        = 12,    # Bayesian iters
                                  nrounds       = 5000,  # max rounds per trial
                                  early_stop    = 20,    # early stopping
                                  nthread       = 24,    # threads for xgb
                                  seed          = 123) {

  # 1) build training + validation DMatrix
  Xtr <- as.matrix(features[selected_for_training, , drop = FALSE])
  ytr <-  obs[selected_for_training]
  Xva <- as.matrix(features[!selected_for_training, , drop = FALSE])
  yva <-  obs[!selected_for_training]

  dtrain <- xgb.DMatrix(Xtr, label = ytr)
  dvalid <- xgb.DMatrix(Xva, label = yva)
  watchlist <- list(train = dtrain, eval = dvalid)

  # 2) objective for hold‐out
  xgb_holdout_obj <- function(eta,
                              max_depth,
                              subsample,
                              colsample_bytree,
                              min_child_weight,
                              lambda,
                              alpha) {

    params <- list(
      objective           = "reg:squarederror",
      eval_metric         = "rmse",
      tree_method        = "hist",
      eta                 = eta,
      max_depth           = as.integer(max_depth),
      subsample           = subsample,
      colsample_bytree    = colsample_bytree,
      min_child_weight    = min_child_weight,
      lambda              = lambda,
      alpha               = alpha
    )

    bst <- xgb.train(
      params                = params,
      data                  = dtrain,
      watchlist             = watchlist,
      nrounds               = nrounds,
      early_stopping_rounds = early_stop,
      verbose               = 0,
      nthread               = nthread
    )

    # we want to maximize Score, so return negative RMSE
    list(Score = -bst$best_score,
         nrounds = bst$best_iteration)
  }

  # 3) run Bayesian optimization
  set.seed(seed)
  bounds <- list(
    eta              = c(0.01, 0.3),
    max_depth        = c(3L, 10L),
    subsample        = c(0.6, 1.0),
    colsample_bytree = c(0.6, 1.0),
    min_child_weight = c(1L, 10L),
    lambda           = c(0.01, 10),
    alpha            = c(0.0, 10)
  )

  bo <- BayesianOptimization(
    FUN         = xgb_holdout_obj,
    bounds      = bounds,
    init_points = init_points,
    n_iter      = n_iter,
    acq         = "ucb",
    kappa       = 2.576,
    eps         = 0.0,
    verbose     = TRUE
  )
  browser()
  # 4) extract best params & rounds
  best_params  <- bo$Best_Par
  best_nrounds <- bo$History$best[nrow(bo$History)]

  final_params <- as.list(best_params)
  final_params$objective   <- "reg:squarederror"
  final_params$eval_metric <- "rmse"

  # 5) retrain on training‐only with chosen rounds
  final_mod <- xgb.train(
    params  = final_params,
    data    = dtrain,
    nrounds = best_nrounds,
    verbose = 1,
    nthread = nthread
  )

  # 6) predict on every row
  all_preds <- predict(final_mod, as.matrix(features))

  list(
    model       = final_mod,
    prediction  = all_preds,
    best_params = best_params,
    best_nrounds= best_nrounds,
    valid_rmse  = -bo$Best_Value
  )
}



library(future)
library(future.apply)

get_xgb <- function(features,
                                 obs,
                                 selected_for_training,
                                 n_evals = 10000,         # Total number of random evaluations
                                 sample_fraction = 1.0, # Set < 1.0 for sampling during tuning
                                 nrounds = 5000,        # Max rounds for xgb.train
                                 early_stop = 20,       # Early stopping for xgb.train
                                 nthread = 1,          # Threads for each xgb.train call
                                 seed = 123) {
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                      max = n_evals,  # Maximum value of the progress bar
                     style = 3,      # Progress bar style (also available style = 1 and style = 2)
                     width = 50)     # Character used to create the bar
  # --- 1) Build FULL training + validation matrices/DMatrix ---
  message("Setting up data...")
  Xtr_full <- as.matrix(features[selected_for_training, , drop = FALSE])
  ytr_full <- obs[selected_for_training]
  Xva <- as.matrix(features[!selected_for_training, , drop = FALSE])
  yva <- obs[!selected_for_training]

  dtrain_full <- xgb.DMatrix(Xtr_full, label = ytr_full) # For final training
  dvalid <- xgb.DMatrix(Xva, label = yva)              # For validation during tuning
  watchlist <- list(eval = dvalid)                      # Watchlist for xgb.train

  # --- 1b) Create a SAMPLE of the training data FOR TUNING ONLY (if requested) ---
  dtrain_tuning <- dtrain_full # Default to full data
  if (sample_fraction < 1.0 && sample_fraction > 0.0) {
      set.seed(seed) # Ensure sample is reproducible
      n_train_full <- nrow(Xtr_full)
      sample_size <- floor(sample_fraction * n_train_full)
      if (sample_size < 10) {
          warning("Sample size for tuning is very small. Using full data instead.")
      } else {
          message(sprintf("Creating training sample of %d rows (%.1f%%) for tuning evaluations.",
                          sample_size, sample_fraction * 100))
          sample_indices <- sample(1:n_train_full, size = sample_size)
          Xtr_sample <- Xtr_full[sample_indices, , drop = FALSE]
          ytr_sample <- ytr_full[sample_indices]
          dtrain_tuning <- xgb.DMatrix(Xtr_sample, label = ytr_sample) # Use this for tuning
      }
  } else {
     message("Using full training data for tuning evaluations.")
  }


  # --- 2) Objective function for Random Search evaluation ---
  # Takes a list of parameters, returns score, nrounds, and params
  xgb_objective_rs <- function(params_list) {
      setTxtProgressBar(pb,getTxtProgressBar(pb)+1)
      # Set default tree method (or allow it in params_list if needed)
      tree_method <- "exact" # Or "hist" if you found it beneficial previously

      params_xgb <- list(
          objective        = "reg:squarederror",
          eval_metric      = "rmse",
          tree_method      = tree_method,
          eta              = params_list$eta,
          max_depth        = as.integer(params_list$max_depth),
          subsample        = params_list$subsample,
          colsample_bytree = params_list$colsample_bytree,
          min_child_weight = as.integer(params_list$min_child_weight),
          lambda           = params_list$lambda,
          alpha            = params_list$alpha
      )

      bst <- tryCatch({
          xgb.train(
              params                = params_xgb,
              data                  = dtrain_tuning,     # Use sample or full data for tuning
              watchlist             = watchlist,
              nrounds               = nrounds,
              early_stopping_rounds = early_stop,
              verbose               = 0,
              nthread               = nthread # Threads for THIS XGBoost run
          )
      }, error = function(e) {
          warning("xgb.train failed for params: ", paste(names(params_list), params_list, collapse=", "), " Error: ", e$message)
          NULL # Return NULL on error
      })

      if (is.null(bst) || bst$best_iteration < 1) {
          list(Score = -Inf, nrounds = NA, Params = params_list) # Return poor score if failed
      } else {
          list(Score = -bst$best_score,          # Negative RMSE (higher is better)
               nrounds = bst$best_iteration,
               Params = params_list)           # Return the parameters tested
      }
  }

  # --- 3) Define bounds and generate random parameter sets ---
  message(sprintf("Generating %d random hyperparameter sets...", n_evals))
  set.seed(seed)
  bounds <- list(
    eta              = c(0.01, 0.3),
    max_depth        = c(3L, 10L),
    subsample        = c(0.6, 1.0),
    colsample_bytree = c(0.6, 1.0),
    min_child_weight = c(1L, 10L),
    lambda           = c(0.01, 10.0),
    alpha            = c(0.0, 10.0)
  )

  random_params_list <- vector("list", n_evals)
  for (i in 1:n_evals) {
      random_params_list[[i]] <- list(
          eta              = runif(1, bounds$eta[1], bounds$eta[2]),
          max_depth        = sample(bounds$max_depth[1]:bounds$max_depth[2], 1),
          subsample        = runif(1, bounds$subsample[1], bounds$subsample[2]),
          colsample_bytree = runif(1, bounds$colsample_bytree[1], bounds$colsample_bytree[2]),
          min_child_weight = sample(bounds$min_child_weight[1]:bounds$min_child_weight[2], 1),
          lambda           = runif(1, bounds$lambda[1], bounds$lambda[2]),
          alpha            = runif(1, bounds$alpha[1], bounds$alpha[2])
      )
  }

  # --- 4) Evaluate parameter sets in parallel ---
  message(sprintf("Starting parallel evaluation of %d sets using %d workers...", n_evals, future::nbrOfWorkers()))
  start_time <- Sys.time()

  # Use future_lapply for parallel execution
  # future.seed = TRUE ensures reproducibility of RNG within parallel tasks
  results_list <- future_lapply(random_params_list, xgb_objective_rs, future.seed = TRUE)

  end_time <- Sys.time()
  time_taken <- end_time - start_time
  message(sprintf("Parallel evaluation finished in %.2f %s", time_taken, units(time_taken)))

  # --- 5) Find best result ---
  best_score <- -Inf
  best_result <- NULL
  valid_results_count <- 0

  for (res in results_list) {
      if (!is.null(res) && !is.na(res$Score) && is.finite(res$Score)) {
          valid_results_count <- valid_results_count + 1
          if (res$Score > best_score) {
              best_score <- res$Score
              best_result <- res
          }
      }
  }

  if (is.null(best_result)) {
      stop("Random search failed to produce any valid results. Check objective function or parameters.")
  }
  message(sprintf("Found best result from %d valid evaluations.", valid_results_count))

  best_params_values <- best_result$Params
  best_nrounds <- best_result$nrounds
  best_valid_rmse <- -best_score # Convert back to positive RMSE

  message(sprintf("Best validation RMSE from Random Search: %.6f", best_valid_rmse))
  message("Best parameters found:")
  print(unlist(best_params_values))
  message(sprintf("Best nrounds found: %d", best_nrounds))


  # --- 6) Retrain final model on FULL training data ---
  message("Retraining final model on FULL training data...")
  final_params <- best_params_values # Already a list
  # Set mandatory params
  final_params$objective   <- "reg:squarederror"
  final_params$eval_metric <- "rmse"
  # Set tree_method consistently if needed (e.g., based on what objective used)
  final_params$tree_method <- "exact" # Or "hist"

  final_mod <- xgb.train(
    params  = final_params,
    data    = dtrain_full, # <--- Use FULL training data
    nrounds = best_nrounds,
    verbose = 1,
    nthread = nthread # Use specified threads for final train
  )

  # --- 7) Predict on every row ---
  message("Generating predictions on full feature set...")
  all_preds <- predict(final_mod, as.matrix(features))

  # --- 8) Return results ---
  message("Random Search complete.")
  list(
    model         = final_mod,
    prediction    = all_preds,
    best_params   = best_params_values,
    best_nrounds  = best_nrounds,
    valid_rmse    = best_valid_rmse, # Best RMSE found during tuning
    tuning_time   = time_taken,      # How long the parallel search took
    all_results   = results_list     # Optional: return all results for inspection
  )
}

plan(multicore, workers = 24)
