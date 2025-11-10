
# Cross validate and fine tune models
cross_validate_tune <- function(model_func, eval_func, X, Y, param_grid,
                                folds = 10, seed = 101025, metric = "Accuracy") {
  if (!requireNamespace("caret", quietly = TRUE))
    stop("Please install the 'caret' package.")
  
  set.seed(seed)
  folds_idx <- caret::createFolds(Y, k = folds, list = TRUE, returnTrain = FALSE)
  
  results <- list()
  
  for (i in seq_along(param_grid)) {
    params <- param_grid[[i]]
    fold_metrics <- matrix(NA, nrow = folds, ncol = 5,
                           dimnames = list(NULL, c("Accuracy", "AUC", "Precision", "Recall", "F1")))
    
    for (f in seq_along(folds_idx)) {
      test_idx <- folds_idx[[f]]
      train_idx <- setdiff(seq_along(Y), test_idx)

      print(params)
      model <- do.call(model_func, c(list(X = X[train_idx, , drop = FALSE],
                                          Y = Y[train_idx]), params))
      
      metrics <- eval_func(model, X[test_idx, , drop = FALSE], Y[test_idx])
      fold_metrics[f, ] <- metrics
    }
    
    avg_metrics <- colMeans(fold_metrics, na.rm = TRUE)
    results[[i]] <- list(params = params, folds = fold_metrics, avg_metrics = avg_metrics)
  }
  
  metric_values <- sapply(results, function(r) r$avg_metrics[[metric]])
  best_idx <- which.max(metric_values)
  best_params <- results[[best_idx]]$params
  
  best_model <- do.call(model_func, c(list(X = X, Y = Y), best_params))
  
  list(
    best_model = best_model,
    best_params = best_params,
    best_metrics = results[[best_idx]]$avg_metrics,
    all_results = results
  )
}
