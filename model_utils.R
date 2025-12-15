# Fitting xgb binary classifiers
fit_xgb_binary_classifier <- function(nrounds, X, Y, ...) {
  dtrain <- xgboost::xgb.DMatrix(data = as.matrix(X), label = Y)
  fit <- xgboost::xgb.train(data = dtrain,
                            nrounds = nrounds,
                            objective = 'binary:logistic',
                            ...)
  fit
}

# Predicting with binary classifiers
predict_model <- function(model, X_new) {
  if (inherits(model, "xgb.Booster")) {
    preds_prob <- predict(model, as.matrix(X_new))
    preds <- ifelse(preds_prob > 0.5, 1, 0)
  } else if ("glm" %in% class(model)) {
    preds_prob <- predict(model, newdata = data.frame(X_new), type = "response")
    preds <- ifelse(preds_prob > 0.5, 1, 0)
  } else if ("randomForest" %in% class(model)) {
    preds_prob <- predict(model, X_new, type = "prob")[, 2]
    preds <- ifelse(preds_prob > 0.5, 1, 0)
  } else {
    stop("Unsupported model type in predict_model.")
  }
  
  list(prob = preds_prob, class = factor(preds))
}

# Evaluating on test data
evaluate_model <- function(model, df.test, features, outcome) {
  
  if (!requireNamespace("caret", quietly = TRUE))
    stop("Please install the 'caret' package.")
  if (!requireNamespace("pROC", quietly = TRUE))
    stop("Please install the 'pROC' package.")

  # Get predictions
  X.test <- df.test[, features]
  Y.test <- df.test[[outcome]]
  preds_out <- predict_model(model, X.test)
  preds <- preds_out$class
  preds_prob <- preds_out$prob
  preds_factor <- factor(preds, levels = c(0, 1))
  Y_test_factor <- factor(Y.test, levels = c(0, 1))

  # Standard classification metrics
  cm <- caret::confusionMatrix(preds_factor, Y_test_factor, positive = "1")
  acc <- cm$overall["Accuracy"]
  precision <- cm$byClass["Precision"]
  recall <- cm$byClass["Recall"]
  f1 <- cm$byClass["F1"]

  # Earliness metrics
  df.test$yhat <- preds_factor

  PAE <- df.test %>%
    filter(outcome == 1) %>% 
    group_by(game_id, play_id, nfl_id) %>%
    arrange(game_id, play_id, nfl_id, frame_id) %>%
    mutate(
      max_frame = max(frame_id),
      is_correct = yhat == outcome,
      errors_ahead = rev(cumsum(rev(!is_correct))),
      correct_from_here = errors_ahead == 0
    ) %>%
    filter(correct_from_here) %>%
    summarize(
      first_stable_frame = min(frame_id),
      max_frame = first(max_frame),
      .groups = 'drop'
    ) %>%
    mutate(earliness_score = (max_frame - first_stable_frame) / max_frame) %>%
    group_by(game_id, play_id) %>%
    summarize(
      play_avg = mean(earliness_score),
      .groups = 'drop'
    ) %>%
    summarize(overall_average = mean(play_avg, na.rm = TRUE)) %>%
    pull(overall_average)


  NAE <- df.test %>%
    filter(outcome == 0) %>% 
    group_by(game_id, play_id, nfl_id) %>%
    arrange(game_id, play_id, nfl_id, frame_id) %>%
    mutate(
      max_frame = max(frame_id),
      is_correct = yhat == outcome,
      errors_ahead = rev(cumsum(rev(!is_correct))),
      correct_from_here = errors_ahead == 0
    ) %>%
    filter(correct_from_here) %>%
    summarize(
      first_stable_frame = min(frame_id),
      max_frame = first(max_frame),
      .groups = 'drop'
    ) %>%
    mutate(earliness_score = (max_frame - first_stable_frame) / max_frame) %>%
    group_by(game_id, play_id) %>%
    summarize(
      play_avg = mean(earliness_score),
      .groups = 'drop'
    ) %>%
    summarize(overall_average = mean(play_avg, na.rm = TRUE)) %>%
    pull(overall_average)
  
  c(Accuracy = acc,
    Precision = precision,
    Recall = recall,
    F1 = f1,
    PAE = PAE,
    NAE = NAE,
    AE = (PAE + NAE)/2)
}

# To finetune models based on test set
finetune <- function(metric,
                     df.train,
                     df.test,
                     features,
                     outcome,
                     param_grid,
                     results_save_file,
                     model_save_file) {

  # Get training data
  results <- list()
  best_metric <- 0
  X.train <- df.train[, features]
  Y.train <- df.train[[outcome]]

  if (!file.exists(model_save_file) | !file.exists(results_save_file)) {

    message('Evaluating hyper-parameters, this may take a while')
    total_iterations <- length(seq_along(param_grid))
    pb <- progress_bar$new(
      format = " Processing [:bar] :percent eta: :eta",
      total = total_iterations,
      clear = FALSE,
      width = 60
    )
    for (i in seq_along(param_grid)) {
      pb$tick()
      # Fit and evaluate model for each set of params
      params <- param_grid[[i]]
      nrounds = params$nrounds
      params_mod <- params
      params_mod$nrounds <- NULL
      model <- fit_xgb_binary_classifier(nrounds, X.train, Y.train, params_mod)
      metrics <- evaluate_model(model, df.test, features, outcome)
      metric_val <- metrics[[metric]]
      if (metric_val > best_metric){
        best_model <- model
      }
      results[[i]] <- list(params = params, metrics = metrics)

      saveRDS(results, file = results_save_file)
      
    }

  }
  else {
    results <- readRDS(results_save_file)
  }

  # Find best set of params based on metric
  metric_values <- sapply(results, function(r) r$metrics[[metric]])
  best_idx <- which.max(metric_values)
  best_params <- results[[best_idx]]$params
  
  # save best model
  if (!file.exists(model_save_file)){
    saveRDS(best_model, model_save_file)
    }
  
  list(
   # best_model = best_model,
    best_params = best_params,
    best_metrics = results[[best_idx]]$avg_metrics,
    all_results = results
  )
}
