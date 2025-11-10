# Fitting binary classifiers
fit_binary_classifier <- function(model_name, X, Y, ...) {
  model_name <- tolower(model_name)
  
  if (model_name == "logistic") {
    fit <- glm(Y ~ ., data = data.frame(Y = Y, X), family = binomial(), ...)
    
  } else if (model_name == "rf") {
    if (!requireNamespace("randomForest", quietly = TRUE))
      stop("Please install the 'randomForest' package.")
    fit <- randomForest::randomForest(x = X, y = as.factor(Y), ...)
    
  } else if (model_name == "xgb") {
    if (!requireNamespace("xgboost", quietly = TRUE))
      stop("Please install the 'xgboost' package.")
    dtrain <- xgboost::xgb.DMatrix(data = as.matrix(X), label = Y)
    fit <- xgboost::xgb.train(data = dtrain, ...)
    
  } else stop("Unsupported model type: ", model_name)
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
evaluate_model <- function(model, X_test, Y_test) {
  if (!requireNamespace("caret", quietly = TRUE))
    stop("Please install the 'caret' package.")
  if (!requireNamespace("pROC", quietly = TRUE))
    stop("Please install the 'pROC' package.")
  
  preds_out <- predict_model(model, X_test)
  preds <- preds_out$class
  preds_prob <- preds_out$prob
  
  preds_factor <- factor(preds, levels = c(0, 1))
  Y_test_factor <- factor(Y_test, levels = c(0, 1))
  
  cm <- caret::confusionMatrix(preds_factor, Y_test_factor, positive = "1")
  acc <- cm$overall["Accuracy"]
  precision <- cm$byClass["Precision"]
  recall <- cm$byClass["Recall"]
  f1 <- cm$byClass["F1"]
  
  auc_val <- tryCatch({
    pROC::roc(response = Y_test, predictor = preds_prob, quiet = TRUE)$auc
  }, error = function(e) NA)
  
  c(Accuracy = acc, AUC = auc_val, Precision = precision, Recall = recall, F1 = f1)
}
