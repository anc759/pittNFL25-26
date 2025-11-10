library(caret)
library(randomForest)
library(xgboost)
library(pROC)

source("model_utils.R")

source("modeling.R")

# use iris dataset for testing
data(iris)
iris_bin <- iris[iris$Species != "setosa", ]
iris_bin$Species <- droplevels(iris_bin$Species)
X <- iris_bin[, -5]
Y <- ifelse(iris_bin$Species == "virginica", 1, 0)

# Define grids
grid_logistic <- list(list(model_name = "logistic"))
grid_rf <- list(
  list(model_name = "rf", ntree = 100, mtry = 2),
  list(model_name = "rf", ntree = 300, mtry = 3)
)
grid_xgb <- list(
  list(model_name = "xgb", nrounds = 100, eta = 0.1, max_depth = 3,
       objective = "binary:logistic", verbose = 0)
)

# Run CV
res_rf <- cross_validate_tune(fit_binary_classifier, evaluate_model, X, Y, grid_rf)

res_log <- cross_validate_tune(fit_binary_classifier, evaluate_model, X, Y, grid_logistic)

res_xgb <- cross_validate_tune(fit_binary_classifier, evaluate_model, X, Y, grid_xgb)

# Compare
summary <- data.frame(
  Model = c("Logistic", "Random Forest", "XGBoost"),
  rbind(res_log$best_metrics, res_rf$best_metrics, res_xgb$best_metrics)
)

summary
