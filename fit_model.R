library(dotenv)
library(readr)
library(caret)
library(randomForest)
library(xgboost)
library(pROC)
library(dplyr)
library(ggplot2)

source("model_utils.R")
source("modeling.R")
source("filters.R")
source("preprocess_function.R")
source("process_o_dir.R")

load_dot_env()
meta_url <- Sys.getenv("META_URL")
output_url <- Sys.getenv("OUTPUT_URL")
input_url <- Sys.getenv("INPUT_URL")
week1_output_url <- Sys.getenv("WEEK1OUTPUT_URL")
week1_input_url <- Sys.getenv("WEEK1INPUT_URL")

meta <- read_csv(meta_url)
week1O <- read_csv(week1_output_url)
week1I <- read_csv(week1_input_url)

out <- filter_by_wp(meta, week1I, week1O)
out <- filter_near_end_of_half(out$meta, out$input, out$output)
out <- filter_by_side(out$meta, out$input, out$output)


data <- data_preprocess(out$input, out$output)

head(data)
colnames(data)

X <- data[, names(data) %in% c("s", "a", "distFromBallLand", "corrected_o", "corrected_dir")]
Y <- ifelse(data$inCircleOutcome, 1, 0)


grid_xgb <- list(
  list(model_name = "xgb", nrounds = 100, eta = 0.1, max_depth = 3,
       objective = "binary:logistic", verbose = 0)
)

res_xgb <- cross_validate_tune(fit_binary_classifier, evaluate_model, X, Y, grid_xgb)


summary <- data.frame(
  Model = c("XGBoost"),
  rbind(res_xgb$best_metrics)
)

fit <- fit_binary_classifier('xgb', X, Y, nrounds = 1000, eta = 0.1, max_depth = 3)
data$


player_id <- 46137
game_id_sel <- 2023090700
play_id_sel <- 101
subset_data <- data %>%
  dplyr::filter(
    nfl_id == player_id,
    game_id == game_id_sel,
    play_id == play_id_sel
  )
X.subset <- subset_data[, names(subset_data) %in% c("s", "a", "distFromBallLand", "corrected_o", "corrected_dir")]
Y.subset <- ifelse(subset_data$inCircleOutcome, 1, 0)


preds <- predict_model(fit, X.subset)

plot_df <- data.frame(
  frame_id = subset_data$frame_id,   # time index
  pred = preds                    # predicted values
)

ggplot(plot_df, aes(x = frame_id, y = pred.prob)) +
  geom_line() +
  labs(
    title = "Predicted Values Across Time",
    x = "Frame ID",
    y = "Prediction"
  ) +
  theme_minimal()


week1I[week1I$nfl_id == 46137, ]$player_position
