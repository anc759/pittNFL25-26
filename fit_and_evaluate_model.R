###########################
# The following code is for constructing the main outcome then fitting and evaluating models on the processed data.
###########################

library(dotenv)
library(readr)
library(caret)
library(randomForest)
library(xgboost)
library(pROC)
library(dplyr)
library(ggplot2)
library(patchwork)
library(progress)

source("nfl_utils.R")
source("model_utils.R")


#########################
# Load in data and construct outcome
#########################

# Load in pre-processed data
df_tracks <- read.csv("data/2023/processed_data.csv")

# Build outcome
outcome_df <- df_tracks %>%
  group_by(game_id, play_id, nfl_id) %>%
  filter(frame_id == max(frame_id)) %>%
  ungroup() %>%
  mutate(
    outcome = 
      (
        sqrt((x - ball_land_x)^2 + (y - ball_land_y)^2) <= 2 |
        sqrt((x - target_x)^2 + (y - target_y)^2) <= 2
      )
  )
df_tracks <- df_tracks %>%
  left_join(outcome_df %>%
              select(game_id, play_id, nfl_id, outcome),
            by = c("game_id", "play_id", "nfl_id"))

###########################
# Filter plays
###########################

# Load metadata
load_dot_env()
meta_url <- Sys.getenv("META_URL")
meta <- read_csv(meta_url)

# Filter out plays with low win probability
meta_filtered <- meta %>%
    filter(
      pre_snap_home_team_win_probability > 0.05 &
      pre_snap_home_team_win_probability < 0.95
    )
df_tracks <- df_tracks %>%
    semi_join(meta_filtered, by = c("game_id", "play_id"))

# Filter out play near end of half
meta_with_time <- meta %>%
    mutate(
      time_left_qtr = as.numeric(substr(game_clock, 1, 2)) * 60 +
                      as.numeric(substr(game_clock, 4, 5)),
      time_left_half = case_when(
        quarter %in% c(1, 3) ~ time_left_qtr + 900,
        TRUE ~ time_left_qtr
      )
    )
meta_filtered <- meta_with_time %>%
  filter(time_left_half > 30)
df_tracks <- df_tracks %>%
  semi_join(meta_filtered, by = c("game_id", "play_id"))

# Only consider S, FS, CB, and SS positions
positions <- c("CB", "FS", "SS", "S")
df_tracks <- df_tracks %>% filter(player_position %in% positions)


###############################
# Fit and evaluate XGBoost models
###############################

# Get training and testing data, define features
df_tracks$outcome <- as.numeric(df_tracks$outcome)
df.train <- df_tracks %>%
  filter(week != 18)
df.test <- df_tracks %>%
  filter(week == 18)
features <- c("distToTR", "distFromBallLand",
              "closing_speed_ball", "closing_speed_tr",
              "dir_ball", "dir_tr",
              "frames_until_throw", "frames_until_ball_land",
              "voronoi_area_tr", "ball_net_influence",
              "tr_net_influence", "blocker_influence")
outcome <- "outcome"


# Define parameter grid for xgb search
param_grid <- list(
  nrounds = c(300, 600, 1000),
  eta = c(0.05, 0.1, 0.5),
  max_depth = c(3, 5, 7),
  min_child_weight = c(1, 5),
  subsample = c(0.5, 0.7, 1.0),
  colsample_bytree = c(0.5, 0.7, 1.0)
)
#grid_list <- apply(
#  expand.grid(param_grid, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE),
#  1,
#  as.list
#)

# For fast testing
grid_list <- list(
  list(nrounds = 100,
       eta = 0.1,
       max_depth = 3),
   list(nrounds = 50,
       eta = 0.1,
       max_depth = 3)
)

results <- finetune("AE",
                    df.train,
                    df.test,
                    features,
                    outcome,
                    grid_list,
                    results_save_file = 'data/grid_search_results.rds',
                    model_save_file = 'data/best_model.rds'
                    )


# Print best params and load model
best_params <- results$best_params
print(best_params)
fit <- readRDS('data/best_model.rds')

# Print eval metrics on train and test data
print(evaluate_model(fit, df.train, features, outcome))
print(evaluate_model(fit, df.test, features, outcome))

# Print importance values
print(xgb.importance(model = fit))

# Save data
X.full <- df_tracks[, features]
df_tracks$prob <- predict_model(fit, X.full)$prob
to_save <- df_tracks %>%
  select(game_id, play_id, nfl_id, frame_id, prob, inBallTRCircle = outcome, output)
write.csv(to_save, 'data/probs.csv')


