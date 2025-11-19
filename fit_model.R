library(dotenv)
library(readr)
library(caret)
library(randomForest)
library(xgboost)
library(pROC)
library(dplyr)
library(ggplot2)
library(patchwork)

source("model_utils.R")
source("modeling.R")
source("filters.R")
source("preprocess_function.R")
source("process_o_dir.R")
source("nfl_utils.R")

load_dot_env()
meta_url <- Sys.getenv("META_URL")
output_url <- Sys.getenv("OUTPUT_URL")
input_url <- Sys.getenv("INPUT_URL")
week1_output_url <- Sys.getenv("WEEK1OUTPUT_URL")
week1_input_url <- Sys.getenv("WEEK1INPUT_URL")

# Download week 1 data
meta <- read_csv(meta_url)
week1O <- read_csv(output_url)
week1I <- read_csv(input_url)

# Filter the data
out <- filter_by_wp(meta, week1I, week1O)
out <- filter_near_end_of_half(out$meta, out$input, out$output)
out <- filter_by_side(out$meta, out$input, out$output)

input <- out$input
output <- out$output
meta <- out$meta

# Add features
input <- data_preprocess(input, output)

output <- speed_acceleration(data = output)
output <- output %>%
  left_join(
    input %>%
      select(game_id, play_id, nfl_id, player_name, player_side, ball_land_x, ball_land_y, inCircleOutcome) %>% distinct(),
    by = c("game_id", "play_id", "nfl_id")
  )
output$distFromBallLand <- sqrt((output$x - output$ball_land_x)^2 + (output$y - output$ball_land_y)^2)


# Build X,Y matrices
X.in <- input[, names(input) %in% c("s", "a", "distFromBallLand", "corrected_o", "corrected_dir")]
Y.in <- ifelse(input$inCircleOutcome, 1, 0)

X.out <- output[, names(output) %in% c("s", "a", "distFromBallLand")]
Y.out <- ifelse(output$inCircleOutcome, 1, 0)


# Fit before and after models
fit.in <- fit_binary_classifier('xgb', X.in, Y.in, nrounds = 1000, eta = 0.1, max_depth = 3)
fit.out <-  fit_binary_classifier('xgb', X.out, Y.out, nrounds = 1000, eta = 0.1, max_depth = 3)


# Generate random plot to check model
plot_random_field_and_probs(input, output, fit.in, fit.out)
