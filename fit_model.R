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
data_dir <- Sys.getenv("DATA_DIR")

# Download data
meta <- read_csv(meta_url)
week1O <- read_csv(output_url)
week1I <- read_csv(input_url)

# Filter the data
out <- filter_by_wp(meta, week1I, week1O)
out <- filter_near_end_of_half(out$meta, out$input, out$output)

input <- out$input
output <- out$output
meta <- out$meta

# Add features
input <- data_preprocess(input, output)

out2 <- filter_by_side(out$meta, input, output)

input <- out2$input
output <- out2$output
meta <- out2$meta


output <- speed_acceleration(data = output)
output <- output %>%
  left_join(
    input %>%
      select(game_id, play_id, nfl_id, player_name, player_side, ball_land_x, ball_land_y, inCircleOutcome, inCircleOutcome_new) %>% distinct(),
    by = c("game_id", "play_id", "nfl_id")
  )
output$distFromBallLand <- sqrt((output$x - output$ball_land_x)^2 + (output$y - output$ball_land_y)^2)


# Build X,Y matrices
input.features <-  c("s", "a", "distFromBallLand", "corrected_o", "corrected_dir", "distToTarget", "corrected_o_tr", "corrected_dir_tr", "frames_until_throw", "max_output_frame")
X.in <- input[, input.features]
Y.in <- ifelse(input$inCircleOutcome_new, 1, 0)

output.features <- c("s", "a", "distFromBallLand")
X.out <- output[, output.features]
Y.out <- ifelse(output$inCircleOutcome_new, 1, 0)

# Fit before and after models
fit.in <- fit_binary_classifier('xgb', X.in, Y.in, nrounds = 1000, eta = 0.1, max_depth = 3)

fit.out <-  fit_binary_classifier('xgb', X.out, Y.out, nrounds = 1000, eta = 0.1, max_depth = 3)

source("nfl_utils.R")

# Generate random plot to check model
plot_random_field_and_probs(input, output, input.features, output.features, c('inCircleOutcome_new'), fit.in, fit.out)

# Get predictions for input and output data

input$prob <- predict_model(fit.in, X.in)$prob
output$prob <- predict_model(fit.out, X.out)$prob

# Save date
data_dir <- Sys.getenv("DATA_DIR")
cols <- c('game_id', 'play_id', 'nfl_id', 'frame_id', 'prob')
write.csv(input[, cols], file.path(data_dir, 'input_probs.csv'))
write.csv(output[, cols], file.path(data_dir, 'output_probs.csv'))
