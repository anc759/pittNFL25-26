######################
# The following code is used to load the input and output data, combine them, and add the features used for modeling.
######################

library(dotenv)
library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)
library(data.table)

source("nfl_utils.R")

load_dot_env()
meta_url <- Sys.getenv("META_URL")
output_url <- Sys.getenv("OUTPUT_URL")
input_url <- Sys.getenv("INPUT_URL")

# Download data
meta <- read_csv(meta_url)
output <- read_csv(output_url)
input <- read_csv(input_url)

input <- input %>% select(-starts_with("..."))
output <- output %>% select(-starts_with("..."))

# For testing purposes
#input <- input %>% filter(week == 1)
#output <- output %>% filter(week == 1)

# Merge dataframes
# ----------------------------------

# Filter input
input_filtered <- input %>%
  select('game_id', 'play_id', 'nfl_id', 'week', 'play_direction', 'absolute_yardline_number',
         'player_name', 'player_side', 'player_role', 'num_frames_output',
         'ball_land_x','ball_land_y') %>%
  distinct()

# Add missing features to output rows
output_mod <- output %>%
  left_join(input_filtered, by = c("game_id", "play_id", "nfl_id", "week"))

# Combine input and output data
input_mod <- input
input_mod$output <- FALSE
output_mod$output <- TRUE
df <- input_mod %>% bind_rows(output_mod)

# Extra code commented out is for testing feature creation; only focus on one play
#df <- df[df$game_id == 2023111203 & df$play_id == 2160, ]

# Estimate missing speed, acceleration, and direction for output data
df <- speed_acceleration_direction(df)

#plot(df[df$nfl_id == 53494, ]$frame_id, df[df$nfl_id == 53494, ]$s)
#plot(df[df$nfl_id == 53494, ]$frame_id, df[df$nfl_id == 53494, ]$dir)
#plot(df[df$nfl_id == 53494, ]$frame_id, df[df$nfl_id == 53494, ]$a)

# Add relative ball landing and tr features
# -------------------------------------------

# Get targeted reciever locations
target_locs <- df %>%
  filter(player_role == "Targeted Receiver") %>%
  select(game_id, play_id, frame_id, target_x = x, target_y = y, target_s = s, target_dir = dir) %>%
  distinct()

# Get euclidean distance to ball landing and targeted reciever for each frame
df <- df %>%
  left_join(target_locs, by = c("game_id", "play_id", "frame_id")) %>%
  mutate(
    distToTR = sqrt((x - target_x)^2 + (y - target_y)^2),
    distFromBallLand = sqrt((x - ball_land_x)^2 + (y - ball_land_y)^2)
  )

#plot(df[df$nfl_id == 53494, ]$frame_id, df[df$nfl_id == 53494, ]$distToTR)
#plot(df[df$nfl_id == 53494, ]$frame_id, df[df$nfl_id == 53494, ]$distFromBallLand)

# Add closing speeds to ball landing and targeted reciever
df <- df %>%
  rowwise() %>%
  mutate(
    closing_speed_ball = compute_closing_speed(x,y,s,dir,ball_land_x, ball_land_y, 0, 0),
    closing_speed_tr = compute_closing_speed(x,y,s,dir, target_x, target_y, target_s, target_dir)
  ) %>%
  ungroup()

#plot(df[df$nfl_id == 53494, ]$frame_id, df[df$nfl_id == 53494, ]$closing_speed_ball)
#plot(df[df$nfl_id == 53494, ]$frame_id, df[df$nfl_id == 53494, ]$closing_speed_tr)

# Add direction relative to ball landing and targeted reciever
df$dir_ball <- process_dir(df)

df$dir_tr <- process_dir_tr(df)

#plot(df[df$nfl_id == 53494, ]$frame_id, df[df$nfl_id == 53494, ]$dir_ball)
#plot(df[df$nfl_id == 53494, ]$frame_id, df[df$nfl_id == 53494, ]$dir_tr)

# Add features based on number of frames; frames_until_throw, frames_until_ball_land
df <- df %>%
  group_by(game_id, play_id) %>%
  mutate(
    max_input_frame  = max(frame_id[output == FALSE]),
    max_output_frame = max(frame_id[output == TRUE]),
    frames_until_throw = ifelse(
      output == FALSE,
      max_input_frame - frame_id,
      0
    ),
    frames_until_ball_land = max_output_frame - frame_id)  %>%
  ungroup() %>%
  select(-max_input_frame, -max_output_frame)

#plot(df[df$nfl_id == 53494, ]$frame_id, df[df$nfl_id == 53494, ]$frames_until_throw)
#plot(df[df$nfl_id == 53494, ]$frame_id, df[df$nfl_id == 53494, ]$frames_until_ball_land)


# Add forward Voronoi for targeted recievers
df <- df %>%
  left_join(
    compute_tr_voronoi(df) %>%
      select(game_id, play_id, frame_id, voronoi_area_tr = voronoi_area),
    by = c("game_id", "play_id", "frame_id"))

#plot(df[df$nfl_id == 53494, ]$frame_id, df[df$nfl_id == 53494, ]$voronoi_area_tr)


# Add team influence on ball landing location and targeted reciever
df <- df %>%
  left_join(
    compute_team_influence_on_ball_land(df) %>%
      select(game_id, play_id, frame_id, ball_net_influence),
    by = c("game_id", "play_id", "frame_id"))

df <- df %>%
  left_join(
    compute_team_influence_on_tr(df) %>%
      select(game_id, play_id, frame_id, tr_net_influence),
    by = c("game_id", "play_id", "frame_id"))

#plot(result$frame_id, result$Defense)
#plot(result$frame_id, result$Offense)

# Add blocker influence on defenders
df <- df %>%
  left_join(
    compute_blocker_influence_on_defenders(df) %>%
      select(game_id, play_id, frame_id,nfl_id = defender_nfl_id,blocker_influence),
    by = c("game_id", "play_id", "frame_id", "nfl_id"))


# Get only data for defenders that show up in output data
true_defender_ids <- df %>%
    filter(
      player_side == 'Defense',
      output == TRUE
    ) %>%
    distinct(game_id, play_id, nfl_id)
save_data <- df %>%
  semi_join(true_defender_ids, by = c("game_id", "play_id", "nfl_id"))

# NA Blocker influence should be replaced with 0
save_data[is.na(save_data$blocker_influence), ]$blocker_influence <- 0

# Save data for training
write.csv(save_data, "data/2023/processed_data.csv", row.names = FALSE)
