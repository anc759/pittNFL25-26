library(dotenv)
library(dplyr)
library(scales)
library(readr)
library(cowplot)
library(gganimate)
library(magick)

source('nfl_utils.R')

# Load in full dataset
output_url <- Sys.getenv("OUTPUT_URL")
input_url <- Sys.getenv("INPUT_URL")
output <- read_csv(output_url)
input <- read_csv(input_url)
input <- input %>% select(-starts_with("..."))
output <- output %>% select(-starts_with("..."))
input_filtered <- input %>%
  select('game_id', 'play_id', 'nfl_id', 'week', 'play_direction', 'absolute_yardline_number',
         'player_name', 'player_side', 'player_role', 'num_frames_output',
         'ball_land_x','ball_land_y') %>%
  distinct()
output_mod <- output %>%
  left_join(input_filtered, by = c("game_id", "play_id", "nfl_id", "week"))
input_mod <- input
input_mod$output <- FALSE
output_mod$output <- TRUE
df <- input_mod %>% bind_rows(output_mod)


# Load probabilies and add to dataframe
probs <- read.csv('data/probs.csv')
df <- df %>%
  left_join(probs %>%
              select(game_id, play_id, nfl_id, frame_id, prob),
            by = c("game_id", "play_id", "nfl_id", "frame_id"))


# Generate gif to gifs directory
generate_gif_of_probs_and_play(df, 2023112600, 903)
