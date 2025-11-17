# Version 1
#
# Takes in the input and output datasets and then creates the outcome variable and processes the orientation and direction variables to be relative to the ball. Only returns data for input dataset and the outcome.
#
#

data_preprocess <- function(input, output) {
  
  # 1. Get Ball Landing Spot
  whereIsTheBallAll <- input %>%
    select(game_id, play_id, ball_land_x, ball_land_y)
  
  whereIsTheBall <- whereIsTheBallAll[!duplicated(whereIsTheBallAll), ]
  
  # 2. Calculate Outcome (One row per player per play)
  # We create 'outcomes_clean' directly here. No need to rejoin to 'output' later.
  outcomes_clean <- output %>%
    group_by(game_id, play_id, nfl_id) %>%
    filter(frame_id == max(frame_id)) %>%
    left_join(whereIsTheBall, by = c("game_id", "play_id")) %>%
    mutate(
      distFromBallLand = sqrt((x - ball_land_x)^2 + (y - ball_land_y)^2),
      inCircleOutcome = ifelse(distFromBallLand <= 2, TRUE, FALSE)
    ) %>%
    # Ungroup is important to prevent grouping issues in future joins
    ungroup() %>% 
    select(game_id, play_id, nfl_id, inCircleOutcome)
  
  # 3. Update Input Data
  input_processed <- input %>%
    mutate(
      distFromBallLand = sqrt((x - ball_land_x)^2 + (y - ball_land_y)^2)
    )
  
  # Process orientation and direction (assuming these functions exist in your environment)
  input_processed$corrected_o <- process_o(input_processed)
  input_processed$corrected_dir <- process_dir(input_processed)
  
  # 4. Final Join
  # Join the clean outcomes directly to the processed input
  final <- input_processed %>%
    left_join(outcomes_clean, by = c("game_id", "play_id", "nfl_id")) %>%
    select(game_id, play_id, nfl_id, frame_id, s, a, 
           distFromBallLand, corrected_o, corrected_dir, inCircleOutcome)
  
  return(na.omit(final))
}


