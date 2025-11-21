# Version 2: This version adds 4 variables, frames_until_throw (frames in input dataset remaining until the ball is thrown) max_output_frame (number of frames between ball thrown and end of play) distToTarget (distance to targeted receiver) final_response (min of distance to targeted receiver and distance to ball landing location)
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
      inCircleOutcome = ifelse(distFromBallLand <= 2, TRUE, FALSE), max_output_frame = frame_id
    ) %>%
    # Ungroup is important to prevent grouping issues in future joins
    ungroup() %>% 
    select(game_id, play_id, nfl_id, inCircleOutcome,max_output_frame)
  
  # 3. Update Input Data
  target_locs <- input %>%
    filter(player_role == "Targeted Receiver") %>%
    select(game_id, play_id, frame_id, target_x = x, target_y = y)
  
  # B. Process Input and Join Target Locations
  input_processed <- input %>%
    group_by(game_id, play_id) %>%
    mutate(
      distFromBallLand = sqrt((x - ball_land_x)^2 + (y - ball_land_y)^2),
      frames_until_throw = max(frame_id) - frame_id
    ) %>%
    ungroup() %>%
    # Join the target coordinates based on Game, Play, and Frame
    left_join(target_locs, by = c("game_id", "play_id", "frame_id")) %>%
    mutate(
      # Calculate Euclidean distance between player (x,y) and target (target_x, target_y)
      distToTarget = sqrt((x - target_x)^2 + (y - target_y)^2),
      final_response = pmin(distToTarget, distFromBallLand)
    )
  
  # Process orientation and direction
  input_processed$corrected_o <- process_o(input_processed)
  input_processed$corrected_dir <- process_dir(input_processed)
  
  # 4. Final Join
  # Join the clean outcomes directly to the processed input
  final <- input_processed %>%
    left_join(outcomes_clean, by = c("game_id", "play_id", "nfl_id")) %>%
    select(game_id, play_id, nfl_id, frame_id, player_name, player_side, x,y,ball_land_x, ball_land_y, s, a,
           distFromBallLand, corrected_o, corrected_dir, inCircleOutcome,frames_until_throw,max_output_frame,distToTarget,final_response)
  return(na.omit(final))
}


