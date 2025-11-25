# Version 5: Added the orientation and direction variables relative to tageted receiver.
#
# Takes in the input and output datasets and then creates the outcome variable and processes the orientation and direction variables to be relative to the ball. Only returns data for input dataset and the outcome.
#
#
#

data_preprocess <- function(input, output) {
  
  # --- STEP 0: Get Player Roles ---
  # We need to know who the "Targeted Receiver" is. 
  # We extract this mapping from the input data to use in the output data.
  role_map <- input %>%
    select(game_id, play_id, nfl_id, player_role) %>%
    distinct() # Ensures unique mapping per player per play

  # --- 1. Get Ball Landing Spot ---
  whereIsTheBallAll <- input %>%
    select(game_id, play_id, ball_land_x, ball_land_y)
  
  whereIsTheBall <- whereIsTheBallAll[!duplicated(whereIsTheBallAll), ]
  
  # --- 2. Calculate Outcome (One row per player per play) ---
  
  # A. Prepare the base outcome data (max frame) and join Roles
  outcomes_base <- output %>%
    group_by(game_id, play_id, nfl_id) %>%
    filter(frame_id == max(frame_id)) %>%
    ungroup() %>%
    # Join the roles so we know who is who in the outcome data
    left_join(role_map, by = c("game_id", "play_id", "nfl_id"))
  
  # B. Find where the Targeted Receiver is at the max frame
  target_outcome_locs <- outcomes_base %>%
    filter(player_role == "Targeted Receiver") %>%
    select(game_id, play_id, target_final_x = x, target_final_y = y)
  
  # C. Calculate Distances
  outcomes_clean <- outcomes_base %>%
    left_join(whereIsTheBall, by = c("game_id", "play_id")) %>%
    # Join the target's final location back to the main data
    left_join(target_outcome_locs, by = c("game_id", "play_id")) %>%
    mutate(
      distFromBallLand = sqrt((x - ball_land_x)^2 + (y - ball_land_y)^2),
      # NEW: Distance to Targeted Receiver at the max frame
      distToTargetFinal = sqrt((x - target_final_x)^2 + (y - target_final_y)^2),
      inCircleOutcome = ifelse(distFromBallLand <= 2, TRUE, FALSE),
      inCircleOutcome_new = ifelse(distFromBallLand <= 2 | distToTargetFinal <= 2, TRUE, FALSE), 
      max_output_frame = frame_id
    ) %>%
    select(game_id, play_id, nfl_id, inCircleOutcome, max_output_frame, inCircleOutcome_new,distToTargetFinal)
  
  # --- 3. Update Input Data ---
  target_locs <- input %>%
    filter(player_role == "Targeted Receiver") %>%
    select(game_id, play_id, frame_id, target_x = x, target_y = y)
  
  input_processed <- input %>%
    group_by(game_id, play_id) %>%
    mutate(
      distFromBallLand = sqrt((x - ball_land_x)^2 + (y - ball_land_y)^2),
      frames_until_throw = max(frame_id) - frame_id
    ) %>%
    ungroup() %>%
    left_join(target_locs, by = c("game_id", "play_id", "frame_id")) %>%
    mutate(
      distToTarget = sqrt((x - target_x)^2 + (y - target_y)^2)
      #,min_distance = pmin(distToTarget, distFromBallLand)
    )
  
  # Process orientation and direction
  input_processed$corrected_o <- process_o(input_processed)
  input_processed$corrected_dir <- process_dir(input_processed)
  
  input_processed$corrected_o_tr <- process_o_tr(input_processed)
  input_processed$corrected_dir_tr <- process_dir(input_processed)
  
  # --- 4. Final Join ---
  final <- input_processed %>%
    left_join(outcomes_clean, by = c("game_id", "play_id", "nfl_id")) %>%
    select(game_id, play_id, nfl_id, frame_id, player_name, player_side, x, y, 
           ball_land_x, ball_land_y, s, a,
           distFromBallLand, corrected_o, corrected_dir,corrected_o_tr, corrected_dir_tr, 
           inCircleOutcome, frames_until_throw, max_output_frame, 
           inCircleOutcome_new,player_role,distToTarget) # Included min_distance from step 3
  final <- final[final$player_role != "Targeted Receiver",]
  return(na.omit(final))
}
