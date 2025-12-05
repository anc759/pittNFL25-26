
#Run the data (probs, the one created by Ryan) through the calculate_play_metrics and then the calculate_type_distribution

calculate_play_metrics <- function(data) {
  data %>%
    # 1. Group by the unique player-play combination
    group_by(nfl_id, game_id, play_id) %>%
    
    # 2. Ensure data is sorted by frame_id 
    arrange(frame_id, .by_group = TRUE) %>%
    
    # 3. Filter to ensure calculation is possible
    # r_diff requirement: At least 5 TRUE frames
    # a_diff requirement: At least 6 FALSE frames
    filter(sum(output == FALSE) >= 6, sum(output == TRUE) >= 5) %>%
    
    # 4. Calculate the differences
    summarise(
      r_diff = nth(prob[output == TRUE], 5) - last(prob[output == FALSE]),
      a_diff = last(prob[output == FALSE]) - nth(prob[output == FALSE], sum(output == FALSE) - 5),
      .groups = "drop" 
    ) %>%
    
    # 5. Create the Categorical 'type' variable
    # We use a threshold of 0.1. 
    # > 0.1 is Reactive (R)
    # < -0.1 is Anticipatory (A)
    # Between -0.1 and 0.1 is Mixed (M)
    mutate(
      diff_val = r_diff - a_diff,
      type = case_when(
        diff_val > 0.1 ~ "R",
        diff_val < -0.1 ~ "A", 
        TRUE ~ "M"
      )
    )
}


calculate_type_distribution <- function(data) {
  data %>%
    # Group by the player ID
    group_by(nfl_id) %>%
    
    # Calculate distribution statistics for the new categorical 'type'
    summarise(
      total_count = n(),
      
      # Counts for each category
      count_R = sum(type == "R", na.rm = TRUE),
      count_A = sum(type == "A", na.rm = TRUE),
      count_M = sum(type == "M", na.rm = TRUE),
      
      # Proportions for each category
      prop_R = mean(type == "R", na.rm = TRUE),
      prop_A = mean(type == "A", na.rm = TRUE),
      prop_M = mean(type == "M", na.rm = TRUE),
      
      # Keeping standardized stats if that variable exists in the input 
      # (Assuming it remains 0/1 numeric, otherwise these will need updating too)
      #std_count_1 = if("type_standardized" %in% names(data)) sum(type_standardized == 1, na.rm = TRUE) else NA,
      #std_prop_1 = if("type_standardized" %in% names(data)) mean(type_standardized, na.rm = TRUE) else NA,
      
      .groups = "drop"
    )
}