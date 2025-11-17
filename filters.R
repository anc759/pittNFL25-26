library(dplyr)

# Filter based on win probability
filter_by_wp <- function(meta, input, output, lower = 0.05, upper = 0.95) {
  meta_filtered <- meta %>%
    filter(
      pre_snap_home_team_win_probability > lower &
      pre_snap_home_team_win_probability < upper
    )

  input_filtered <- input %>%
    semi_join(meta_filtered, by = c("game_id", "play_id"))

  output_filtered <- output %>%
    semi_join(meta_filtered, by = c("game_id", "play_id"))

  list(
    meta = meta_filtered,
    input = input_filtered,
    output = output_filtered
  )
}

# Remove plays that are near the end of a half
filter_near_end_of_half <- function(meta, input, output, min_time_left = 30) {
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
    filter(time_left_half > min_time_left)
  # Apply same filtering to input/output
  input_filtered <- input %>%
    semi_join(meta_filtered, by = c("game_id", "play_id"))
  output_filtered <- output %>%
    semi_join(meta_filtered, by = c("game_id", "play_id"))
  list(
    meta = meta_filtered,
    input = input_filtered,
    output = output_filtered
  )
}

filter_by_side <- function(meta, input, output, side = "Defense") {
  input_filtered <- input %>%
    dplyr::filter(player_side == side)
  keys <- input_filtered %>%
    dplyr::select(game_id, play_id, nfl_id) %>%
    dplyr::distinct()
  output_filtered <- output %>%
    dplyr::inner_join(keys, by = c("game_id", "play_id", "nfl_id"))
  list(
    meta = meta,
    input = input_filtered,
    output = output_filtered
  )
}



#nrow(meta)
#summary(meta$pre_snap_home_team_win_probability)

#out <- filter_by_wp(meta, week1I, week1O)

#summary(out$meta$pre_snap_home_team_win_probability)
#nrow(out$meta)

#out <- filter_near_end_of_half(out$meta, out$input, out$output)

#nrow(out$meta)
