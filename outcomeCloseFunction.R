#ball land data is in INPUT data set
#final player position is in OUTPUT data set
#function starts by outputting a table for all players with final data
#play_id, game_id, nfl_id, inCircleYesNo

library(tidyverse)

inCircleYesNo <- function(inputDataset, outputDataset){
  
  whereIsTheBallAll <-
    inputDataset %>%
    select(game_id, play_id, ball_land_x, ball_land_y)
  
  whereIsTheBall <-
    whereIsTheBallAll[!duplicated(whereIsTheBallAll),]
  
  
  withOut<-
    outputDataset %>%
    group_by(game_id, play_id, nfl_id) %>%
    filter(frame_id == max(frame_id)) %>%
    left_join(whereIsTheBall, by = c("game_id", "play_id")) %>%
    mutate(distFromBallLand = sqrt((x - ball_land_x)^2 + (y - ball_land_y)^2),
           inCircleOutcome = ifelse(distFromBallLand <= 2, TRUE, FALSE)) %>%
    select(game_id, play_id, nfl_id, inCircleOutcome, distFromBallLand)
  
  out <- outputDataset %>%
    left_join(withOut, by = c("game_id", "play_id", "nfl_id"))
  
  return(out)
}

inCircleYesNo(inputDataset = week1I, outputDataset = week1O)
