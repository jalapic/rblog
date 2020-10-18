
### Soccer Tracking Data - code from Rob Hickman


#https://github.com/SkillCorner/opendata/tree/master/data/matches
#https://twitter.com/robwhickman/status/1314274943270608896

library(tidyverse)
library(jsonlite)

object_info <- fromJSON("https://raw.githubusercontent.com/SkillCorner/opendata/master/data/matches/2440/match_data.json")

players_info <- object_info %>%
  .$players %>%
  flatten() %>%
  select(name = last_name, team = team_id, object_id = trackable_object, role = player_role.name)

ball_info <-data.frame(name = "ball", team = NA, object_id = 55, role = "ball")

ref_info <- object_info %>%
  .$referees %>%
  flatten() %>%
  select(name = last_name, object_id = trackable_object) %>%
  mutate(team = NA, role = "ref")


objects <- bind_rows(players_info, ball_info, ref_info)

track_data <- fromJSON("https://raw.githubusercontent.com/SkillCorner/opendata/master/data/matches/2440/structured_data.json") %>%
  flatten() %>%
  filter(!is.na(time)) %>%
  filter(lengths(data) > 0) %>%
  tidyr::separate(time, into = c("mins", "secs"), sep = ":") %>%
  mutate(time = as.numeric(mins) * 60 + as.numeric(secs)) %>%
  tidyr::unnest(data) %>%
  select(frame, period, time, object_id = trackable_object, x, y, z) %>%
  left_join(objects, by = "object_id")

tail(track_data)
range(track_data$time)/60

track_data[track_data$name=="ball",]
