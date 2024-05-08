library(tidyverse)

url = "https://raw.githubusercontent.com/fivethirtyeight/data/master/nba-raptor/historical_RAPTOR_by_team.csv"
raptor = read_csv(url)

raptor = raptor |>
  select(player_name, team, season, season_type, mp, raptor_offense:raptor_total)

write_csv(x = raptor, file = "data/raptor")

