#### Generate sentiment data

us_state_coordinates <- read.csv("us-states.csv") 
us_county_coordinates <- readRDS("us-counties.rds") 
world_country_coordinates <- read.csv("world-countries.csv") 
jp_prefecture_coordinates <- read.csv("jp-prefectures.csv") 
jp_prefecture_name_id_map <- read.csv("jp-prefecture-name-id-map.csv")

usethis::use_data(
  us_state_coordinates,
  us_county_coordinates,
  world_country_coordinates,
  jp_prefecture_coordinates,
  jp_prefecture_name_id_map,
  overwrite = TRUE)
