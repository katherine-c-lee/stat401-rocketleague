# load libraries
library(lubridate)
library(tidyverse)

# load raw game data
game_data_raw = read.csv(file = "data/raw/data_raw.csv")

# read in from comp
game_data_raw <- data

# clean game data
game_data <- game_data_raw %>%
  as_tibble() %>%
  rename('idx' = 'Unnamed..0') %>%
  select(-c("X"))

game_data$goal <- as.logical(game_data$goal) %>%
  as.numeric()

game_data$shot <- as.logical(game_data$shot)
game_data$is_orange <- as.logical(game_data$is_orange)

head(game_data)

# write cleaned data to file
write.csv(game_data, file = "data/clean/game_data.csv")
