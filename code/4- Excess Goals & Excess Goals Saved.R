# load libraries
library(kableExtra)   # for printing tables
library(cowplot)      # for side by side plots
library(pROC)         # for ROC curves
library(tidyverse)
library("ggplot2")
library(randomForest)  # random forests
library(gbm)           # boosting
library(RColorBrewer)
library(ROCR)
library(dplyr)


############################ Load data from file 4 ###########################
setwd('/Users/lindawang/Library/Mobile Documents/com~apple~CloudDocs/Linda Wang/Spring 2022 Classes/STAT 401/Final Project/stat401-rocketleague/code')

data_with_boost_xg <- read.csv(file = "data/clean/data_with_boost_xg.csv")
shot_data <- read.csv(file = "data/clean/shot_data.csv")

################################# Excess Goals  #################################
# this section is basically katie's code in the 3- file
# made very minor modifications
# using xgboost model with teammate data and engineered features as final model
names(data_with_boost_xg)

# sum all xgs for expected goals
total_xg <- data_with_boost_xg %>%
  group_by(shot_taker_id) %>%
  summarise(sum_xg = sum(xg))

total_goals <- data_with_boost_xg %>%
  group_by(shot_taker_id) %>%
  summarise(total_goals = sum(goal))

total_attempts <- data_with_boost_xg %>%
  group_by(shot_taker_id) %>%
  summarise(count = n())

aggregate <- total_xg %>%
  inner_join(total_goals, by = "shot_taker_id") %>%
  inner_join(total_attempts, by = "shot_taker_id") %>%
  mutate(outperformance = total_goals/(sum_xg)) %>%
  mutate(actual_goal_rate = total_goals/count) %>%
  mutate(avg_xg = sum_xg/count) %>%
  arrange(desc(outperformance)) %>%
  filter(count>20)

# top 5 players in terms of excess goals
head(aggregate, 5)

# top 5 players in terms of total shots taken
aggregate %>%
  arrange(desc(count)) %>%
  head(5)   

## there is some overlap b/w the top 5 players by shots taken and top 5 by excess goals

## this result could suggest that the game is really more based on luck and not really skill?

# top average xg players
aggregate %>%
  arrange(desc(avg_xg)) %>%
  head(5)

## shot taker id 76561198124326808 seems to be good at positioning and takes lots of shots
  
# filter data for top players
data_with_boost_xg %>%
  as_tibble() %>%
  filter(shot_taker_id == "76561198028093603")

################################# Excess Goals Saved #################################
## inner join with shot_data to grab opponent IDs
shot_data_ids <- shot_data %>%
  select(contains("id"), "ball_pos_x", "ball_pos_y", "ball_pos_z","goal",contains("arc_cos")) %>%
  select(-c("idx"))

boost_data_ids <- data_with_boost_xg %>%
  inner_join(shot_data_ids, by = c("shot_taker_id","ball_pos_x", "ball_pos_z","goal",))

nrow(boost_data_ids) - nrow(data_with_boost_xg) # deal with this later

xg_saved_1 <- boost_data_ids %>%
  rename("saver_id" = "opp_1_id") %>%
  mutate(defended_xg = xg - goal) %>%
  group_by(saver_id) %>%
  summarise(total_defended = sum(defended_xg),
            count = n()) %>%
  ungroup() %>%
  select(c("saver_id","total_defended","count"))

xg_saved_2 <- boost_data_ids %>%
  rename("saver_id" = "opp_2_id") %>%
  mutate(defended_xg = xg - goal) %>%
  group_by(saver_id) %>%
  summarise(total_defended = sum(defended_xg),
            count = n()) %>%
  ungroup() %>%
  select(c("saver_id","total_defended","count"))

unique_ids <- data_with_boost_xg %>%
  group_by(data_with_boost_xg$shot_taker_id) %>%
  summarise(count = n()) %>%
  filter(count>20) %>%
  rename("saver_id" = "data_with_boost_xg$shot_taker_id")

xg_saved <- unique_ids %>%
  inner_join(xg_saved_1, by = "saver_id") %>%
  inner_join(xg_saved_2, by = "saver_id") %>%
  mutate(total_defended = total_defended.x + total_defended.x) %>%
  mutate(total_count = count.x + count.y) %>%
  mutate(excess_goals_saved = total_defended / total_count) %>%
  select(c("saver_id", "excess_goals_saved"))


