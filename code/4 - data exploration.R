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
library(corrplot)

## read in clean shot data from '3- xG with rankings' file line 385
clean_shot_data
names(clean_shot_data)

##### correlation plot ##### 

clean_shot_data_locations = clean_shot_data %>%
  select(c("ball_pos_x", "ball_pos_y", "ball_pos_z", 
           "shot_taker_pos_x", "shot_taker_pos_y", "shot_taker_pos_z",
           "team_mate_pos_x", "team_mate_pos_y", "team_mate_pos_z",
           "opp_1_pos_x", "opp_1_pos_y", "opp_1_pos_z", "opp_2_pos_x",
           "opp_2_pos_y", "opp_2_pos_z"))

corrplot(cor(clean_shot_data_locations),        # Correlation matrix
         method = "shade", # Correlation plot method
         type = "upper",    # Correlation plot style (also "upper" and "lower")
         diag = TRUE,      # If TRUE (default), adds the diagonal
         tl.col = "black", # Labels color
         bg = "white",     # Background color
         col = NULL)       # Color palette

clean_shot_data_num = clean_shot_data %>%
  select_if(is.numeric)
names(clean_shot_data_num)

######################## Response-Feature Relationships ########################

## to prevent selection bias, use training set to explore response-feature

shot_train_xg ## '3- xG with rankings' file line 394
names(shot_train_xg)

# Goal vs. distanceToGoal
goal_distance = shot_train_xg %>%
  select(logDistanceToGoal, goal) %>%
  ggplot(aes(x = goal, y = logDistanceToGoal, group = goal)) +
  geom_boxplot() +
  labs(x = "Goal",
       y = "(Log) Distance to Goal") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

# Goal vs. distance to teammate
goal_distance_teammate = shot_train_xg %>%
  select(distanceToTeam, goal) %>%
  ggplot(aes(x = goal, y = distanceToTeam, group = goal)) +
  geom_boxplot() +
  labs(x = "Goal",
       y = "Distance to Teammate") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

# Goal vs. distance to opp
goal_distance_opp1 = shot_train_xg %>%
  select(distanceToOpp1, goal) %>%
  ggplot(aes(x = goal, y = distanceToOpp1, group = goal)) +
  geom_boxplot() +
  labs(x = "Goal",
       y = "Distance to Opponent 1") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

goal_distance_opp2 = shot_train_xg %>%
  select(distanceToOpp2, goal) %>%
  ggplot(aes(x = goal, y = distanceToOpp2, group = goal)) +
  geom_boxplot() +
  labs(x = "Goal",
       y = "Distance to Opponent 2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

p_all = plot_grid(goal_distance, goal_distance_teammate, 
                          goal_distance_opp1, goal_distance_opp2,
                          nrow = 2)
ggsave(filename = "results/box-distances.png", 
       plot = p_all, 
       device = "png", 
       width = 5, 
       height = 5)



