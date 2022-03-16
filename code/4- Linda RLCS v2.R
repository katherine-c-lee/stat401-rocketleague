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

######  Linda #########
setwd('/Users/lindawang/Desktop/Final Project Python Code/outputs')

clean_csv <- function(csv_path) {
  shot_data <- read.csv(file = csv_path)
  
  clean_shot_data <- shot_data %>%
    select(-c(frame)) %>%
    select(-contains("throttle"), -contains("name"), -contains("_dodge"),
           -contains("steer"), -contains("ping"), -contains("cam"),
           -contains("handbrake"), -contains("boost"),
           -contains("jump")) %>%
    add_column(shot_taker_boost_active = shot_data$shot_taker_boost_active) %>%
    na.omit()
  
  clean_shot_data$goal <- as.logical(clean_shot_data$goal) %>%
    as.numeric()
  
  clean_shot_data$logDistanceToGoal <- log(clean_shot_data$distanceToGoal)
  
  clean_shot_data <- clean_shot_data %>%
    add_column(distanceToOpp1 = ((.$shot_taker_pos_x - .$opp_1_pos_x)^2 
                                 + (.$shot_taker_pos_y - .$opp_1_pos_y)^2 
                                 + (.$shot_taker_pos_z - .$opp_1_pos_z)^2) ^(1/2))
  
  clean_shot_data <- clean_shot_data %>%
    add_column(distanceToOpp2 = ((.$shot_taker_pos_x - .$opp_2_pos_x)^2 
                                 + (.$shot_taker_pos_y - .$opp_2_pos_y)^2 
                                 + (.$shot_taker_pos_z - .$opp_2_pos_z)^2) ^(1/2))
  
  clean_shot_data <- clean_shot_data %>%
    add_column(distanceToTeam = ((.$shot_taker_pos_x - .$team_mate_pos_x)^2
                                 + (.$shot_taker_pos_y - .$team_mate_pos_y)^2
                                 + (.$shot_taker_pos_z - .$team_mate_pos_z)^2) ^(1/2))
  # cos theta
  # create vectors between ball and opposition players
  clean_shot_data <- clean_shot_data %>%
    add_column(vector_opp_1_x = (.$opp_1_pos_x - .$ball_pos_x))
  
  clean_shot_data <- clean_shot_data %>%
    add_column(vector_opp_1_y = (.$opp_1_pos_y - .$ball_pos_y))
  
  clean_shot_data <- clean_shot_data %>%
    add_column(vector_opp_1_z = (.$opp_1_pos_z - .$ball_pos_z))
  
  clean_shot_data <- clean_shot_data %>%
    add_column(vector_opp_2_x = (.$opp_2_pos_x - .$ball_pos_x))
  
  clean_shot_data <- clean_shot_data %>%
    add_column(vector_opp_2_y = (.$opp_2_pos_y - .$ball_pos_y))
  
  clean_shot_data <- clean_shot_data %>%
    add_column(vector_opp_2_z = (.$opp_2_pos_z - .$ball_pos_z))
  
  # calculate dot products of position and velocity vectors 
  clean_shot_data <- clean_shot_data %>%
    add_column(dotproduct_opp_1 = ((.$vector_opp_1_x * .$ball_vel_x)
                                   + (.$vector_opp_1_y * .$ball_vel_y)
                                   + (.$vector_opp_1_z * .$ball_vel_z)))
  
  clean_shot_data <- clean_shot_data %>%
    add_column(dotproduct_opp_2 = ((.$vector_opp_2_x * .$ball_vel_x)
                                   + (.$vector_opp_2_y * .$ball_vel_y)
                                   + (.$vector_opp_2_z * .$ball_vel_z)))
  
  # calculate magnitudes of the vectors
  clean_shot_data <- clean_shot_data %>%
    add_column(magnitude_vel = ((.$ball_vel_x * .$ball_vel_x)^2
                                + (.$ball_vel_y * .$ball_vel_y)^2
                                + (.$ball_vel_z * .$ball_vel_z)^2) ^(1/2))
  
  clean_shot_data <- clean_shot_data %>%
    add_column(magnitude_opp_1 = ((.$vector_opp_1_x * .$vector_opp_1_x)^2
                                  + (.$vector_opp_1_y * .$vector_opp_1_y)^2
                                  + (.$vector_opp_1_z * .$vector_opp_1_z)^2) ^(1/2))
  
  clean_shot_data <- clean_shot_data %>%
    add_column(magnitude_opp_2 = ((.$vector_opp_2_x * .$vector_opp_2_x)^2
                                  + (.$vector_opp_2_y * .$vector_opp_2_y)^2
                                  + (.$vector_opp_2_z * .$vector_opp_2_z)^2) ^(1/2))
  
  # actual cos theta
  clean_shot_data <- clean_shot_data %>%
    add_column(cos_theta_opp_1 = .$dotproduct_opp_1 / (.$magnitude_opp_1 * .$magnitude_vel))
  
  clean_shot_data <- clean_shot_data %>%
    add_column(cos_theta_opp_2 = .$dotproduct_opp_2 / (.$magnitude_opp_2 * .$magnitude_vel))
  
  # remove the helper columns (vector, dotproduct, magnitude)
  clean_shot_data <- clean_shot_data %>%
    select(-contains("vector"), -contains("dotproduct"), -contains("magnitude")) %>%
    na.omit()
  
  return(clean_shot_data)
}


#################################### xgBoost ################################### 
apply_xg <- function(clean_shot_data, xg_model) {
  gbm_pred = predict(xg_model, n.trees = 200,
                     type = "response", newdata = clean_shot_data)
  gbm_pred_class <- ifelse(gbm_pred > 0.5, 1, 0)
  
  gbm_misclass = mean(gbm_pred_class != clean_shot_data$goal)
  print(paste("Misclassification rate: ",gbm_misclass))
  
  gbm_logloss <- LogLoss(y_pred = gbm_pred, y_true = clean_shot_data$goal)
  print(paste("Log Loss: ",gbm_logloss))
  
  data_with_boost_xg = cbind(clean_shot_data, gbm_pred) %>%
    rename(xg = gbm_pred) %>%
    as_tibble()
  
  return(data_with_boost_xg)
}

clean_apply_xg <- function(csv_path, xg_model) {
  xg_df <- apply_xg(clean_csv(csv_path), xg_model)
}

############################# Testing Functions #####################
europe_clean <- clean_csv('europe_1.csv')
europe_xg <- apply_xg(europe_clean, gbm_fit)
europe_xg <- clean_apply_xg('europe_1.csv', gbm_fit)

############################ Main Body ##############################
europe_1_xg <- clean_apply_xg('europe_1.csv', gbm_fit)
europe_2_xg <- clean_apply_xg('europe_2.csv', gbm_fit)
europe_3_xg <- clean_apply_xg('europe_3.csv', gbm_fit)
na_1_xg <- clean_apply_xg('na_1.csv', gbm_fit)
na_2_xg <- clean_apply_xg('na_2.csv', gbm_fit)
na_3_xg <- clean_apply_xg('na_3.csv', gbm_fit)

europe_1_shots <- read.csv('europe_1_actual_shots.csv')
europe_2_shots <- read.csv('europe_2_actual_shots.csv')
europe_3_shots <- read.csv('europe_3_actual_shots.csv')
na_1_shots <- read.csv('na_1_actual_shots.csv')
na_2_shots <- read.csv('na_2_actual_shots.csv')
na_3_shots <- read.csv('na_3_actual_shots.csv')

######################## Game-Level Analysis ########################
## note that our method doesn't count goals that happen after gametime
## is over
## e.g. in the europe 1 game, stake hit the ball, gametime finished, ball 
## went in
## note that a huge limitation of our analysis is that 
## sometimes the package does not accurately classify shot vs dribbling

analyze_game <- function(xg_df, shots_df) {
  xg_df %>% 
    group_by(shot_taker) %>%
    summarise(sumXG = sum(xg),
              sumGoals = sum(goal)) %>% 
    right_join(shots_df, by = c('shot_taker' = 'playerName')) %>% 
    mutate(goalRate = sumGoals / actualShots) %>% 
    mutate(avgXG = sumXG / actualShots)
}


analyze_game(europe_1_xg, europe_1_shots)
analyze_game(europe_2_xg, europe_2_shots)
analyze_game(europe_3_xg, europe_3_shots)
analyze_game(na_1_xg, na_1_shots)
analyze_game(na_2_xg, na_2_shots)
analyze_game(na_3_xg, na_3_shots)

######################## Player-Level Analysis ########################
## note that our method doesn't count goals that happen after gametime
## is over
## e.g. in the europe 1 game, stake hit the ball, gametime finished, ball 
## went in
## note that a huge limitation of our analysis is that 
## sometimes the package does not accurately classify shot vs dribbling

analyze_player <- function(xg_df, shots_df) {
  xg_df %>% 
    group_by(shot_taker) %>%
    summarise(sumXG = sum(xg),
              sumGoals = sum(goal)) %>% 
    right_join(shots_df, by = c('shot_taker' = 'playerName')) %>% 
    arrange(by_group = shot_taker) %>% 
    replace_na(list('sumXG' = 0, 'sumGoals' = 0))
}

a <- analyze_player(europe_1_xg, europe_1_shots)
b <- analyze_player(europe_2_xg, europe_2_shots)
c <- analyze_player(europe_3_xg, europe_3_shots)
d <- (a[,2:4] + b[,2:4] + c[,2:4])
europe_results <- cbind(a[,1], d)
europe_results <- europe_results %>% 
  mutate(goalRate = sumGoals / actualShots,
         avgXG = sumXG / actualShots,
         sumXG = round(sumXG, 3),
         goalRate = round(goalRate, 3),
         avgXG = round(avgXG, 3))

a <- analyze_player(na_1_xg, na_1_shots)
b <- analyze_player(na_2_xg, na_2_shots)
c <- analyze_player(na_3_xg, na_3_shots)
d <- (a[,2:4] + b[,2:4] + c[,2:4])
na_results <- cbind(a[,1], d)
na_results <- na_results %>% 
  mutate(goalRate = sumGoals / actualShots,
         avgXG = sumXG / actualShots,
         sumXG = round(sumXG, 3),
         goalRate = round(goalRate, 3),
         avgXG = round(avgXG, 3))

europe_results
na_results
