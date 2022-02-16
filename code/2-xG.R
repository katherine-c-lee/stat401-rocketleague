# load libraries
library(kableExtra)   # for printing tables
library(cowplot)      # for side by side plots
library(pROC)         # for ROC curves
library(tidyverse)
library("ggplot2")
library(randomForest)  # random forests
library(gbm)           # boosting
library(keras)         # to train neural networks
library(RColorBrewer)

# load clean game data
setwd("~/Dropbox (Penn)/__SPRING 2022/STAT401")
game_data <- read.csv(file = "")
game_data <- read.csv(file = "data/clean/game_data.csv")

# prep data for logistic regression with shot data
shot_data <- game_data %>%
  select(-c(frame, time, is_orange, distanceToGoal, shot_taker_id,
            opp_1_id, opp_2_id)) %>%
  select(-contains("throttle"), -contains("name"), -contains("_dodge"),
         -contains("steer"), -contains("ping"), -contains("team_mate"),
         -contains("ball"), -contains("handbrake"), -contains("boost"), 
         -contains("jump")) %>%
  na.omit()

shot_data$goal <- as.logical(shot_data$goal) %>%
  as.numeric()
shot_data$shot <- as.logical(shot_data$shot)

shot_data1 <- shot_data %>%
  filter(shot == TRUE) %>%
  select(-c(shot))

# train / test split
set.seed(5)
n1 = nrow(shot_data1)
train_samples1 = sample(1:n1, round(0.8*n1))
shot_train = shot_data1[train_samples1,]
shot_test = shot_data1[-train_samples1,]

# check class proportions -- about 35.1% of shots result in goals
shot_train %>%
  summarise(Mean = mean(shot_train$goal))
shot_data1 %>%
  summarise(Mean = mean(shot_data1$goal))

# model 1: logistic regression
glm_fit = glm(goal ~ . - idx, family = "binomial", data = shot_train)
summary(glm_fit)

glm_pred = predict(glm_fit, type = "response", newdata = shot_test)
glm_pred_class <- ifelse(glm_pred > 0.5, 1, 0)
glm_misclass = mean(glm_pred_class != shot_test$goal)
glm_misclass

# model 2: xgboost
gbm_fit_shot = gbm(goal ~ . - idx,
                   distribution = "bernoulli",
                   n.trees = 1000,
                   interaction.depth = 1,
                   shrinkage = 0.1,
                   cv.folds = 5,
                   data = shot_train)

save(gbm_fit_shot, file = "results/gbm_fit_shot.Rda")

gbm_predict_shot = predict(gbm_fit_shot, type = "response", 
                           newdata = shot_test)
gbm_pred_class_shot <- ifelse(gbm_predict_shot > 0.5, 1, 0)
gbm_misclass_shot = mean(gbm_pred_class_shot != shot_test$goal)
gbm_misclass_shot

hist(gbm_predict_shot)

# apply xgboost predictions to full dataset
gbm_predict_full_shot = predict(gbm_fit_shot, type = "response", 
                                newdata = shot_data1)

data_with_xg = cbind(shot_data1, gbm_predict_full_shot) %>%
  rename(xg = gbm_predict_full_shot)

data_with_xg %>%
  ggplot(aes(x = shot_taker_pos_x, y = shot_taker_pos_y, colour = xg)) +
  geom_point(size = 0.001) +
  geom_tile() +
  theme_bw() +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  labs(x = "Shot Taker X Position", y = "Shot Taker Y Position")

