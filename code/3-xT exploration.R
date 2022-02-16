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

setwd("~/Dropbox (Penn)/__SPRING 2022/STAT401")
game_data <- read.csv(file = 'combined.csv')
game_data

# ranges of the field
summary(game_data$shot_taker_pos_x)
summary(game_data$shot_taker_pos_y)
summary(game_data$shot_taker_pos_z)

summary(game_data$team_mate_pos_x)
summary(game_data$team_mate_pos_y)
summary(game_data$team_mate_pos_z)

