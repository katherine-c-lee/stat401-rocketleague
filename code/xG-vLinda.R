# xG attempt vLinda

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

###### Load data ########
# setwd('/Users/lindawang/Library/Mobile Documents/com~apple~CloudDocs/Linda Wang/Spring 2022 Classes/STAT 401/Final Project/stat401-rocketleague/code')
setwd("~/Dropbox (Penn)/__SPRING 2022/STAT401/stat401-rocketleague")

game_data <- read.csv(file = "data/clean/game_data.csv")

# filter out data so it's just shots
shot_data <- game_data %>%
  filter(shot == TRUE)  %>%
  select(-c(shot))

# save it down
write.csv(shot_data, "data/clean/shot_data.csv", row.names = FALSE)

# this is just so i can view the column names in a separate tab
write.csv(colnames(shot_data), "data/clean/shot_data_cols.csv", row.names = FALSE)

# read it back in
shot_data <- read.csv(file = "data/clean/shot_data.csv")

# prep data for logistic regression with shot data
# removed 'X' because i'm pretty sure its a superfluous column that was 
# created when saving and loading data
clean_shot_data <- shot_data %>%
  select(-c(frame, time, is_orange, shot_taker_id,
            opp_1_id, opp_2_id)) %>%
  select(-contains("throttle"), -contains("name"), -contains("_dodge"),
         -contains("steer"), -contains("ping"), -contains("cam"),
         -contains("handbrake"), -contains("boost"),
         -contains("jump"), -("X")) %>%
  add_column(shot_taker_boost_active = shot_data$shot_taker_boost_active) %>%
  na.omit()

clean_shot_data$goal <- as.logical(clean_shot_data$goal) %>%
  as.numeric()

######### EDA ##########
# clean_shot_data %>%
#   #filter(clean_shot_data$distanceToGoal < 1000) %>%
#   ggplot(aes(x = shot_taker_pos_x, y = shot_taker_pos_y, colour = goal)) +
#   geom_point(size = 0.001) +
#   geom_tile() +
#   theme_bw() +
#   labs(x = "Shot Taker X Position", y = "Shot Taker Y Position")


# explore relationship between goal probability and distance? i would assume it's log
# clean_shot_data %>%
#   ggplot(aes(x = distanceToGoal, y = mean(goal)))

# head(clean_shot_data, 5)

######################## Feature Engineering ########################
#log distance
clean_shot_data$logDistanceToGoal <- log(clean_shot_data$distanceToGoal)

#distance between opponents
clean_shot_data <- clean_shot_data %>%
  add_column(distanceToOpp1 = ((.$shot_taker_pos_x - .$opp_1_pos_x)^2 
             + (.$shot_taker_pos_y - .$opp_1_pos_y)^2 
             + (.$shot_taker_pos_z - .$opp_1_pos_z)^2) ^(1/2))

clean_shot_data <- clean_shot_data %>%
  add_column(distanceToOpp2 = ((.$shot_taker_pos_x - .$opp_2_pos_x)^2 
                               + (.$shot_taker_pos_y - .$opp_2_pos_y)^2 
                               + (.$shot_taker_pos_z - .$opp_2_pos_z)^2) ^(1/2))
# distance to teammate
clean_shot_data <- clean_shot_data %>%
  add_column(distanceToTeam = ((.$shot_taker_pos_x - .$team_mate_pos_x)^2
                               + (.$shot_taker_pos_y - .$team_mate_pos_y)^2
                               + (.$shot_taker_pos_z - .$team_mate_pos_z)^2) ^(1/2))

# remove team_mate columns because it causes bugs
clean_shot_data <- clean_shot_data %>%
  select(-contains("team_mate_")) %>%
  na.omit()

############## arc cos angle with opponents ##############

# create vectors between ball and opposition players
clean_shot_data <- clean_shot_data %>%
  add_column(vector_opp_1_x = (.$ball_pos_x - .$opp_1_pos_x))

clean_shot_data <- clean_shot_data %>%
  add_column(vector_opp_1_y = (.$ball_pos_y - .$opp_1_pos_y))

clean_shot_data <- clean_shot_data %>%
  add_column(vector_opp_1_z = (.$ball_pos_z - .$opp_1_pos_z))

clean_shot_data <- clean_shot_data %>%
  add_column(vector_opp_2_x = (.$ball_pos_x - .$opp_2_pos_x))

clean_shot_data <- clean_shot_data %>%
  add_column(vector_opp_2_y = (.$ball_pos_y - .$opp_2_pos_y))

clean_shot_data <- clean_shot_data %>%
  add_column(vector_opp_2_z = (.$ball_pos_z - .$opp_2_pos_z))

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

head(clean_shot_data, 5)

# check that cos_theta is all within 1 to -1
# all of these should be equal to 0
sum(clean_shot_data$cos_theta_opp_1 < -1)
sum(clean_shot_data$cos_theta_opp_2 < -1)
sum(clean_shot_data$cos_theta_opp_2 > 1)
sum(clean_shot_data$cos_theta_opp_2 > 1)

# train / test split
set.seed(5)
n1 = nrow(clean_shot_data)
train_samples1 = sample(1:n1, round(0.8*n1))
shot_train = clean_shot_data[train_samples1,]
shot_test = clean_shot_data[-train_samples1,]

# check class proportions -- about 35.1% of shots result in goals
shot_train %>%
  summarise(Mean = mean(shot_train$goal))
shot_data %>%
  summarise(Mean = mean(shot_data$goal))

# model 1: logistic regression
glm_fit = glm(goal ~ . - idx, family = "binomial" (link = "logit"), data = shot_train)
summary(glm_fit)

glm_pred = predict(glm_fit, type = "response", newdata = shot_test)
glm_pred_class <- ifelse(glm_pred > 0.5, 1, 0)
glm_misclass = mean(glm_pred_class != shot_test$goal)
glm_misclass

# apply predictions to full dataset
predictions = predict(glm_fit, type = "response", newdata = clean_shot_data)

data_with_xg = cbind(clean_shot_data, predictions) %>%
  rename(xg = predictions)

data_with_xg %>%
  ggplot(aes(x = shot_taker_pos_x, y = shot_taker_pos_y, colour = xg)) +
  geom_point(size = 0.001) +
  geom_tile() +
  theme_bw() +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  labs(x = "Shot Taker X Position", y = "Shot Taker Y Position")

data_with_xg %>%
  filter(predictions>0.7) %>%
  ggplot(aes(x = shot_taker_pos_x, y = shot_taker_pos_y, colour = xg)) +
  geom_point(size = 0.001) +
  geom_tile() +
  theme_bw() +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  labs(x = "Shot Taker X Position", y = "Shot Taker Y Position")

install.packages("scatterplot3d") # Install
library("scatterplot3d") # load

colors <- c("#999999", "#E69F00")
colors = colors[as.numeric(data_with_xg$goal)]
scatterplot3d(data_with_xg[,17:19], pch = 16, grid = TRUE, box = FALSE)

install.packages("plotly")
library(plotly)

plot_ly(x = data_with_xg[,17], y = data_with_xg[,18], z = data_with_xg[,19],
        type = "scatter3d", marker = list(size = 2)) %>%
  add_markers(color = ~data_with_xg[,2])
