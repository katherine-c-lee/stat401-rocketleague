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
  select(contains("id"), "ball_pos_x", "ball_pos_y", "ball_pos_z","goal") %>%
  select(-c("idx"))

boost_data_ids <- data_with_boost_xg %>%
  inner_join(shot_data_ids, by = c("shot_taker_id","ball_pos_x", "ball_pos_y",
                                   "ball_pos_z","goal"))

# noticed that there are repeated rows
# 1456 repetitions
# counts are all 4 or 9
fi <- boost_data_ids %>% 
  group_by(across(all_of(c("shot_taker_id","ball_pos_x", "ball_pos_y", 
                           "ball_pos_z","goal")))) %>%
  summarise(count = n()) %>% 
  filter(count > 1)

table(fi$count)

# 1603 repetitions
# counts are all 2 or 3
foo <- boost_data_ids %>% 
  group_by(across(all_of(names(boost_data_ids)))) %>%
  summarise(count = n()) %>% 
  filter(count > 1) 

table(foo$count)

# deal with repetitions 
boost_data_ids <- boost_data_ids %>% 
  distinct(across(all_of(names(boost_data_ids))))

# calculate excess goals saved when player is "opponent 1"
xg_saved_1 <- boost_data_ids %>%
  rename("saver_id" = "opp_1_id") %>%
  mutate(defended_xg = xg - goal) %>%
  group_by(saver_id) %>%
  summarise(total_defended = sum(defended_xg),
            count = n()) %>%
  ungroup() %>%
  select(c("saver_id","total_defended","count"))

# calculate excess goals saved when player is "opponent 2"
xg_saved_2 <- boost_data_ids %>%
  rename("saver_id" = "opp_2_id") %>%
  mutate(defended_xg = xg - goal) %>%
  group_by(saver_id) %>%
  summarise(total_defended = sum(defended_xg),
            count = n()) %>%
  ungroup() %>%
  select(c("saver_id","total_defended","count"))

# create a dataframe of all players who made a shot
unique_ids <- data_with_boost_xg %>%
  group_by(data_with_boost_xg$shot_taker_id) %>%
  summarise(count = n()) %>%
  filter(count>20) %>%
  rename("saver_id" = "data_with_boost_xg$shot_taker_id")

# create a dataframe that contains excess goals saved
xg_saved <- unique_ids %>%
  inner_join(xg_saved_1, by = "saver_id") %>%
  inner_join(xg_saved_2, by = "saver_id") %>%
  mutate(total_defended = total_defended.x + total_defended.x) %>%
  mutate(total_count = count.x + count.y) %>%
  mutate(excess_goals_saved = total_defended / total_count) %>%
  select(c("saver_id", "excess_goals_saved")) %>% 
  arrange(desc(excess_goals_saved))

# top 5 players in terms of excess goals saved
head(xg_saved, 5)

# join with excess goals dataframe to contrast
excess_goals_and_saved <- xg_saved %>% 
  inner_join(aggregate, by = c("saver_id" = "shot_taker_id"))

# plot relationship between excess saved & excess goals
excess_goals_and_saved %>%
  ggplot(aes(x = excess_goals_saved, y = outperformance)) + 
  geom_point() +
  geom_smooth(method=lm)

# we only have 177 data points
nrow(excess_goals_and_saved) 

# do linear regression
excess_lm = lm(outperformance~excess_goals_saved, 
               data = excess_goals_and_saved)
summary(excess_lm)

#### result:
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         0.98980    0.02111  46.892   <2e-16 ***
#   excess_goals_saved -0.53799    0.24358  -2.209   0.0285 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2729 on 175 degrees of freedom
# Multiple R-squared:  0.02712,	Adjusted R-squared:  0.02156 
# F-statistic: 4.878 on 1 and 175 DF,  p-value: 0.0285


################################# Excess Goals Saved #################################
## approach 2
## we only assign excess goals saved to the player who is nearest to the 
## shot-taker

opp_1_save <- boost_data_ids %>%
  filter(distanceToOpp1 < distanceToOpp2) %>% 
  rename("saver_id" = "opp_1_id") %>%
  mutate(defended_xg = xg - goal) %>%
  group_by(saver_id) %>%
  summarise(total_defended = sum(defended_xg),
            count = n()) %>%
  ungroup() %>%
  select(c("saver_id","total_defended","count"))

opp_2_save <- boost_data_ids %>%
  filter(distanceToOpp1 > distanceToOpp2) %>% 
  rename("saver_id" = "opp_2_id") %>%
  mutate(defended_xg = xg - goal) %>%
  group_by(saver_id) %>%
  summarise(total_defended = sum(defended_xg),
            count = n()) %>%
  ungroup() %>%
  select(c("saver_id","total_defended","count"))

xg_saved_attempt_2 <- unique_ids %>%
  inner_join(opp_1_save, by = "saver_id") %>%
  inner_join(opp_2_save, by = "saver_id") %>%
  mutate(total_defended = total_defended.x + total_defended.x) %>%
  mutate(total_count = count.x + count.y) %>%
  mutate(excess_goals_saved = total_defended / total_count) %>%
  select(c("saver_id", "excess_goals_saved")) %>% 
  arrange(desc(excess_goals_saved))

nrow(xg_saved_attempt_2)

# join with excess goals dataframe to contrast
excess_goals_and_saved_2 <- xg_saved_attempt_2 %>% 
  inner_join(aggregate, by = c("saver_id" = "shot_taker_id"))

# plot relationship between excess saved & excess goals
excess_goals_and_saved_2 %>%
  ggplot(aes(x = excess_goals_saved, y = outperformance)) + 
  geom_point() +
  geom_smooth(method=lm) +
  xlab("Average xG Defended Against (Defensive Power)") +
  ylab("Average Actual Goals Over xG (Offensive Power)")

# we have 2 fewer datapoints, at 175 rows now
nrow(excess_goals_and_saved_2) 

# do linear regression
excess_lm_2 = lm(outperformance~excess_goals_saved, 
               data = excess_goals_and_saved)
summary(excess_lm_2)

## results
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         0.99235    0.02116  46.900   <2e-16 ***
#   excess_goals_saved -0.63584    0.25651  -2.479   0.0141 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2719 on 175 degrees of freedom
# Multiple R-squared:  0.03392,	Adjusted R-squared:  0.0284 
# F-statistic: 6.144 on 1 and 175 DF,  p-value: 0.01413

# very slightly better r-squared (0.2 to 0.3) but not significant enough
