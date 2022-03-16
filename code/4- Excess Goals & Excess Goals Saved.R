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

################################# Expected Goals  #################################
# this section is basically katie's code in the 3- file
# made very minor modifications
# using xgboost model with teammate data and engineered features as final model

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
  mutate(actual_goal_rate = total_goals/count) %>%
  mutate(avg_xg = sum_xg/count) %>%
  filter(count>10)

################################# Expected Goals Saved #################################
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
# fi <- boost_data_ids %>% 
#   group_by(across(all_of(c("shot_taker_id","ball_pos_x", "ball_pos_y", 
#                            "ball_pos_z","goal")))) %>%
#   summarise(count = n()) %>% 
#   filter(count > 1)
# 
# table(fi$count)

# 1603 repetitions
# counts are all 2 or 3
# foo <- boost_data_ids %>% 
#   group_by(across(all_of(names(boost_data_ids)))) %>%
#   summarise(count = n()) %>% 
#   filter(count > 1) 
# 
# table(foo$count)

# deal with repetitions, grab only distinct rows
boost_data_ids <- boost_data_ids %>% 
  distinct(across(all_of(names(boost_data_ids))))

# calculate XG saved when player is "opponent 1"
xg_saved_1 <- boost_data_ids %>%
  rename("saver_id" = "opp_1_id") %>%
  mutate(defended_xg = xg - goal) %>%
  group_by(saver_id) %>%
  summarise(total_defended = sum(defended_xg),
            count = n()) %>%
  ungroup() %>%
  select(c("saver_id","total_defended","count"))

# calculate XG saved when player is "opponent 2"
xg_saved_2 <- boost_data_ids %>%
  rename("saver_id" = "opp_2_id") %>%
  mutate(defended_xg = xg - goal) %>%
  group_by(saver_id) %>%
  summarise(total_defended = sum(defended_xg),
            count = n()) %>%
  ungroup() %>%
  select(c("saver_id","total_defended","count"))

# create a dataframe that contains total XG saved
xg_saved <- xg_saved_1 %>%
  full_join(xg_saved_2, by = "saver_id") %>%
  replace_na(list('total_defended.x' = 0, 'count.x' = 0,
                  'total_defended.y' = 0, 'count.y' = 0)) %>% 
  mutate(total_defended = total_defended.x + total_defended.y) %>%
  mutate(total_count = count.x + count.y) %>%
  mutate(avg_xg_saved = total_defended / total_count) %>%
  filter(total_count>10) %>% 
  select(c("saver_id", "avg_xg_saved")) %>% 
  arrange(desc(avg_xg_saved))

# join with XG dataframe to contrast
avg_xg_off_def <- xg_saved %>% 
  inner_join(aggregate, by = c("saver_id" = "shot_taker_id"))

<<<<<<< HEAD
# plot relationship between excess saved & excess goals
goals_saved_xg <- excess_goals_and_saved %>%
  ggplot(aes(x = excess_goals_saved, y = outperformance)) + 
  geom_point() +
  geom_smooth(method=lm, se = FALSE)

ggsave(filename = "results/goals_saved_xg.png", 
       plot = goals_saved_xg, 
       device = "png", 
       width = 6, 
       height = 5)
=======
# plot relationship between xg made and xg defended
avg_xg_off_def %>%
  ggplot(aes(x = avg_xg_saved, y = avg_xg)) + 
  geom_point() +
  geom_smooth(method=lm) +
  xlab("Average xG Defended (Defensive Skill)") +
  ylab("Average xG (Offensive Skill)") + 
  ggtitle("Relationship between Defensive and Offensive Skill") +
  theme(plot.title = element_text(hjust = 0.5))
>>>>>>> e9f34570d9d0d858ce2840ed412df6e631d3b704

# we only have 964 data points
nrow(avg_xg_off_def) 

# do linear regression
offense_defense_lm = lm(avg_xg~avg_xg_saved, 
               data = avg_xg_off_def)
summary(offense_defense_lm)

#### result:
# Call:
#   lm(formula = avg_xg ~ avg_xg_saved, data = avg_xg_off_def)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.227169 -0.045625 -0.001817  0.048092  0.312726 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.344317   0.002277  151.20   <2e-16 ***
#   avg_xg_saved -0.014816   0.022443   -0.66    0.509    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.07445 on 1078 degrees of freedom
# Multiple R-squared:  0.0004041,	Adjusted R-squared:  -0.0005231 
# F-statistic: 0.4358 on 1 and 1078 DF,  p-value: 0.5093


################################# XGoals Saved #################################
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

xg_saved_attempt_2 <- opp_1_save %>% 
  full_join(opp_2_save, by = "saver_id") %>%
  replace_na(list('total_defended.x' = 0, 'count.x' = 0,
                    'total_defended.y' = 0, 'count.y' = 0)) %>% 
  mutate(total_defended = total_defended.x + total_defended.x) %>%
  mutate(total_count = count.x + count.y) %>%
  mutate(avg_xg_saved = total_defended / total_count) %>%
  filter(total_count>10) %>% 
  select(c("saver_id", "avg_xg_saved")) %>% 
  arrange(desc(avg_xg_saved))

# join with excess goals dataframe to contrast
avg_xg_off_def_2 <- xg_saved_attempt_2 %>% 
  inner_join(aggregate, by = c("saver_id" = "shot_taker_id"))

# plot relationship between excess saved & excess goals
avg_xg_off_def_2 %>%
  ggplot(aes(x = avg_xg_saved, y = avg_xg)) + 
  geom_point() +
  geom_smooth(method=lm) +
  xlab("Average xG Defended (Defensive Skill)") +
  ylab("Average xG (Offensive Skill)") + 
  ggtitle("Relationship between Defensive and Offensive Skill") +
  theme(plot.title = element_text(hjust = 0.5))

setwd("~/Desktop/Final Project/stat401-rocketleague/results")
ggsave("def_off.png")


# 649 rows
nrow(avg_xg_off_def_2) 

# 105 rows with a perfect 0 
nrow(avg_xg_off_def_2[avg_xg_off_def_2$avg_xg_saved == 0,])

# do linear regression
excess_lm_2 = lm(avg_xg~avg_xg_saved, 
               data = avg_xg_off_def_2)
summary(excess_lm_2)

# 03f216c44bb24c0ebed7f101bae5aabc
# opp_2_save[opp_2_save$saver_id == '03f216c44bb24c0ebed7f101bae5aabc',]

