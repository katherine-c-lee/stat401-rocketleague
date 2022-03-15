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

# apply boost predictions to full dataset
predictions = predict(gbm_fit, type = "response", newdata = xgdata)

data_with_boost_xg = cbind(xgdata, predictions) %>%
  rename(xg = predictions) %>%
  as_tibble()

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
  arrange(desc(outperformance)) %>%
  mutate(avg_xg = sum_xg/count) %>%
  filter(count>20)

# write.csv(aggregate, file = "results/aggregate.csv")

aggregate_no_ids <- aggregate %>% select(-c(shot_taker_id))
corrplot(cor(aggregate_no_ids),        # Correlation matrix
         method = "shade", # Correlation plot method
         type = "upper",    # Correlation plot style (also "upper" and "lower")
         diag = TRUE,      # If TRUE (default), adds the diagonal
         tl.col = "black", # Labels color
         bg = "white",     # Background color
         col = NULL)       # Color palette

# top 5 players in terms of excess goals
top5_outperformance = head(aggregate, 5)
top5_outperformance

# top 5 players in terms of total shots taken
top5_attempts = aggregate %>%
  arrange(desc(count)) %>%
  head(5)   
top5_attempts  ## there is no overlap

# top 5 average xg players
top5_avg_xg <- aggregate %>%
  arrange(desc(avg_xg)) %>%
  head(5)
top5_avg_xg ## seems like the high average xg players are not taking a ton of shots

## top outperformer deep dive
top_outperformer <- aggregate %>% 
  arrange(desc(outperformance)) %>%
  head(1) %>%
  pull(shot_taker_id)

data_with_boost_xg %>%
  filter(shot_taker_id == top_outperformer) %>%
  select_if(is.numeric) %>%
  names()

top_performer_avgs <- data_with_boost_xg %>%
  filter(shot_taker_id == top_outperformer) %>%
  select_if(is.numeric) %>%
  colMeans()

overall_avg_player <- data_with_boost_xg %>%
  select_if(is.numeric) %>%
  colMeans()

(overall_avg_player-top_performer_avgs)/top_performer_avgs
# # the top outperformer is mainly outperforming in ball ang vel x and ball hit team no
## ang vel x is how much the ball is spinning in the x direction, could be that the player is a "fast tempo" shot taker

## check to see if the ang vel x of the ball holds for the second top outperformer
outperformer2 <- aggregate[2,] %>%
  pull(shot_taker_id)

data_with_boost_xg %>%
  filter(shot_taker_id == outperformer2) %>%
  select_if(is.numeric) %>%
  names()

performer2_avgs <- data_with_boost_xg %>%
  filter(shot_taker_id == outperformer2) %>%
  select_if(is.numeric) %>%
  colMeans()

(overall_avg_player-performer2_avgs)/top_performer_avgs
## ball vel x, ball rot x, shot taker rotation, team mate velocity x


## player 76561198002542052 appears in top 5 average xg, and top 5 outperformance

# filter data for top players
names(data_with_boost_xg)
data_with_boost_xg %>%
  as_tibble() %>%
  filter(shot_taker_id == "76561198012337838") %>%
  select(goal, distanceToGoal, xg, shot_taker_pos_x, 
         shot_taker_pos_y, shot_taker_pos_z)
