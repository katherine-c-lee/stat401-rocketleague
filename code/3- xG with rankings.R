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

setwd("~/Dropbox (Penn)/__SPRING 2022/STAT401/stat401-rocketleague")
game_data
names(game_data)
shot_data <- game_data %>%
  filter(shot == TRUE)  %>%
  select(-c(shot))

dim(shot_data)

clean_shot_data <- shot_data %>%
  select(-c(frame, time, is_orange,
            opp_1_id, opp_2_id)) %>%
  select(-contains("throttle"), -contains("name"), -contains("_dodge"),
         -contains("steer"), -contains("ping"), -contains("cam"),
         -contains("handbrake"), -contains("boost"),
         -contains("jump")) %>%
  add_column(shot_taker_boost_active = shot_data$shot_taker_boost_active) %>%
  na.omit()
clean_shot_data$goal <- as.logical(clean_shot_data$goal) %>%
  as.numeric()

names(clean_shot_data)
# clean_shot_data$shot_taker_id

##################### Feature Engineering w/o Teammate Data ####################
clean_shot_data <- shot_data %>%
  select(-c(frame, time, is_orange, shot_taker_id,
            opp_1_id, opp_2_id)) %>%
  select(-contains("throttle"), -contains("name"), -contains("_dodge"),
         -contains("steer"), -contains("ping"), -contains("cam"),
         -contains("handbrake"), -contains("boost"),
         -contains("jump")) %>%
  add_column(shot_taker_boost_active = shot_data$shot_taker_boost_active) %>%
  na.omit()

clean_shot_data$goal <- as.logical(clean_shot_data$goal) %>%
  as.numeric()

# write.csv(head(clean_shot_data), file = "results/clean_shot_data.csv")

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

head(clean_shot_data, 5)

# check that cos_theta is all within 1 to -1
# all of these should be equal to 0
sum(clean_shot_data$cos_theta_opp_1 < -1)
sum(clean_shot_data$cos_theta_opp_2 < -1)
sum(clean_shot_data$cos_theta_opp_2 > 1)
sum(clean_shot_data$cos_theta_opp_2 > 1)

############################## Logistic Regression #############################
# train / test split
set.seed(5)
n1 = nrow(clean_shot_data)
train_samples1 = sample(1:n1, round(0.8*n1))
shot_train = clean_shot_data[train_samples1,]
shot_test = clean_shot_data[-train_samples1,]

names(shot_train)
dim(shot_train)
dim(shot_test)

glm_fit = glm(goal ~ . - idx - distanceToGoal, 
              family = "binomial" (link = "logit"), 
              data = shot_train)
summary(glm_fit)
save(glm_fit, file = "results/glm_fit.Rda")

# probability predictions
glm_pred = predict(glm_fit, type = "response", newdata = shot_test)

# log loss
library(MLmetrics)
glm_losloss_fe <- LogLoss(y_pred = glm_pred, y_true = shot_test$goal)
glm_losloss_fe

# apply test probabilities to test dataset
testdata_logreg_pred = cbind(shot_test, glm_pred) %>%
  rename(xg = glm_pred) %>%
  as_tibble()

# ROC curve
roc_data = roc(testdata_logreg_pred %>% pull(goal), 
               testdata_logreg_pred %>% pull(xg)) 
fpr_tpr <- tibble(FPR = 1-roc_data$specificities,
                  TPR = roc_data$sensitivities)
fpr_tpr %>%
  ggplot(aes(x = FPR, y = TPR)) + 
  geom_line() + 
  geom_abline(slope = 1, linetype = "dashed") +
  theme_bw()

# print the AUC
roc_data$auc

gmean = sqrt(fpr_tpr$TPR * (1-fpr_tpr$FPR))
gmean_max = which.max(gmean)
opt_threshold = roc_data$thresholds[gmean_max]
opt_threshold

# misclassification rate with optimal threshold
glm_pred_class <- ifelse(glm_pred > 0.5, 1, 0)
glm_misclass = mean(glm_pred_class != shot_test$goal)
glm_misclass
accuracy = 1-glm_misclass
accuracy

# apply test probabilities to test dataset
cbind(shot_test, glm_pred_class) %>%
  as_tibble() %>%
  select(glm_pred_class, goal) %>%
  table()

# apply predictions to full dataset
predictions = predict(glm_fit, type = "response", newdata = clean_shot_data)

data_with_glm_xg = cbind(clean_shot_data, predictions) %>%
  rename(xg = predictions)

#################################### Ridge #####################################
library(glmnetUtils)                              # to run ridge and lasso
source("code/functions/plot_glmnet.R") 

# run ridge regression
ridge_fit = cv.glmnet(goal ~ . -idx - distanceToGoal,   
                      alpha = 0,                 
                      nfolds = 10,               
                      data = shot_train, 
                      show_col_types = FALSE)
plot(ridge_fit)

coef(ridge_fit, s = "lambda.1se") %>% head()
coef(ridge_fit, s = "lambda.min") %>% head()
save(ridge_fit, file = "results/ridge_fit.Rda")

ridge_predictions = predict(ridge_fit, 
                            newdata = shot_test, 
                            s = "lambda.1se") %>% as.numeric()

# misclassification rate
ridge_pred_class <- ifelse(ridge_predictions > 0.5, 1, 0)
ridge_misclass = mean(ridge_pred_class != shot_test$goal)
ridge_misclass

# log loss
ridge_losloss_fe <- LogLoss(y_pred = ridge_predictions, y_true = shot_test$goal)
ridge_losloss_fe

#################################### Lasso #####################################
lasso_fit = cv.glmnet(goal ~ . -idx - distanceToGoal,   
                      alpha = 1,                 
                      nfolds = 10,               
                      data = shot_train,
                      show_col_types = FALSE)
save(lasso_fit, file = "results/lasso_fit.Rda")

plot(lasso_fit)


p = plot_glmnet(lasso_fit, shot_train, features_to_plot = 6, 
                lambda = lasso_fit$lambda.min)
p
ggsave(filename = "results/lasso-trace-plot.png", 
       plot = p, 
       device = "png", 
       width = 7, 
       height = 5)

# extract features selected by lasso and their coefficients
beta_hat_std = extract_std_coefs(lasso_fit, shot_train, 
                                 lambda = lasso_fit$lambda.min)
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>% 
  head(10) %>%
  write_tsv("results/lasso-features-table.tsv")

lasso_predictions = predict(lasso_fit, 
                            newdata = shot_test, 
                            s = "lambda.1se") %>% as.numeric()

# misclassification rate
lasso_pred_class <- ifelse(lasso_predictions > 0.5, 1, 0)
lasso_misclass = mean(lasso_pred_class != shot_test$goal)
lasso_misclass

# log loss
lasso_losloss_fe <- LogLoss(y_pred = lasso_predictions, y_true = shot_test$goal)
lasso_losloss_fe

##################### Feature Engineering w/ Teammate Data #####################
clean_shot_data <- shot_data %>%
  select(-c(frame, time, is_orange,
            opp_1_id, opp_2_id)) %>%
  select(-contains("throttle"), -contains("name"), -contains("_dodge"),
         -contains("steer"), -contains("ping"), -contains("cam"),
         -contains("handbrake"), -contains("boost"),
         -contains("jump")) %>%
  add_column(shot_taker_boost_active = shot_data$shot_taker_boost_active) %>%
  na.omit()

clean_shot_data$goal <- as.logical(clean_shot_data$goal) %>%
  as.numeric()

# write.csv(head(clean_shot_data), file = "results/clean_shot_data.csv")

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
# clean_shot_data <- clean_shot_data %>%
  # select(-contains("team_mate_")) %>%
  # na.omit()

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

names(clean_shot_data)

#################################### xgBoost ################################### 
# for data, rerun feature engineering code, except for removal of teammate data
xgdata <- clean_shot_data %>%
  select(-c("team_mate_id","shot_taker_boost_active", "idx"))

# train / test split
set.seed(5)
shot_train_xg = xgdata[train_samples1,]
shot_test_xg = xgdata[-train_samples1,]

gbm_fit = gbm(goal ~ . -shot_taker_id,
              distribution = "bernoulli",
              n.trees = 200,
              interaction.depth = 3,
              shrinkage = 0.1,
              cv.folds = 5,
              data = shot_train_xg)
boost_feature_importance <- summary(gbm_fit, n.trees = 200, plotit = FALSE) %>%
  as_tibble() %>%
  head(10)

write.csv(boost_feature_importance, 
          file = "results/boost_feature_importance.csv")

save(gbm_fit, file = "gbm_fit.Rda")
load("results/gbm_fit.Rda")
summary(gbm_fit)

# partial dependence plots
plot(gbm_fit, i.var = "distanceToGoal", n.trees = 200, type = "response")
plot(gbm_fit, i.var = "cos_theta_opp_1", n.trees = 200, type = "response")
plot(gbm_fit, i.var = "cos_theta_opp_2", n.trees = 200, type = "response")

gbm_probabilities = predict(gbm_fit, n.trees = 200,
                            type = "response", newdata = shot_test_xg)

# misclassification rate
gbm_pred = predict(gbm_fit, n.trees = 200,
                   type = "response", newdata = shot_test_xg)
gbm_pred_class <- ifelse(gbm_pred > 0.5, 1, 0)
gbm_misclass = mean(gbm_pred_class != shot_test_xg$goal)
gbm_misclass

# log loss
gbm_logloss <- LogLoss(y_pred = gbm_pred, y_true = shot_test_xg$goal)

# ROC curve
pred_gbm <- prediction(gbm_pred, shot_test_xg$goal)
perf_gbm <- performance(pred_gbm, "tpr", "fpr")
plot(perf_gbm)

# takeaway: xgboost with engineered features + teammate features is better

# apply predictions to full dataset
predictions = predict(gbm_fit, type = "response", newdata = xgdata)

data_with_boost_xg = cbind(xgdata, predictions) %>%
  rename(xg = predictions) %>%
  as_tibble()

names(data_with_boost_xg)


p_xg <- data_with_boost_xg %>%
  ggplot(aes(x = shot_taker_pos_x, y = shot_taker_pos_y, colour = xg)) +
  geom_point(size = 0.001) +
  geom_tile() +
  theme_bw() +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  labs(x = "Shot Taker X Position", y = "Shot Taker Y Position")

p_outcome <- data_with_boost_xg %>%
  ggplot(aes(x = shot_taker_pos_x, y = shot_taker_pos_y, colour = goal)) +
  geom_point(size = 0.001) +
  geom_tile() +
  theme_bw() +
  # scale_colour_gradientn(colours = terrain.colors(10)) +
  labs(x = "Shot Taker X Position", y = "Shot Taker Y Position")

p_all = plot_grid(p_xg, p_outcome, 
                  nrow = 2)

ggsave(filename = "results/xg-field.png", 
       plot = p_all, 
       device = "png", 
       width = 6, 
       height = 10)



############################# xG Model Evaluation ##############################
# for benchmark, find log loss when just predicting the mean of the training set
shot_train_xg # training dataset from xgboost

avg_goals <- shot_train_xg %>%
  summarise(avg_gaol = mean(goal)) %>%
  as.numeric()
avg_goals_repped <- rep(avg_goals, dim(shot_test_xg)[[1]])
pred_naive <- cbind(shot_test_xg, avg_goals_repped)
names(pred_naive)
naive_logloss <- LogLoss(y_pred = avg_goals_repped, y_true = shot_test_xg$goal)

xg_model_eval_reg <- tribble(
  ~Model, ~"Log Loss", ~"Misclassification Rate",
  #--|--|----
  "Logistic Regression, FE, no teammate data", glm_losloss_fe, glm_misclass,
  "Ridge Regression, FE, no teammate data", ridge_losloss_fe, ridge_misclass,
  "Lasso Regression, FE, no teammate data", lasso_losloss_fe, lasso_misclass,
) ## xgboost is best

write.csv(xg_model_eval_reg, file = "results/xg_model_eval_reg.csv")

xg_model_eval <- tribble(
  ~Model, ~"Log Loss", ~"Misclassification Rate",
  #--|--|----
  "Logistic Regression, FE, no teammate data", glm_losloss_fe, glm_misclass,
  "Ridge Regression, FE, no teammate data", ridge_losloss_fe, ridge_misclass,
  "Lasso Regression, FE, no teammate data", lasso_losloss_fe, lasso_misclass,
  "xgBoost, FE, with teammate data", gbm_logloss, gbm_misclass, 
  "Naive classifier", naive_logloss, avg_goals
) ## xgboost is best

write.csv(xg_model_eval, file = "results/xg_model_eval.csv")


