detach("package:dplyr")
install.packages("pdp")
library("pdp")

# partial dependence plots vLinda
# https://rpubs.com/vishal1310/QuickIntroductiontoPartialDependencePlots
par_distanceToGoal <- pdp::partial(gbm_fit, pred.var = c("distanceToGoal"), 
                                   n.trees = 200, chull = TRUE)
autoplot(par_distanceToGoal, contour = TRUE)

##
par_angles <- pdp::partial(gbm_fit, pred.var = c("cos_theta_opp_1", 
                                                 "cos_theta_opp_2"), 
                                   n.trees = 200, chull = TRUE)
autoplot(par_angles, contour = TRUE)

##
par_angle_distance <- pdp::partial(gbm_fit, pred.var = c("cos_theta_opp_1", 
                                                         "distanceToGoal"), 
                           n.trees = 200, chull = TRUE)
autoplot(par_angle_distance, contour = TRUE)

##
par_angle_distance <- pdp::partial(gbm_fit, pred.var = c("cos_theta_opp_1", 
                                                         "distanceToGoal"), 
                                   n.trees = 200, chull = TRUE)
autoplot(par_angle_distance, contour = TRUE)

##
par_angle_distance_opp <- pdp::partial(gbm_fit, pred.var = c("cos_theta_opp_1", 
                                                         "distanceToOpp1"), 
                                   n.trees = 200, chull = TRUE)
autoplot(par_angle_distance_opp, contour = TRUE)


##
names(xgdata)

