# Caret Measuring Performance
# https://topepo.github.io/caret/measuring-performance.html
library(caret)

# postResample() ----
# RMSE, R2, MAE for test (after predict) results for regression

library(mlbench)
data(BostonHousing)

set.seed(280)
bh_index <- createDataPartition(BostonHousing$medv, p = .75, list = FALSE)
bh_tr <- BostonHousing[ bh_index, ]
bh_te <- BostonHousing[-bh_index, ]

set.seed(7279)
lm_fit <- train(medv ~ . + rm:lstat,
                data = bh_tr, 
                method = "lm")
bh_pred <- predict(lm_fit, bh_te)

lm_fit


postResample(pred = bh_pred, obs = bh_te$medv)


# confusionMatrix()
# cross tabulation of the observed and predicted classes

confusionMatrix(data = test$predicted, reference = test$observed)

# use mode for precision / recall if you prefer them over specificity 
confusionMatrix(data = test_set$pred, reference = test_set$obs, mode = "prec_recall")



# twoClassSummary()
# For data with two classes, there are specialized functions for measuring model performance. 
# First, the twoClassSummary function computes the area under the ROC curve and the specificity and sensitivity under the 50% cutoff. 

# ROC, Sensitivity, Specificity
twoClassSummary(test_set, lev = levels(test_set$obs))

# AUC, Precision, Recall
prSummary(test_set, lev = levels(test_set$obs))