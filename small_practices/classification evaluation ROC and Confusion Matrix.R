#### Classification performance evaluation
# https://lgatto.github.io/IntroMachineLearningWithR/supervised-learning.html



### Confusion Matrix ----
# Mines vs Rocks classification

#libraries
library("mlbench")

# data
data(Sonar)

## random 60/40 split
tr = sample(nrow(Sonar), round(nrow(Sonar) * 0.6))
train = Sonar[tr, ]
test = Sonar[-tr, ]

# logistic reg model
model = glm(Class ~ ., data = train, family = "binomial")

p = predict(model, test, type = "response")
summary(p)

# Confusion Matrix - by hand
cl = ifelse(p > 0.5, "M", "R")
table(cl, test$Class)

# Confusion Matrix - Caret
confusionMatrix(as.factor(cl), test$Class)


### ROC Curve ----

# library for ROC Curve
library(caTools)

# Plot ROC - y = sensitivity / recall      and      x = 1 - precision / specificity
caTools::colAUC(p, test$Class, plotROC = TRUE)
?colAUC


### AUC in Caret ----

## Create trainControl object: myControl
myControl = trainControl(
  method = "cv", ## cross validation
  number = 10,   ## 10-fold
  summaryFunction = twoClassSummary, ## NEW
  classProbs = TRUE, # IMPORTANT
  verboseIter = FALSE
)
## Train glm with custom trainControl: model
model = train(Class ~ ., Sonar,
               method = "glm", ## to use glm's logistic regression
               trControl = myControl) 

## Print model to console
print(model)
