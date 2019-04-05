### Machine Learning Evaluation Metrics in R

# credit: https://machinelearningmastery.com/machine-learning-evaluation-metrics-in-r/

# libraries
library(caret)
library(mlbench)

# load the dataset
data(PimaIndiansDiabetes)

# prepare resampling method
control = trainControl(method="cv", number=5)

# model
set.seed(7)
fit = train(diabetes~., data=PimaIndiansDiabetes, method="glm", metric="Accuracy", trControl=control)

# display results
print(fit)


#### ROC ----

# load the dataset
data(PimaIndiansDiabetes)

# prepare resampling method
control = trainControl(method="cv", number=5, classProbs=TRUE, summaryFunction=twoClassSummary)

set.seed(7)
fit = train(diabetes~., data=PimaIndiansDiabetes, method="glm", metric="ROC", trControl=control)

# display results
print(fit)

##### ROC CURVE ----
# if you want to plot the roc auc curve you need to add savePredictions to trainControl function

control = trainControl(method="cv", 
                       number=5, 
                       classProbs=TRUE, 
                       summaryFunction=twoClassSummary, 
                       savePredictions = T)

fit = train(diabetes~., data=PimaIndiansDiabetes, method="glm", trControl=control)

head(fit$pred)

# ROC CURVE LIBRARY
library(pROC)

# Plot:
plot.roc(fit$pred$obs, fit$pred$pos)



#### LOG-LOSS ----
# Logarithmic Loss or LogLoss is used to evaluate binary classification but it is more common for 
# multi-class classification algorithms. Specifically, it evaluates the probabilities estimated by the algorithms.

# load the dataset
data(iris)
# prepare resampling method
control = trainControl(method="cv", number=5, classProbs=TRUE, summaryFunction=mnLogLoss)
set.seed(7)
fit = train(Species~., data=iris, method="rpart", metric="logLoss", trControl=control)
# display results
print(fit)