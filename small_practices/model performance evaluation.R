#### Model perfomance Evaluation
# https://lgatto.github.io/IntroMachineLearningWithR/supervised-learning.html

# libraries ----
library(caret)
library(dplyr)

# data ----
data("diamonds")



### RMSE | in-sample, train error ----

# simple linear regression
model = lm(price ~ ., diamonds)

p = predict(model, diamonds) #in-sample / train error

# calculate RMSE 1 - caret
RMSE(p, diamonds$price)

# calculate RMSE 2 - around (error on prediction)
error <- p - diamonds$price
rmse_in <- sqrt(mean(error^2)) ## in-sample RMSE 
rmse_in



### RMSE | out-of-sample, test error and split ----

# by hand approach
set.seed(42)

sample_size = nrow(diamonds) * 0.8 # 80-20 split
train = sample(nrow(diamonds), sample_size) # take random 80%

model = lm(price ~ ., data = diamonds[train, ])

p = predict(model, diamonds[-train, ])

error = p - diamonds$price[-train]
rmse_out = sqrt(mean(error^2)) ## out-of-sample RMSE 
rmse_out

# caret approach
split = 0.80

trainIndex = createDataPartition(diamonds$price, p = split, list=FALSE)

diamonds_train = diamonds[ trainIndex,]
diamonds_test = diamonds[-trainIndex,]

model = lm(price ~ ., data = diamonds_train)

p = predict(model, diamonds_test)

RMSE(p, diamonds_test$price)


### CROSS-VALIDATION ----

# We specify the method (the learning algorithm) we want to use. "lm" for now
# We then set the out-of-sample training procedure to 10-fold cross validation (method = "cv" and number = 10). 
# To simplify the output in the material for better readability, we set the verbosity flag to FALSE, 
# but it is useful to set it to TRUE in interactive mode.

set.seed(42)

model =  train(price ~ ., diamonds,
               method = "lm", 
               trControl = trainControl(method = "cv", 
                                        number = 10, 
                                        verboseIter = F))
model
summary(model)
model$results

RMSE(p, diamonds$price)

p = predict(model, diamonds)
error = p - diamonds$price
rmse_xval = sqrt(mean(error^2)) ## xval RMSE
rmse_xval


