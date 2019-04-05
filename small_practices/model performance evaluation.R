#### Model perfomance and Classification performance
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
