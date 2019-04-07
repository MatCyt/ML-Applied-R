#### Random Forest
# https://lgatto.github.io/IntroMachineLearningWithR/supervised-learning.html

# Random forest models are accurate and non-linear models and robust to over-fitting and hence quite popular. 
#They however require hyperparameters to be tuned manually, like the value k in the example above.


## Decision tree

# libraries
library(caret)
library(rpart.plot) # to visualize

set.seed(123)

trainIndex = createDataPartition(Sonar$Class, p = 0.8, list=FALSE)

data_train = Sonar[trainIndex,]
data_test = Sonar[-trainIndex,]

trctrl = trainControl(method = "cv", number = 10, repeats = 3)

tree_fit = train(Class ~., data = data_train, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)

tree_fit

prp(tree_fit$finalModel, box.palette = "Reds")


p = predict(tree_fit, data_test, type = "raw")
confusionMatrix(as.factor(p), data_test$Class) 

### Random Forest - Caret

set.seed(12)
model = train(Class ~ .,
               data = Sonar,
               method = "ranger") 
print(model)

plot(model)
