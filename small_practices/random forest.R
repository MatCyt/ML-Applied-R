#### Random Forest
# https://lgatto.github.io/IntroMachineLearningWithR/supervised-learning.html

# Random forest models are accurate and non-linear models and robust to over-fitting and hence quite popular. 
#They however require hyperparameters to be tuned manually, like the value k in the example above.


## Decision tree

# libraries
library(caret)
library(rpart.plot) # to visualize
library(mlbench)

# data
data(Sonar)

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


#### mtry
# caret automate the tuning of the hyperparameter using a grid search, which can be parametrised 
# by setting tuneLength (that sets the number of hyperparameter values to test) 
# or directly defining the tuneGrid (the hyperparameter values)


### Tune Length
# The tuneLength parameter tells the algorithm to try different default values for the main parameter

model_tl5 = train(Class ~ .,
                  data = Sonar,
                  method = "ranger",
                  tuneLength = 5)

print(model_tl5)

model_tl2 = train(Class ~ .,
                  data = Sonar,
                  method = "ranger",
                  tuneLength = 2)

print(model_tl2)

### tuneGrid
# The tuneGrid parameter lets us decide which values the main parameter will take
# While tuneLength only limit the number of default parameters to use.

set.seed(42)
myGrid = expand.grid(mtry = c(5, 10, 20, 40, 60),
                     splitrule = c("gini", "extratrees"),
                     min.node.size = 5)

model_tG = train(Class ~ .,
                 data = Sonar,
                 method = "ranger", 
                 tuneGrid = myGrid,
                 trControl = trainControl(method = "cv",
                                          number = 5,
                                          verboseIter = FALSE))
print(model_tG)

# Random forest, 5-fold cross validation, tuneLength = 5

set.seed(42)
model = train(Class ~ .,
               data = Sonar,
               method = "ranger",
               tuneLength = 5,
               trControl = trainControl(method = "cv",
                                        number = 5,
                                        verboseIter = FALSE))
plot(model)
