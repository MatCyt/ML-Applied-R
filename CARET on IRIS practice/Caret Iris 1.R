# Caret Practice - IRIS 
# https://subhayo.wordpress.com/2017/11/09/predictive-modelling-with-caret-classification-of-iris-species/

# Libraries
library(caret)
library(pROC)
library(AppliedPredictiveModeling) # transparentTheme

# Data
data("iris")

summary(iris)
head(iris)

set.seed(123)

# Visualize the dataset
transparentTheme( trans = 0.5)
featurePlot(x = iris[, 1:4], 
              y = iris$Species, 
              plot = "ellipse",
              auto.key = list(columns = 3))


# Train, test split 
trainIndex = createDataPartition(iris$Species, p = .8, # 80-20
                                    list = FALSE,
                                    times = 1)
train = iris[ trainIndex,]
test  = iris[-trainIndex,]

# Cross validation
fitControl = trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5)

# Decision tree
dt.fit = train(Species ~ ., data = train,
                method = "rpart",
                trControl = fitControl,
                preProcess=c("center", "scale"))

summary(dt.fit)

p = predictions = predict(dt.fit, test)

confusionMatrix(p, test$Species)

# KNN
knn.fit = train(Species ~ ., data = train,
                 method = "knn",
                 trControl = fitControl,
                 preProcess=c("center", "scale"))

summary(knn.fit)

p = predict(knn.fit, test)

confusionMatrix(p, test$Species)

# Random forest
rf.fit = train(Species ~ ., data = train,
                method = "rf",
                trControl = fitControl,
                preProcess=c("center", "scale"))

summary(rf.fit)

p = predict(rf.fit, test)

confusionMatrix(p, test$Species)