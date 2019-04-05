### CARET - test & validation
# https://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/

# ibraries
library(caret)
library(klaR) # NaiveBayes function

# iris dataset
data(iris)

####  Data Split ----

# 80-20 train test split

split=0.80

trainIndex = createDataPartition(iris$Species, p = split, list=FALSE)

data_train = iris[ trainIndex,]
data_test = iris[-trainIndex,]

# train a naive bayes model
model = NaiveBayes(Species~., data=data_train)

# make predictions
x_test = data_test[,1:4]
y_test = data_test[,5]
predictions = predict(model, x_test)

# summarize results
confusionMatrix(predictions$class, y_test)


#### k-fold cross validation ----

# train control, number of folds and method
train_control = trainControl(method="cv", number=10)

# train the model
model = train(Species~., data=iris, trControl=train_control, method="nb", metric = "Accuracy", tuneGrid=grid)

# summarize results
print(model)


#### repeated k-fold cross validation ----

# define training control
train_control = trainControl(method="repeatedcv", number=10, repeats=3)

# train the model
model = train(Species~., data=iris, trControl=train_control, method="nb")

# summarize results
print(model)
