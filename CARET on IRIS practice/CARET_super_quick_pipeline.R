# Super simple IRIS on Caret

# library
library(caret)

# data
data(iris)

dataset = iris

# train test split
test_index = createDataPartition(dataset$Species, p=0.8, list = F)

train = dataset[-test_index,]
test = dataset[test_index,]

# summary
summary(train)
str(train)

# target variable class distribution
as.data.frame(prop.table(table(dataset$Species)))

# caret simple visualizations
featurePlot(x=train[,1:4], y=train[,5], plot="box", layout= c(4,1), auto.key = list(columns = 2))

# density plots for each attribute by class value
scales = list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=train[,1:4], y=train[,5], plot="density", scales=scales)


# Test Harness - cross validation
control = trainControl(method="cv", number=10)
metric = "Accuracy"

# Fit models 

# a) linear algorithms
set.seed(7)
fit.lda = train(Species~., data=dataset, method="lda", metric=metric, trControl=control)

# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart = train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)

# kNN
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)

# c) advanced algorithms
# SVM
set.seed(7)
fit.svm = train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)

# Random Forest
set.seed(7)
fit.rf = train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

# Select best
results = resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

dotplot(results)


# print best
print(fit.lda)


# Make predictions
# estimate skill of LDA on the validation dataset
predictions = predict(fit.lda, test)
confusionMatrix(predictions, test$Species)
