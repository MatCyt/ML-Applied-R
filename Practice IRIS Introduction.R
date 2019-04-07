# IRIS Practice Introduction with CARET
# https://machinelearningmastery.com/machine-learning-in-r-step-by-step/


# Libraries
library(caret)
library(ellipse)

# data
data("iris")
dataset = iris


# train test split 80-20 ----
split_index = createDataPartition(dataset$Species, p=0.80, list=FALSE)

# 20% for the test, 80% for the training
ds_training = dataset[split_index, ]
ds_test = dataset[-split_index, ]


### Summarize Dataset ----
dim(ds_training)

summary(ds_training)

View(ds_training)

str(ds_training)

# look at the class of target
levels(ds_training$Species)

# class distribution
percentage = prop.table(table(ds_training$Species)) * 100
cbind(freq = table(ds_training$Species), percentage=percentage)

# shorter approach to class distribution
table(ds_training$Species)/nrow(ds_training)


### Visualize Dataset ----

# We are going to look at two types of plots:
    # 1) Univariate plots to better understand each attribute.
    # 2) Multivariate plots to better understand the relationships between attributes.

# Split features and target
x = ds_training[,1:4]
y = ds_training[,5]

# Univariate exaplme - Box Plots for each numeric (all of them in this case) independent variable
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}

# Multivariate -  scatterplots of all pairs of attributes and color the points by class
# scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")

# Box plot for features plotted for each class
featurePlot(x=x, y=y, plot="box")

# density plots for each attribute by class value
scales = list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)


### Modelling ----

# 10-fold cross validation
mycontrol = trainControl(method="cv", number=10)

# Models - let's evaluate 5 different algorithms
    # Linear Discriminant Analysis (LDA)
    # Classification and Regression Trees (CART).
    # k-Nearest Neighbors (kNN).
    # Support Vector Machines (SVM) with a linear kernel.
    # Random Forest (RF)

# set.seed - to ensure that the evaluation of each algorithm is performed using exactly the same data splits
set.seed(7)

# LINEAR
fit.lda = train(Species~., 
                data=dataset, 
                method="lda", 
                metric="Accuracy", 
                trControl=mycontrol)

# NON-LINEAR
# CART - decision tree
set.seed(7)
fit.cart = train(Species~., 
                 data=dataset, 
                 method="rpart", 
                 metric="Accuracy", 
                 trControl=mycontrol)
# kNN
set.seed(7)
fit.knn = train(Species~., 
                data=dataset, 
                method="knn", 
                metric="Accuracy", 
                trControl=mycontrol)

# ADVANCED NON-LINEAR
# SVM
set.seed(7)
fit.svm = train(Species~., 
                data=dataset, 
                method="svmRadial", 
                metric="Accuracy", 
                trControl=mycontrol)
# Random Forest
set.seed(7)
fit.rf = train(Species~., 
                data=dataset, 
                method="rf", 
                metric="Accuracy", 
                trControl=mycontrol)


# compare the results
results = resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

dotplot(results)


# LDA seems to be the best performing in current configuration
print(fit.lda)


### Make predictions ----

# predict LDA on test set
p = predict(fit.lda, ds_test)
confusionMatrix(p, ds_test$Species)

