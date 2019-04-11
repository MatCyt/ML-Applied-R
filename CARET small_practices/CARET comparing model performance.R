### Model Selection Caret
# https://lgatto.github.io/IntroMachineLearningWithR/supervised-learning.html


# We are going to compare different predictive models and chose the best one.

# First create a set of common training controller object with the same train/test folds 
# and model evaluation metrics that we will re-use. This is important to guarantee fair comparison.

# Churn Data - about 15% of the customers churn. It is important to maintain this proportion in all the folds.


# libraries
library(caret)
library(C50)

# Dataset
data(churn)

# balance within the dataset
table(churnTrain$churn)/nrow(churnTrain)


# SAME RE-USABLE FOLDS ----

# we want the same folds to be re-used over multiple model training rounds, we are going to pass the train/test splits directly.
# These splits are created with the createFolds function, which creates a list containing the element indices for each fold.

myFolds = createFolds(churnTrain$churn, k = 5)
str(myFolds)

# verify that the folds maintain the same structure
sapply(myFolds, function(i) {
  table(churnTrain$churn[i])/length(i)
})

# We can now a train control object to be reused consistently for different model trainings.
myControl = trainControl(
  summaryFunction = twoClassSummary, # binary problem
  classProb = TRUE,
  verboseIter = FALSE,
  savePredictions = TRUE,
  index = myFolds # same folds
)

# ALL POSSIBLE MODEL NAMES ----
names(getModelInfo())


# Model 1 - glmnet ----
glm_model = train(churn ~ .,
                   churnTrain,
                   metric = "ROC", 
                   method = "glmnet",
                   tuneGrid = expand.grid(alpha = 0:1, lambda = 0:10/10),
                   trControl = myControl)

print(glm_model)
plot(glm_model)

# Model 2 - Random Forest ----
rf_model = train(churn ~ .,
                  churnTrain,
                  metric = "ROC", 
                  method = "ranger",
                  tuneGrid = expand.grid(
                    mtry = c(2, 5, 10, 19),
                    splitrule = c("gini", "extratrees"),
                    min.node.size = 5),
                  trControl = myControl)

print(rf_model)
plot(rf_model)

# Model 3 - kNN ----
knn_model = train(churn ~ .,
                   churnTrain,
                   metric = "ROC", 
                   method = "knn",
                   tuneLength = 20,
                   trControl = myControl)

print(knn_model)
plot(knn_model)

# Model 4 - Support vector machine ----
svm_model = train(churn ~ .,
                   churnTrain,
                   metric = "ROC", 
                   method = "svmRadial",
                   tuneLength = 10,
                   trControl = myControl)

print(svm_model)
plot(svm_model)

# Model 5 - Naive Bayes ----
nb_model = train(churn ~ .,
                  churnTrain,
                  metric = "ROC", 
                  method = "naive_bayes",
                  trControl = myControl)

print(nb_model)
plot(nb_model)


#### COMPARING MODELS ----

# We can now use the caret::resamples function that will compare the models and pick the one with 
# the highest AUC and lowest AUC standard deviation.

model_list = list(glmmet = glm_model,
                   rf = rf_model,
                   knn = knn_model,
                   svm = svm_model,
                   nb = nb_model)
resamp = resamples(model_list)


resamp

# results comparison
summary(resamp)

# visual comparison
lattice::bwplot(resamp, metric = "ROC")



## Run pre-processing to see if it will influence the results of one of the models

# svm without pre-process
svm_model1 = train(churn ~ .,
                    churnTrain,
                    metric = "ROC", 
                    method = "svmRadial",
                    tuneLength = 5,
                    trControl = myControl)

# svm with pre-process
svm_model2 = train(churn ~ .,
                    churnTrain[, c(2, 6:20)],
                    metric = "ROC", 
                    method = "svmRadial",
                    preProcess = c("scale", "center", "pca"),
                    tuneLength = 5,
                    trControl = myControl)

model_list = list(svm1 = svm_model1,
                   svm2 = svm_model2)

resamp_svm = resamples(model_list)

summary(resamp_svm)

bwplot(resamp_svm, metric = "ROC")


# Predict using the best model
p = predict(rf_model, churnTest)

confusionMatrix(p, churnTest$churn)