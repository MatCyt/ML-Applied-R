# CARET - Preprocessing
# https://topepo.github.io/caret/pre-processing.html


# SEARCH THROUGH SECTIONS ----
library(caret)
library(earth)



# Creating Dummy Variables ----
# used, saved, loaded AS A MODEL - need to predict()

dummies = dummyVars(survived ~ ., data = etitanic)
head(predict(dummies, newdata = etitanic))



# Identifying Correlated Predictors ----
data(mtcars)

descrCor = cor(mtcars) # find cor matrix

highlyCorDescr = findCorrelation(descrCor, cutoff = .75) # find highly correlated predictors based on cutoff

filteredDescr = mtcars[,-highlyCorDescr] # filter them out - WATCH OUT - it deletes all of them



# Centering and Scaling using PreProcess() ----
preProcValues = preProcess(training, method = c("center", "scale"))

trainTransformed = predict(preProcValues, training)
testTransformed = predict(preProcValues, test)



# Imputation ----
preProcess_missingdata_model = preProcess(training, method='knnImpute')
preProcess_missingdata_model

trainData = predict(preProcess_missingdata_model, newdata = trainData)



# PCA - Transforming Predictors ----

# The preProcess class can apply this transformation by including "pca" in the method argument. 
# Doing this will also force scaling of the predictors. 
# Note that when PCA is requested, predict.preProcess changes the column names to PC1, PC2 and so on



