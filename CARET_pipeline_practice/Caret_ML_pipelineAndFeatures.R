# CARET - Pipeline and main features. Sample pipeline
# https://www.machinelearningplus.com/machine-learning/caret-package/

# GOAL: predict which of the two brands of orange juices did the customers buy

# DATASET: https://raw.githubusercontent.com/selva86/datasets/master/orange_juice_withmissing.csv
# 1070 rows, 18 features, 'Purchase' as target variable



# 1. LOAD AND PREPARE --------------------------------------------------------

# 1.1 libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, skimr, RANN, randomForest, fastAdaboost, gbm, xgboost, caretEnsemble, C50, earth)

# 1.2 dataset
oranges = read.csv("./CARET_pipeline_practice/orange_juice_withmissing.csv")

str(oranges)
View(head(oranges))
# purchase CH or MM indicates one of the brands



# 2. DATA PREPARATION AND PREPROCESSING --------------------------------------

# 2.1 Train - Test split

# In caret - createDataPartition. The advantage over traditional sample is that it saves the original proportion of the categories in target variable
# something that can be disturbed in random sampling

# Create training and test datasets
set.seed(123)

# 2.1.1 split index percentage
train_index = createDataPartition(oranges$Purchase, p = 0.8, list = FALSE)

# 2.1.2 Create training and test set
trainData = oranges[train_index, ]
testData = oranges[-train_index, ]

# 2.1.3 Store x and y for later use
x = trainData[, 2:18]
y = trainData$Purchase


# 2.2 Descriptive Statistics

# Skimr package - quick descriptives for each column
library(skimr)
skimmed = skim_to_wide(trainData)
skimmed[, c(1:5, 9:11, 13, 15:16)]


# 2.3 Missing values imputation using preProcess

# To predict the missing values with k-Nearest Neighbors using preProcess():
# 1. You need to set the method=knnImpute for k-Nearest Neighbors and apply it on the training data. This creates a preprocess model.
# 2. Then use predict() on the created preprocess model by setting the newdata argument on the same training data.

# Create the knn imputation model on the training data
preProcess_missingdata_model = preProcess(trainData, method='knnImpute')
preProcess_missingdata_model

# That is, it has centered (subtract by mean) 16 variables, ignored 2, used k=5 (considered 5 nearest neighbors) 
# to predict the missing values and finally scaled (divide by standard deviation) 16 variables.

# Use the imputation model to predict the values of missing data points
library(RANN)  # required for knnInpute
trainData = predict(preProcess_missingdata_model, newdata = trainData)
anyNA(trainData)


# 2.4 Create dummy variables | Up for discussion if this is a necessity in R with factors available

# Change categorical columns into numeric on order to be encoded\
# Dummies should be built ONLY on the training data set - you might get a new values in the test set that would create a new dummy

# In Caret one hot encoding is done via dummyVars()

# One-Hot Encoding
# Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.
dummies_model = dummyVars(Purchase ~ ., data=trainData)

# Create the dummy variables using predict. The Y variable (Purchase) will not be present in trainData_mat.
trainData_mat = predict(dummies_model, newdata = trainData)

# # Convert to dataframe
trainData = data.frame(trainData_mat)

# # See the structure of the new dataset
str(trainData)
# variable Store7 was splitted into two


# 2.5 Variable transformation
# if required

# Preprocessing available in caret
# range: Normalize values so it ranges between 0 and 1
# center: Subtract Mean
# scale: Divide by standard deviation
# BoxCox: Remove skewness leading to normality. Values must be > 0
# YeoJohnson: Like BoxCox, but works for negative values.
# expoTrans: Exponential transformation, works for negative values.
# pca: Replace with principal components
# ica: Replace with independent components
# spatialSign: Project the data to a unit circle

# Converting all numeric values to range between 0 and 1 - method = range
preProcess_range_model = preProcess(trainData, method='range')
trainData = predict(preProcess_range_model, newdata = trainData)

# Append the Y variable
trainData$Purchase = y


# 3. VISUALIZATION - featurePlot() -------------------------------------------

# Feature importance - how predictors influence x

# In this problem, the X variables are numeric whereas the Y is categorical. So how to gauge if a given X is an important predictor of Y?
# A simple common sense approach is, if you group the X variable by the categories of Y, a significant mean shift amongst
# the X’s groups is a strong indicator (if not the only indicator) that X will have a significant role to help predict Y.

featurePlot(x = trainData[, 1:18], 
            y = trainData$Purchase, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)), # label font size
            scales = list(x = list(relation="free"), # free scales
                          y = list(relation="free")))

# Density plots
# In this case, For a variable to be important, I would expect the density curves to be significantly different
# for the 2 classes, both in terms of the height (kurtosis) and placement (skewness).
   
featurePlot(x = trainData[, 1:18], 
            y = trainData$Purchase, 
            plot = "density",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))




# 4. FEATURE SELECTION - RECURSIVE FEATURE ELIMINATION -----------------------

# how does recursive feature elimination work?
# 1. Build a ML model on a training dataset and estimate the feature importances on the test dataset.
# 2. Keeping priority to the most important variables, iterate through by building models of given subset sizes, 
# that is, subgroups of most important predictors determined from step 1. Ranking of the predictors is recalculated in each iteration.
# 3. The model performances are compared across different subset sizes to arrive at the optimal number and list of final predictors.

# implemented through rfe function - tell rfe which algorithm to use and how to cross validate it

set.seed(100)
options(warn=-1)

subsets = c(1:5, 10, 15, 18)

ctrl = rfeControl(functions = rfFuncs, # specify the model used, here Random Forest
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile = rfe(x=trainData[, 1:18], y=trainData$Purchase,
                 sizes = subsets, # determines what all model sizes (the number of most important features) the rfe will use. like 1 to 5, 10, 15, 18
                 rfeControl = ctrl) # rfeControl parameter on the other hand receives the output of the rfeControl() as values.

lmProfile

# here it seems that model with only 3 variables outperform model with 18




# 5. TRAINING AND TUNING THE MODEL ----------------------------------------

names(getModelInfo()) # all the models
modelLookup('rf') # check model details

# 5.1 Training and interpreting the results

# using Multivariate Adaptive Regression Splines (MARS) using method = 'earth'
modelLookup('earth')

# Set the seed for reproducibility
set.seed(100)

# Train the model using randomForest and predict on the training data itself.
model_mars = train(Purchase ~ ., data=trainData, method='earth')
fitted = predict(model_mars)

# HOW IS USING TRAIN() DIFFERENT THAN USING JUST ALGORITHM FUNCTION DIRECTLY?
# on top of building the model, train() can do also:
# 1. Cross validation
# 2. Parameter tuning
# 3. Choose the optimal model based on a given evalueation metric
# 4. Preprocess predictors

model_mars
# hyper parameters (number of trees and interaction depths were tested and basic cross validation was applied)

plot(model_mars, main="Model Accuracies with MARS")


# 5.2 Variable importance
varimp_mars = varImp(model_mars)
plot(varimp_mars, main="Variable Importance with MARS")


# 5.3 Prepare the test dataset and predict

# Now in order to use the model to predict on new data, the new data has to be preprocessed and transformed 
# just the way we did on the training data.

# Thanks to caret, all the information required for pre-processing is stored in the 
# respective preProcess model and dummyVar model.
# So if you used CARET "models" for processing you can just re-use them on the test set, quick and easy

# You need to pass the testData through these models in the same sequence as you did last time:
# preProcess_missingdata_model –> dummies_model –> preProcess_range_model

# Step 1: Impute missing values 
testData2 = predict(preProcess_missingdata_model, testData)  

# Step 2: Create one-hot encodings (dummy variables)
testData3 = predict(dummies_model, testData2)

# Step 3: Transform the features to range between 0 and 1
testData4 = predict(preProcess_range_model, testData3)

# View
head(testData4[, 1:10])


# 5.4 PREDICT ON TEST DATA
predicted = predict(model_mars, testData4)
head(predicted)


# 5.5 
