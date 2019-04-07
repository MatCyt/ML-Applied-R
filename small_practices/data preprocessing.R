# Data Preprocessing small practice
# https://lgatto.github.io/IntroMachineLearningWithR/supervised-learning.html

#### Missing values ----

# Two base approaches - drop the observations (or whole feature if it is one feature with high concentration of NAs)
# When the proportion of NAs is higher - impute them


# Let’s start by simulating a dataset containing missing values using the mtcars dataset. 
# Below, we will want to predict the mpg variable using cyl, disp, and hp, with the latter containing 10 missing values.

data(mtcars)

mtcars[sample(nrow(mtcars), 10), "hp"] = NA # 10 random missing values

Y = mtcars$mpg    ## target variable
X = mtcars[, 2:4] ## predictors

try(train(X, Y))
?try

# Caret offers several methods for handling (imputing) missing values
# Imputing using caret also allows to optimise the imputation based on the cross validation splits, 
# as train will do median imputation inside each fold.

### Median imputation ----
train(X, Y, preProcess = "medianImpute")

train(X, Y, preProcess = "knnImpute")


#### Scaling and centring ----

train(X, Y, preProcess = "scale")

train(X, Y, preProcess = "center")

train(X, Y, preProcess = "pca")


#### Multiple pre-processing methods ----

train(X, Y, preProcess = c("medianImpute", "center", "scale", "pca"))


?preProcess
