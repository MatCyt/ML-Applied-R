### Simple submission to try out caret workflow and model comparison

### libraries
library(caret)
library(dplyr)
library(pROC)
library(readr)

### data
train = read_csv('./Titanic/data/train.csv')
test  = read_csv('./Titanic/data/test.csv')

# merge
test$Survived = NA
full = rbind(train, test)

# quick assessment - reminder
str(full)
summary(full)



### simple data preprocessing

# change into factor

# simple imputation of NAs in age

# simple imputation of NAs in Fare 

# simple imputation of NAs in Embarked



### run several models for comparison

