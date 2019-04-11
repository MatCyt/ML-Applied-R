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
factor_vars = c('Pclass','Sex','Embarked')
full[factor_vars] = lapply(full[factor_vars], function(x) as.factor(x))

# simple imputation of NAs in age with a mean
full$Age[is.na(full$Age)] = mean(full$Age, na.rm = T)

# simple imputation of NAs in Fare - median
full[is.na(full$Fare),] # 1 row

full$Fare[is.na(full$Fare)] = median(full$Fare, na.rm = T)

# simple imputation of NAs in Embarked
sum(is.na(full$Embarked))
full[is.na(full$Embarked), ]

table(full$Embarked, full$Pclass)

full$Embarked[is.na(full$Embarked)] = "S"

### run several models for comparison
fdfs


### small tuning


### predict and save results
