# An Application to Caravan Insurance Data

# Dataset library
library(ISLR)

# Explore dataset
?Caravan
head(Caravan)
attach(Caravan)

summary(Purchase) # non balanced set


standardized.X = scale(Caravan[,-86])

var(Caravan[,1])
var(Caravan[,2]) # huge variance across variables - scale them
var(standardized.X[,1])
var(standardized.X[,2])

# test train
test = 1:1000 

train.X = standardized.X[-test,] # all variables BUT target (standardized got rid of column 86)
test.X = standardized.X[test,]

train.Y = Purchase[-test] # target variable
test.Y = Purchase[test]

# KNN
set.seed(1)
knn.pred = knn(train.X,test.X,train.Y,k=1)
mean(test.Y != knn.pred)
mean(test.Y != "No")
table(knn.pred,test.Y)

knn.pred = knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)

knn.pred = knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)


# Logistic regression
 
glm.fits = glm(Purchase~.,data=Caravan,family=binomial,subset=-test)

glm.probs = predict(glm.fits,Caravan[test,],type="response")
glm.pred=rep("No",1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)
glm.pred=rep("No",1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred,test.Y)
