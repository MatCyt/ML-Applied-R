# K-Nearest Neighbors - ITSL

# The Stock Market Data ----

# Load Library - datasets
library(ISLR)

# Explore dataset - stock data
names(Smarket)
summary(Smarket)
head(Smarket)


pairs(Smarket) # old graphics but great exploration function
pairs(Smarket, col = Smarket$Direction) # color by binary feature values

cor(Smarket[,-9]) # correlations

attach(Smarket)
plot(Volume)


# K-Nearest Neighbors ----

library(class)

?knn

Xlag = cbind(Lag1, Lag2)
train = Year < 2005

train.X = Xlag[train,]

test.X = Xlag[!train,]

train.Direction = Direction[train]

set.seed(1)
knn.pred = knn(train.X,test.X,train.Direction,k=1) # knn with one neighbor

table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])


knn.pred=knn(train.X,test.X,train.Direction,k=3) # knn with three neighbor
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)
