# Chapter 3 Lab: Linear Regression

# load library
library(MASS) # datasets
library(ISLR) # datasets

# Simple Linear Regression

# load dataset
df = Boston

?Boston
fix(Boston)
names(Boston)


#### linear regression ----

# plot variables
plot(medv ~ lstat, Boston)

# medv as target variable - median value of owner-occupied homes in \$1000s.

attach(Boston) # so that we don't have to specify dataset or colnames with $

# run regression
lm.fit1 = lm(medv~lstat)

lm.fit1
summary(lm.fit1)

# regression components
names(lm.fit1)
coef(lm.fit1)
confint(lm.fit1)

# plot the regression model
abline(lm.fit1)

abline(lm.fit1,lwd=1,col="red")

# predict with regression values
predict(lm.fit1,data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(lm.fit1,data.frame(lstat=(c(5,10,15))), interval="prediction")


#### Visualize regression

# Regression itself - relation between variables
plot(medv ~ lstat, Boston)
abline(lm.fit1,lwd=1,col="red") 

# Visualize Regression results
par(mfrow=c(2,2))
plot(lm.fit1)
# in the residuals vs. fitted we look for non-linearity

plot(predict(lm.fit1), residuals(lm.fit1))
plot(predict(lm.fit1), rstudent(lm.fit1))
plot(hatvalues(lm.fit1))
which.max(hatvalues(lm.fit1))

#### Multiple Linear Regression ----


# MLP Boston
lm.fit2=lm(medv~lstat+age,data=Boston)
summary(lm.fit2)


lm.fit3=lm(medv~.,data=Boston)
summary(lm.fit3)

# Visualize the results
par(mfrow=c(2,2))
plot(lm.fit3)

lm.fit4=update(lm.fit3, ~.-age) # update model


# Interaction Terms

summary(lm(medv~lstat*age,data=Boston)) # adding interaction between age and lstat (and keeping both of them)

lm.fit_interaction = lm(medv~lstat*age,data=Boston)
summary(lm.fit_interaction)

# Non-linear Transformations of the Predictors

lm.fit6=lm(medv~lstat+I(lstat^2))
summary(lm.fit6)

par(mfrow=c(2,2))
plot(lm.fit6)

plot(medv~lstat)
points(lstat, fitted(lm.fit6), col = "red", pch = 10) # visualize non linear fit


# anova
lm.fit7=lm(medv~lstat)
anova(lm.fit7,lm.fit6)


# fit polynomial
lm.fit9=lm(medv~poly(lstat,5))
summary(lm.fit9)
summary(lm(medv~log(rm),data=Boston))

# Qualitative Predictors

Carseats = ISLR::Carseats

fix(Carseats)
names(Carseats)
summary(Carseats)

# lm model
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)
attach(Carseats)

contrasts(ShelveLoc) # show how categorical variables were coded

# Writing Functions

LoadLibraries
LoadLibraries()
LoadLibraries=function(){
 library(ISLR)
 library(MASS)
 print("The libraries have been loaded.")
 }
LoadLibraries
LoadLibraries()


regplot = function(x,y,...) { 
  fit = lm(y~x)
  plot(x,y,...)
  abline(fit, col = "red")
  }

regplot(Price, Sales, xlab = "Price", col = "blue", pch = 20)
