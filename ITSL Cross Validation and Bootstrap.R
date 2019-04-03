# ITSL Cross-Validation and the Bootstrap



# The Validation Set Approach ----

# library
library(ISLR)
library(boot)

### linear models
attach(Auto)

# train set
set.seed(1)
train = sample(392,196)
?sample

lm.fit = lm(mpg~horsepower,data = Auto,subset = train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)


lm.fit2 = lm(mpg~poly(horsepower,2),data = Auto,subset = train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)


lm.fit3 = lm(mpg~poly(horsepower,3),data = Auto,subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

# different seed and sample
set.seed(2)
train = sample(392,196)


lm.fit = lm(mpg~horsepower,subset = train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2 = lm(mpg~poly(horsepower,2),data = Auto,subset = train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)


lm.fit3 = lm(mpg~poly(horsepower,3),data = Auto,subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)


# Leave-One-Out Cross-Validation ----

glm.fit = glm(mpg~horsepower,data = Auto)
coef(glm.fit)

lm.fit = lm(mpg~horsepower,data = Auto)
coef(lm.fit)


glm.fit = glm(mpg~horsepower,data = Auto) # glm in linear version 

cv.err = cv.glm(Auto,glm.fit) # Leave one out -  cv.glm use model as input

cv.err$delta # cross validated prediction error
# first number is the looc error
# 2nd is bias corrected version of it


# SELECT BEST polynomial degree for non linear relationship - store the cv error results and plot them
cv.error = rep(0,5) 
for (i in 1:5){
 glm.fit = glm(mpg~poly(horsepower,i),data = Auto)
 cv.error[i] = cv.glm(Auto,glm.fit)$delta[1]
 }
cv.error

x = 1:5
plot(x, cv.error, type ='b') 


# k-Fold Cross-Validation ----


# 10 fold cross validation
set.seed(17)

cv.error.10 = rep(0,10)
for (i in 1:10){
  
 glm.fit = glm(mpg~poly(horsepower,i),data = Auto)
 
 cv.error.10[i] = cv.glm(Auto,glm.fit,K = 10)$delta[1]
 }
cv.error.10



# The Bootstrap ----

alpha.fn = function(data,index){
 X = data$X[index]
 Y = data$Y[index]
 return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
 }
alpha.fn(Portfolio,1:100)
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace = T))
boot(Portfolio,alpha.fn,R = 1000)

# Estimating the Accuracy of a Linear Regression Model

boot.fn = function(data,index)
 return(coef(lm(mpg~horsepower,data = data,subset = index)))
boot.fn(Auto,1:392)
set.seed(1)
boot.fn(Auto,sample(392,392,replace = T))
boot.fn(Auto,sample(392,392,replace = T))
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower,data = Auto))$coef
boot.fn = function(data,index)
 coefficients(lm(mpg~horsepower+I(horsepower^2),data = data,subset = index))
set.seed(1)
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data = Auto))$coef

