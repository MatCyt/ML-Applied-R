# LOGISTIC REGRESSION - ITSL

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


# Logistic Regression

glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)

summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]

glm.probs = predict(glm.fits,type="response")

# confusion matrix
glm.pred = rep("Down",1250)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction) 

mean(glm.pred == Direction) # accuracy

# Training and test set

train=(Year<2005) # create filter for the dataset

Smarket.2005 = Smarket[!train,] # test
Direction.2005 = Direction[!train]

# refit the model
glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)

glm.probs = predict(glm.fits,Smarket.2005,type="response") # predict on new data

# evaluate - confuction matrix for test set
glm.pred = rep("Down",252)
glm.pred[glm.probs>.5] = "Up"
table(glm.pred,Direction.2005)

mean(glm.pred==Direction.2005) # error rate
mean(glm.pred!=Direction.2005)
# Overfitting

# Hence run a smaller model
glm.fits = glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)

glm.probs=predict(glm.fits,Smarket.2005,type="response")

glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005) # smaller error rate

summary(glm.fits) # still nothing is significant

predict(glm.fits,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")