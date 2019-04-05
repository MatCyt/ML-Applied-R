# kNN from Class on Iris

# library
library(class)
library(dplyr)

# data
df = data("iris")

# run
set.seed(123)

tr = sample(150, 50) # 50 random samples from iris
ts = sample(150, 50) 

knnres = knn(iris[tr, -5], iris[ts, -5], iris$Species[tr]) #knn(train without class, test without class, class from train)
head(knnres)

# knn returns by default predicted classes

# compare the predicted outcome (class from train) to the actual values in confusion matrix (by class from test)
table(knnres, iris$Species[ts])

# simple accuracy - how many out of knn predictors are the same as test class values
mean(knnres == iris$Species[ts])


# we can set the amount of nearest neighbours that needs to be considered to assign a class
args(knn) # it is 1 by default

# let's compare different options, values for neighbours from 1 to 10
neighbours = 1:10
comp = data.frame(k = 0, accuracy = 0)

for (i in neighbours) {
  knnres_loop = knn(iris[tr, -5], iris[ts, -5], iris$Species[tr], k = i)
  accuracy = mean(knnres_loop == iris$Species[ts])
  comp[i, ] = c(i, accuracy)}

# show best values for k
comp %>%
  filter(accuracy == max(accuracy))
