# CARET Data Visualization

# library
library(caret)

# data
data("iris")


# Scatterplot Matrix ----
featurePlot(x = iris[, 1:4],
            y = iris$Species,
            plot = "pairs",
            # add a key at the top
            auto.key = list(columns = 3))


# Scatterplot Matrix with Elipses ----
library(ellipse)

featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "ellipse",
            ## Add a key at the top 
            auto.key = list(columns = 3)) 


# Box Plots ----
featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "box", 
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(4,1 ), 
            auto.key = list(columns = 2))


# Scatter Plots ----
featurePlot(x = iris[, 1:3], 
            y = iris[, 4], 
            plot = "scatter", 
            layout = c(3, 1))


# Density Plots ----
featurePlot(x=iris[,1:4], 
            y=iris[,5], 
            plot="density", 
            scales=list(x=list(relation="free"), 
                        y=list(relation="free")), 
            auto.key=list(columns=3))


