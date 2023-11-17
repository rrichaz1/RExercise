####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

# naive_bayesian_classification.R
# Yezhou Sun

# Use the implementation of naive bayesian classification from e1071 R library

#install.packages('e1071', dependencies=T)
library(e1071)

# Use iris data set 
data('iris')
str(iris)
summary(iris)

plot(iris)

set.seed(45100) 

# Create training and test data
idx <- # Creates an index
  sample( # on a random
    seq(
      1, 
      nrow(iris)
      ), 
    floor(0.67 * nrow(iris)), # Two-thirds of the iris units
    replace = F
    )

training.data <- 
  iris[idx, ] # Two-thirds

test.data <- 
  iris[-idx, ] # One-third

# Create naive bayesian classification model for Species, using other variables 
# as predictors.
m.bayes <- 
  naiveBayes(
    x = training.data[, -5], # Explanatory Features
    y = training.data[, 5] # Labeled Target
    )

str(m.bayes)
m.bayes
summary(m.bayes)

# Prior distribution of Species
m.bayes$apriori

# Since all predictors are continuous, naive bayesian classifier generated 
# Gaussian distributions of each class for each predictor. 

# Distributions of sepal length

m.bayes$tables$Sepal.Length

# Plot these distributions.
plot(
  function(x) dnorm(
    x = x, 
    mean = m.bayes$tables$Sepal.Length[1,1], # Setosa mean
    sd = m.bayes$tables$Sepal.Length[1,2] # Setosa sd
    ), 
  3, # lower bound of anonymous function
  10, # upper bound of anonymous function
  col = 'red', 
  main = 'Conditional sepal length distribution for the 3 species', 
  ylab = 'PDF', 
  xlab = 'Sepal Length'
  )
curve(
  dnorm(
    x = x, 
    mean = m.bayes$tables$Sepal.Length[2,1], # Versicolor mean
    sd = m.bayes$tables$Sepal.Length[2,2] # Versicolor sd
    ), 
  add = T, 
  col = 'blue'
  )
curve(
  dnorm(
    x = x, 
    mean = m.bayes$tables$Sepal.Length[3,1], # Virginica mean
    sd = m.bayes$tables$Sepal.Length[3,2] # Virginica sd
    ), 
  add = T, 
  col = 'green'
  )

# If predictor is discrete, mosaicplot() can be used to visualize distributions.

# Test model performance using test data
pred.bayes <- 
  predict(
    object = m.bayes, 
    newdata = test.data[, -5]
    )

table(
  x = pred.bayes, 
  data = test.data[, 5], 
  dnn = list('predicted', 'actual') # Names for Rows and Columns
  )

rm(
  list = ls()
  ) # Clears global environment of all visible objects (some hidden objects
#   # may remain)

# End