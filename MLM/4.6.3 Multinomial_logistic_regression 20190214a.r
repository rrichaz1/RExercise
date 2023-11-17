####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

# Multinomial Regression via Perceptron Equivalency on MNIST Dataset
# 
# Created by: Yezhou Sun
# Modified by: Marc d. Paradis

# install.packages('nnet', dependencies=T)
library(nnet)

data.dir <- 
  'C:/Users/mparadis/Desktop/DSU/Immersion College/02 MLM/04. Supervised Classification/4.6 ANN-Based Classification/'

source(
  file = paste(
    data.dir,
    '4.6.2 Load_MNIST_objects 20181016a.r', # Note probably a bad idea to hard
#                                           # code the date in the filename
    sep = ''
  )
)

# Read and process datasets

MNIST.multilog.train <- 
  cbind(
    MNIST.train.norm.x, 
    y = MNIST.train.y
    )

#is.data.frame(MNIST.multilog.train)
#is.array(MNIST.multilog.train)
#is.matrix(MNIST.multilog.train)
#is.vector(MNIST.multilog.train)

#MNIST.multilog.train$y

MNIST.multilog.train$y <- 
  factor(
    MNIST.multilog.train$y, 
    levels = sort(
      unique(MNIST.multilog.train$y)
      )
    )

MNIST.multilog.test <- 
  cbind(
    MNIST.test.norm.x, 
    y = MNIST.test.y)

MNIST.multilog.test$y <- 
  factor(
    MNIST.multilog.test$y, 
    levels = sort(
      unique(MNIST.multilog.test$y)
      )
    )

# FOLLOWING TWO STATEMENTS TAKE LONG TIME AND LARGE CHUNK OF RAM TO COMPLETE

MNIST.multilog.start <-
  Sys.time()
MNIST.multilog.m <- 
  multinom(
    formula = y ~ ., 
    data = MNIST.multilog.train, 
    MaxNWts = 1000000
    )
MNIST.multilog.end <-
  Sys.time()
MNIST.multilog.run <- # Takes about 12 minutes on my laptop
  MNIST.multilog.end - MNIST.multilog.start

# Takes A LOT longer to run than training
# summary(MNIST.multilog.m)

predicted_scores <- 
  predict(
    object = MNIST.multilog.m, 
    newdata = MNIST.multilog.test, 
    type = 'probs' # 'raw', 'class'
    )

predicted_class <- 
  predict(
    object = MNIST.multilog.m, 
    newdata = MNIST.multilog.test
    )

# Confusion matrix
table(
  predicted_class, 
  MNIST.multilog.test$y
  )

# Accurancy on test data
1 - mean(as.character(predicted_class) != as.character(MNIST.multilog.test$y))

# END