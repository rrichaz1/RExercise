####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

# Load MNIST dataset Script
# 
# Created by: Yezhou Sun
# Modified by: Marc d. Paradis

data.dir <- 
  'C:/Users/mparadis/Desktop/DSU/Immersion College/02 MLM/04. Supervised Classification/4.6 ANN-Based Classification/'

# Read and process datasets

MNIST.train.x <- 
  read.csv(
    paste(
      data.dir, 
      'MNIST_train_x.csv', 
      sep = ''
    ), 
    header = T,
    sep = ',')

MNIST.train.norm.x <- 
  data.matrix(MNIST.train.x / 255) # Normalizing 0 - 255 to 0 - 1. NNs work 
#                                  # better with 0 - 1 inputs.

MNIST.train.y <- 
  read.csv(
    paste(
      data.dir, 
      'MNIST_train_y.csv', 
      sep = ''
    ), 
    header = T
  )

MNIST.train.vec.y <- 
  MNIST.train.y[,1]

MNIST.test.x <- 
  read.csv(
    paste(
      data.dir, 
      'MNIST_test_x.csv', 
      sep = ''
    ), 
    header = T
  )

MNIST.test.norm.x <- 
  data.matrix(MNIST.test.x / 255) # Normalizing 0 - 255 to 0 - 1. NNs work 
#                                 # better with 0 - 1 inputs.

#is.array(test.x)
#is.matrix(test.x)
#is.vector(test.x)

MNIST.test.y <- 
  read.csv(
    paste(
      data.dir, 
      'MNIST_test_y.csv', 
      sep = ''
    ), 
    header = T
  )

MNIST.test.vec.y <- 
  MNIST.test.y[,1]

#is.array(test.y)
#is.matrix(test.y)
#is.vector(test.y)

# END