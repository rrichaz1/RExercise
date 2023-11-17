####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

# Load MNIST dataset
# 
# Created by: Yezhou Sun
# Modified by: Marc d. Paradis

# This script is based on 
# https://github.com/apache/incubator-mxnet/blob/master/R-package/vignettes/mnistCompetition.Rmd

library(mxnet)

# Library needed for unzip gunzip file
#install.packages('R.utils', dependencies=T)
library(R.utils)

data.dir <- # Enter directory where MNIST data will be placed
  'C:/Users/mparadis/Desktop/DSU/Immersion College/02 MLM/04. Supervised Classification/4.6 ANN-Based Classification/'

# dir.create(data.dir)

# Helper functions for reading and converting compressed raw image and label 
# files.

load_image_file <- 
  function(filename){
    f <- 
      file(
        filename, 
        'rb'
        )
    readBin(
      f, 
      'integer', 
      n = 1, 
      size = 4, 
      endian = 'big'
      )
    n <- 
      readBin(
        f,
        'integer', 
        n = 1, 
        size = 4, 
        endian = 'big'
        )
    nrow <- 
      readBin(
        f,
        'integer', 
        n = 1, 
        size = 4, 
        endian = 'big'
        )
    ncol <- 
      readBin(
        f,
        'integer', 
        n = 1, 
        size = 4, 
        endian = 'big'
        )
    x <- 
      readBin(
        f, 
        'integer', 
        n = n * nrow * ncol, 
        size = 1, 
        signed = F
        )
    x <-
      matrix(
        x, 
        ncol = nrow * ncol, 
        byrow = T
        )
    close(f)
    x
    }

load_label_file <- 
  function(filename){
    f <- 
      file(
        filename, 
        'rb'
        )
    readBin(
      f,
      'integer', 
      n = 1, 
      size = 4, 
      endian = 'big'
      )
    n <- 
      readBin( f, 
               'integer', 
               n = 1, 
               size = 4, 
               endian = 'big'
               )
    y = readBin(
      f,
      'integer', 
      n = n, 
      size = 1, 
      signed = F
      )
    close(f)
    y
    }

# Download, unzip and convert raw image and label files from Yann LeCunn's 
# website

download.file(
  'http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz', 
  destfile = paste(
    data.dir, 
    'train-images-idx3-ubyte.gz', 
    sep = ''
    )
  )

gunzip(
  paste(
    data.dir, 
    'train-images-idx3-ubyte.gz', 
    sep = ''
    ), 
  remove = F
  )

download.file(
  'http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz',
  destfile = paste(
    data.dir, 
    'train-labels-idx1-ubyte.gz', 
    sep = ''
    )
  )

gunzip(
  paste(
    data.dir, 
    'train-labels-idx1-ubyte.gz', 
    sep = ''
    ), 
  remove = F
  )

download.file(
  'http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz', 
  destfile = paste(
    data.dir, 
    't10k-images-idx3-ubyte.gz', 
    sep = ''
    )
  )

gunzip(
  paste(
    data.dir, 
    't10k-images-idx3-ubyte.gz', 
    sep = ''
    ), 
  remove = F
  )

download.file(
  'http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz', 
  destfile = paste(
    data.dir, 
    't10k-labels-idx1-ubyte.gz', 
    sep = ''
    )
  )

gunzip(
  paste(
    data.dir, 
    't10k-labels-idx1-ubyte.gz', 
    sep = ''
    ), 
  remove = F
  )

train.x <- 
  load_image_file(
    paste(
      data.dir, 
      'train-images-idx3-ubyte', 
      sep = ''
      )
    )

dim(train.x) # 60,000 digits by 784 pixels (28 x 28) each pixel 0 - 255 grey

test.x <- 
  load_image_file(
    paste(
      data.dir, 
      't10k-images-idx3-ubyte', 
      sep = ''
      )
    )

dim(test.x) # 10,000 digits by 784 pixels (28 x 28) each pixel 0 - 255 grey

train.y <- 
  load_label_file(
    paste(
      data.dir, 
      'train-labels-idx1-ubyte', 
      sep = ''
      )
    )

length(train.y) # 60,000 labels (0 - 9 for each digit)

table(train.y) # Check distribution of digits, note 1,300 instance difference
#              # betweem instances with label 1 and instances with label 5

test.y <- 
  load_label_file(
    paste(
      data.dir, 
      't10k-labels-idx1-ubyte', 
      sep = ''
      )
    )

length(test.y) # 10,000 labels (0 - 9 for each digit)

table(test.y) # Check distribution of digits, note 245 instance difference
#             # betweem instances with label 1 and instances with label 5
#             # which approximately mirrors train set i.e. ones are over-
#             # represented and fives are under-represented.

# Visualize a random image from training data

n <- 50
x <- 
  matrix(
    train.x[n,], 
    nrow = 28, 
    ncol = 28
    )
x <- 
  apply(
    x, 
    2, 
    as.numeric
    )
image(
  1:28, 
  1:28, 
  x, 
  col = gray((0:255)/255))

train.y[n]	# Actual label for image

# Output data in text

write.csv(
  train.x, 
  file = paste(
    data.dir, 
    'MNIST_train_x.csv', 
    sep = ''
    ), 
  row.names = F, 
  quote = F
  )

train.y <- 
  data.frame(
    y = train.y
    )

write.csv(
  train.y, 
  file = paste(
    data.dir, 
    'MNIST_train_y.csv', 
    sep = ''
    ), 
  row.names = F, 
  quote = F
  )

write.csv(
  test.x, 
  file = paste(
    data.dir, 
    'MNIST_test_x.csv', 
    sep = ''
    ), 
  row.names = F, 
  quote = F
  )

test.y <- 
  data.frame(
    y = test.y
    )

write.csv(
  test.y, 
  file = paste(
    data.dir, 
    'MNIST_test_y.csv', 
    sep = ''
    ), 
  row.names = F, 
  quote = F
  )

# End