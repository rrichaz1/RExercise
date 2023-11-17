####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

# Install mxnet R library for CPU
# 
# Created by: Yezhou Sun
# Modified by: Marc d. Paradis

# This script is based on 
# https://github.com/apache/incubator-mxnet/tree/master/R-package

# Also in the 1.2.2 R Install Script from MLM

cran <- 
  getOption('repos')

cran['dmlc'] <- 
  'https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/'
options(
  repos = cran
  )
install.packages('mxnet')

library(mxnet)

# End


