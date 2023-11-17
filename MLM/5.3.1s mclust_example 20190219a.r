####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

# Based on code by Yezhou Sun

# Reference for mclust package: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5096736/pdf/nihms793803.pdf

#install.packages(c('mclust','factoextra'), dependencies=T)

library(mclust)

library(factoextra)

data(iris)

str(iris)

d <- 
  iris[, 1:4]

dim(d)

cl <- 
  iris[, 5]

plot(iris)

# Clustering
m1 <- 
  Mclust(d)

summary(
  m1, 
  parameters = T
  )

coordProj(
  data = d, 
  dimens = c(2,4), # Sepal.Width v. Petal.Width
  what = 'uncertainty',
  parameters = m1$parameters, 
  z = m1$z
  )

coordProj(
  data = d, 
  dimens = c(2,3), # Sepal.Width v. Petal.Length
  what = 'uncertainty',
  parameters = m1$parameters, 
  z = m1$z
  )

coordProj(
  data = d, 
  dimens = c(3,4), # Petal.Length v. Petal.Width
  what = 'uncertainty',
  parameters = m1$parameters, 
  z = m1$z
  )

# BIC plot
fviz_mclust(
  m1, 
  what = 'BIC', 
  palette = 'npg'
  )

m1.ICL <- 
  mclustICL(d)

summary(m1.ICL)

# Cluster plot
fviz_mclust(
  m1, 
  'classification', 
  geom = 'point',  
  pointsize =1.5, 
  palette = 'npg'
  )

# Cluster membership of each row in d
m1$classification

# fix G=3
m1.3 <- 
  Mclust(
    d, 
    G = 3 # Components
    )

coordProj(
  data = d, 
  dimens = c(2,4), 
  what = 'uncertainty',
  parameters = m1.3$parameters, 
  z = m1.3$z
  )

coordProj(
  data = d, 
  dimens = c(2,3), 
  what = 'uncertainty',
  parameters = m1.3$parameters, 
  z = m1.3$z
  )

coordProj(
  data = d, 
  dimens = c(3,4), 
  what = 'uncertainty',
  parameters = m1.3$parameters, 
  z = m1.3$z
  )

require(data.table)

setDT(iris)

#investigate
iris[ ,un2 := m1$uncertainty]

fivenum(iris$un2)

iris[ ,un3 := m1.3$uncertainty]

fivenum(iris$un3)

# z are the posterior class probs
iris[ ,z2.1 := m1$z[,1] ]

iris[ ,z2.2 := m1$z[,2] ]

iris[ ,z3.1 := m1.3$z[,1] ]

iris[ ,z3.2:=m1.3$z[,2] ]

iris[ ,z3.3:=m1.3$z[,3] ]

iris[Sepal.Width < 2.5 & Petal.Width < 0.5]


# Bootstrap inference of best model
boot1 <- 
  MclustBootstrap(
    m1, 
    G = m1$G, 
    nboot = 999, 
    type = 'bs', 
    modelName = m1$modelName
    )

summary(
  boot1, 
  what = 'se'	# Standard error
  )

summary(
  boot1, 
  what = 'ci' # Confidence interval
  )	

# Supervised Classification
m2 <- 
  MclustDA( # Discriminant Analysis based on Gaussian Finite Mixture Model
    data = d, 
    class = cl, # Label vector for training data 
    modelType = 'EDDA'
    )

summary(
  m2, 
  parameters = T
  )

# Data points marked by known classes. Ellipses represent covariance structures.
plot(
  m2, 
  what = 'scatterplot'
  )

# Data points marked by predicted classes
plot(
  m2, 
  what = 'classification'
  )

# Correctly and incorrectly marked data points
plot(
  m2, 
  what = 'error', 
  dimens = 3:4
  )

# Cross validation
cv <- 
  cvMclustDA( # k-fold cross-validation of MclustDA
    object = m2, 
    nfold = 10
    )

cv

cv$error

cv$se

# Density estimation
m3 <- 
  densityMclust( # density estimate for each data point using Gaussian finite
                 # mixture model
    data = d
    )

summary(
  m3, 
  parameters = T
  )

fviz_mclust(
  m3, 
  what = 'BIC', 
  palette = 'npg'
  )

# Density plots
plot( # Takes ~30 seconds to plot on my laptop
  m3, 
  what = 'density', 
  type = 'image', 
  grid = 100
  )

# Densities
m3$density

rm(
  list = ls()
  )

# End