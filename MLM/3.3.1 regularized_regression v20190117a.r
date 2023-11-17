####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456


# regularized_regression.R
# Yezhou Sun
# Marc d. Paradis

options(
  scipen = 999
  )

#install.packages('glmnet')
library(glmnet)

#install.packages('caret')
library(caret)

# In this exercise, we will use data set cars from caret library.
# It has suggested retail price and characteristics of each car.
data(
  cars, 
  package = 'caret'
  )
str(cars)

# Fit a regular linear model for price.
lm.fit <- 
  lm(
    Price ~ ., 
    data = cars
    )

summary(lm.fit)

# Take a look at the results.  What important data prep step did we not do?

# Remember that we are always seeking to either minimize or account for 
# variance with our models.

# Using glmnet for regularized regression.
# For details, please refer to 
# https://web.stanford.edu/~hastie/glmnet/glmnet_beta.html.

# Fit a LASSO regression model.
lasso.fit <- 
  cv.glmnet(
    as.matrix(cars[, -1]), 
    as.matrix(cars[, 1]), 
    family = 'gaussian', 
    type.measure = 'mse', # default = 'deviance'; 'class'; 'auc'; 'mae'
    nfold = 5, 
    alpha = 1 # => Lasso
    )

# Plot the number of predictors used vs MSE.
plot(
  x = lasso.fit, # Note this is actually plot.cv.glmnet which is how R 'knows'
                 # how to plot the fit object
  main = 'Lasso Model' # Note overlap. This is the kind of thing you can spend
                       # hours tweaking (in Excel as easily as R)
  )

# 1st vertical line represents lambda.min, 2nd lambda.1se.
# lambda.1se is considered more robust than lambda.min.
lasso.fit$lambda.min
log(lasso.fit$lambda.min) # Note it is the natural log plotted on x-axis, this
#                         # is an R thing, log = ln, log10 = log base 10

lasso.fit$lambda.1se
log(lasso.fit$lambda.1se)

# Pull out regression coefficients
coef( # Again, actually coef.cv.glmnet
  object = lasso.fit, 
  lambda = 'lambda.1se'
  )

# Note that model eliminated Sound, Saturn, coupe and sedan from predictors.

# Fit a ridge regression model.
ridge.fit <- 
  cv.glmnet(
    x = as.matrix(cars[, -1]), 
    y = as.matrix(cars[, 1]), 
    family = 'gaussian', 
    type.measure = 'mse', # default = 'deviance'; 'class'; 'auc'; 'mae'
    nfold = 5, 
    alpha = 0 # => Ridge
    )

plot(
  x = ridge.fit,
  main = 'Ridge Model')

ridge.fit$lambda.min
log(ridge.fit$lambda.min)

ridge.fit$lambda.1se
log(ridge.fit$lambda.1se)

coef(
  ridge.fit, 
  lambda = 'lambda.1se'
  )

# Note that difference on number of predictors used between LASSO and ridge 
# regressions. Ridge eliminated nothing. ML algos are not magic! Or alternately,
# Magic is Hard.

# Fit a elastic net regression where alpha=0.5.
enet.fit <- 
  cv.glmnet(
    as.matrix(cars[, -1]), 
    as.matrix(cars[, 1]), 
    family = 'gaussian', 
    type.measure = 'mse', 
    nfold = 5, 
    alpha = 0.5
    )

plot(
  x = enet.fit,
  main = 'ENet Model')

enet.fit$lambda.min
log(enet.fit$lambda.min)

enet.fit$lambda.1se
log(enet.fit$lambda.1se)

coef(
  enet.fit, 
  lambda = 'lambda.1se'
  )

# Note Elastic Net eliminated Sedan, Coupe, Saturn, Buick, and Sound. Play
# around with alpha and see what happens - but remember, it is just a linear
# combination of ridge and lasso so you (should) never see anything new.

rm(
  list = ls()
  )

# END