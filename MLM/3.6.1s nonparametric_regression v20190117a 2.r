####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

# nonparametric_regression.R 
# Yezhou Sun

# In this exercise, we will run kernel regression using np library
# and Multivariate Adaptive Regression Splines (MARS) using earth library.

#install.packages('np', dependencies=T)
#install.packages('earth', dependencies=T)

library(np)
library(earth)


wrkng.path <- 
  'C:/Users/mparadis/Desktop/DSU/Immersion College/02 MLM/03. Supervised Regression/3.6 NonParametric Regression/'
# We will need this working path later

n <- 100 # Our standard value for n in these scripts, if you have changed it 
#        # elsewhere then update it here too

# Simple example

set.seed(36100)
x <- 
  seq(
    from = 0, 
    to = 1, 
    length = n
    )

set.seed(36101)
y <- 
  sin(2*pi*x) + # y is dependent on x via a sine function
  rnorm(        # random noise is added
    n = n,    # n so length of noise vector = length of x vector
    mean = 0,   # Std Normal error
    sd = 0.5    # Std Normal error
    ) 

plot(
  x = x,
  y = y
  )

fit1 <- 
  npreg(       # Non-parametric Regression
    txdat = x, # Independent variable
    tydat = y, # Dependent variable
    residual = T
    )

summary(fit1) # Just like Chris said, always good to run summary() on any fit
              # object

# Plot data points and fitted line
plot(
  x = x, 
  y = y
  )
lines(
  x = x, 
  y = fitted(fit1), # fitted() is a generic function to extract fitted values
  col = 'red', # Line color = red
  lwd = 2      # Line width = 2
  )

# Wow! That's kind of like magic - and would you ever have thought that you
# could use linear regression to model non-linear data!

# Q-Q plot of residuals

qqnorm(fit1$resid)
qqline(
  fit1$resid, 
  lwd = 2,    # Line Width = 2 
  col = 'red' # Line color = red
  )

# Note that even though data and fit are highly non-linear, the residuals are
# quite linear on the Q-Q Plot which is indicative of normally distributed
# residuals.

# Real data
data(ozone1)	# Ozone readings in LA.
str(ozone1)

plot(ozone1) # Lattice Plot

# Mulitple linear regression
fit.linear <- 
  lm(
    O3 ~ ., 
    data = ozone1
    )

summary(fit.linear)	# Note adjusted R squared and residual standard error

# What other algorithms might you combine with non-parametric regression in
# order to intelligently reduce the number of independent variables.

# Residual Q-Q plot
qqnorm(fit.linear$residuals)
qqline(fit.linear$residuals)

# Kernel regression

print('DO NOT RUN THIS') # This is a note for anyone running this live as the
                         # next several fits will each take some time to run

fit.kernel <- #This will take several minutes to run
  npreg(
    tydat = ozone1$O3, # Dependent variable
    txdat = ozone1[, -1], 
    regtype = 'll', # Local Linear estimator (default is lc = Local Constant)
    residual = T
    )	

save( # Saves R object to a directory of your choice
  fit.kernel, # This can be a list in order to save multiple objects
  file = paste(wrkng.path,'fit.kernel.rda',sep = '')
  )

attach(
  paste(
    wrkng.path,'fit.kernel.rda',sep = '')
  ) # Note does not load object into global environment, loads into a local
    # environment

summary(fit.kernel)	# Note that the improvements on R squared and 
                    # residual standard error.

#plot( # This takes about a minute to run
#  fit.kernel, 
#  plot.errors.method = 'bootstrap'
#  )

# Let's save this plot as a larger png file so that we can see it more clearly
# Alternately, use the zoom button on the plot tab of the display pane
png(
  file = paste(
    wrkng.path,
    'fit.kernel.errors.png',
    sep = ''
  ), 
  width = 800, 
  height = 800
  )

plot( # This takes about a minute to run
  fit.kernel, 
  plot.errors.method = 'bootstrap'
  )

dev.off()

# Note that plotting this outside of the .png file causes formatting issues
# namely that the plot window to the left is too small

qqnorm(fit.kernel$resid)
qqline(fit.kernel$resid) # Note not really a great fit

# MARS - Multiple Adaptive Regression Splines
fit.mars <- # MARS is trademarked, so R cleverly calls it's package earth
  earth(
    O3 ~ ., 
    data = ozone1, 
    nfold = 5
    )

png(
  file = paste(
    wrkng.path,
    'fit.earth.png',
    sep = ''
  ), 
  width = 800, 
  height = 800
)

plot(fit.mars)

dev.off()

summary(fit.mars)

# Variable importance
var.imp <- 
  evimp(fit.mars)
var.imp		# temp is the most important variable for predicting O3 level.

png(
  file = paste(
    wrkng.path,
    'variable.imp.png',
    sep = ''
  ), 
  width = 800, 
  height = 800
)

plot(var.imp)

dev.off()

rm(
  list = ls()
  )

# End