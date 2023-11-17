####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

#Marc d. Paradis
#R 3.5.1 for Windows
#RStudio 1.1.456

# multivariable_linear_regression.R
# Yezhou Sun

data(mtcars)	# mtcars is a data set built in R installation.
str(mtcars)

# Fit a model for mpg using cyl, disp, hp, and wt variables
m1 <- 
  lm(
    mpg ~ cyl + disp + hp + wt, 
    data = mtcars
    )

str(m1)
summary(m1)

# Diagnostic plots
layout(
  mat = matrix(
    data = c(1, 2, 3, 4), 
    nrow = 2, 
    ncol = 2
    )
  )

plot(m1)

layout(
  mat = matrix(
    data = 1, 
    nrow = 1, 
    ncol = 1
  )
)

# Coefficients
m1$coefficients

# 95% confidence intervals of coefficients
confint(
  m1, 
  level = 0.95)

# Fit another model
m2 <- 
  lm(
    mpg ~ cyl + disp + hp, 
    data = mtcars
    )

summary(m2)

# Compare two models
anova(m1, m2)
# The two models are significantly different (p=0.0007589) and m1 is much better
# than m2 based on residual sum of squares (RSS).

# Stepwise variable selection
#install.packages('MASS', dependencies=T)
library(MASS)

m3 <- 
  lm(
    mpg ~ ., #Remember that ~ . means against all other columns in the dataset
    data = mtcars
    )

selection <- 
  stepAIC(
    m3, 
    direction = 'both'	#Both forward and backward selection
    )

selection$anova

rm(
  list = ls()
  ) # Clears global environment of all visible objects (some hidden objects
    # may remain)

# End
