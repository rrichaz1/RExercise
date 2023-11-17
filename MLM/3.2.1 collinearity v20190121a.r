####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

#Marc d. Paradis
#R 3.5.1 for Windows
#RStudio 1.1.456


# collinearity.R
# Yezhou Sun/Marc d. Paradis

# Exact collinearity

# Create independent and dependent variables
set.seed(32100)

x1.col <- 
  rnorm(
    n = 1000, 
    mean = 80, 
    sd = 6
    )

set.seed(32101)

x2.col <- 
  rnorm(
    n = 1000, 
    mean = 100, 
    sd = 8
    )

# Note that x1.col and x2.col are technically independent of each other and
# represent what is called a bivariate (two variables) normal (Gaussian/Bell
# curve) distribution.  Bivariate Normal Distributions are usually independent
# we are now about to create a third, perfectly collinear variable:

x3.col <- 
  3*x1.col + 2*x2.col

set.seed(32102)

y1.col.the <- 
  5 + # Intercept: Theta0
  x1.col - x2.col + # Coefficients: Theta1 and Theta2
  rnorm( # Residuals normally distributed around the response variable
    n = 1000, 
    mean = 0, 
    sd = 4
    )

df.3lr.col.obs <- 
  data.frame(
    y1.col.the,
    x1.col, 
    x2.col, 
    x3.col
    )

head(
  x = df.3lr.col.obs, 
  n = 10
  )

cor(df.3lr.col.obs) # Some pretty strong correlations

plot(df.3lr.col.obs) # Even more obvious when visualized. Note even if you
#                    # knew nothing about the code above, looking at this data
#                    # it is pretty easy to see that x1.col and x2.col are
#                    # likely to be independent and that y1.col.the and x1.col
#                    # seem also largely independent.

pairs(df.3lr.col.obs) # Plots the same thing as above, but is a more explicit
#                     # call

# To make this qualitatively and quantitatively even clearer, let's use the
# cor.plot function from the psych package

library(psych)

cor.plot(
  r = cor( # A correlation matrix
    x = df.3lr.col.obs,
    use = 'everything',
    method = 'pearson'  # Others: 'kendall' or 'spearman'
    ),
  numbers = TRUE # Display the numeric value of the correlations
  )

m.3lr.col.obs <- 
  lm(
    y1.col.the ~ x1.col + x2.col + x3.col, # Note incorrectly specified model
    data = df.3lr.col.obs
    )

summary(m.3lr.col.obs) # Note that x3 was removed during fitting. Why is this?

x.temp <- 
  cbind(
    1, 
    as.matrix(
      df.3lr.col.obs[,2:4]
      )
    )

head(
  x = x.temp, 
  n = 10
  )

solve(t(x.temp) %*% x.temp)

# Try to find regression coefficient betas using t(X) %*% X but it's not 
# solvable because linear correlation.

m1.3lr.col.obs <- 
  lm(
    y1.col.the ~ x1.col + x2.col, 
    data = df.3lr.col.obs
    )

summary(m1.3lr.col.obs)

m2.3lr.col.obs <- 
  lm(
    y1.col.the ~ x1.col + x3.col, # Endogeneity and Collinearity
    data = df.3lr.col.obs
    )

summary(m2.3lr.col.obs) # Note how far off coefficients are, even though
#                       # intercept is largely accurate.  But you just don't
#                       # know

m3.3lr.col.obs <- 
  lm(
    y1.col.the ~ x2.col + x3.col, # Endogeneity and Collinearity
    data = df.3lr.col.obs
    )

summary(m3.3lr.col.obs) # Note how far off coefficients are, even though
#                       # intercept is largely accurate.  But you just don't
#                       # know

rm(
  list = ls()
  )

# END
