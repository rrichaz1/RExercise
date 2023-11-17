####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

# Based on: 3.2 Endogeneity and Collinearity
# By: Marc d. Paradis

n <- 100 # Set observed sample size (Number of experimental units in each
         # observed sample)

r <- 10 # Set observed re-sample size (Number of times observed sample of 
        # size n drawn from population of size N)

theta.x0 <- 35 # Set ground truth y-intercept parameter for linear model

unif.min <- 0 # minimum value of continuous uniform distribution

unif.max <- 1000 # maximum value of continuous uniform distribution

set.seed(310100) # Note that set.seed interprets the argument as an integer. 
                 # The convention I use is to take the file number and add two
                 # zeros to the end. Setting the seed immediately before the 
                 # generation of random variates (e.g. runif, rnorm, or any
                 # function which has an embedded random generation such as
                 # Random Forest, k-nearest neighbors or k-means)

x1.ind <- # Random variable vector of independent x1 values
  runif( # pulled from a random uniform distribution
    n = n, # of sample size n
    min = unif.min, # with minimum value of 0 (35 + (1.5 *0)) + 10 = 45
    max = unif.max # and maximum value of 1,000 (35 + (1.5 *1000)) + 10 = 1,545
  )

is.vector(x1.ind) # TRUE

mean(x1.ind) # 506.9302
sd(x1.ind) # 287.9923

plot( # Plot...
  x = x1.ind # ...x1.ind random variates by index
  ) 
abline( # add a...
  h = mean(x1.ind) # ...horizontal line with y-intercept of mean(x1.ind)
  )

plot( # Plot...
  sort( # ...sorted by...
    x = x1.ind, # ...x1.ind random variates...
    decreasing = FALSE # ...in increasing value
    ) 
  ) # Note wobble due to random sampling

hist(
  x = x1.ind
  ) # Plot a histogram of random variates

theta.x1.ind <- 1.5 # Set ground truth linear coefficient for independent x1
                    # variable

y1.slr.the <- # Theoretical value of dependent RV y as a simple linear model of
              # independent RV x1
  theta.x0 + (theta.x1.ind * x1.ind) 

df.slr.ind.the <-
  as.data.frame(
    cbind(
      y1.slr.the,
      x1.ind
    )
  )

m.slr.ind.the <-
  lm(
    y1.slr.the ~ x1.ind,
    data = df.slr.ind.the
  )

# Assumption #1: Linearity of Parameters
# y1.slr.the = theta.x0 + (theta.x1.ind * x1.ind)
summary(m.slr.ind.the) # Perfect fit because No noise or bias in equation

summary(m.slr.ind.the)$r.squared # Can't get more perfect than 1

plot(
  y1.slr.the ~ x1.ind,
  data = df.slr.ind.the
  )
abline(m.slr.ind.the) # R "understands" m.slr.ind.the is an object for a linear
                      # model and plots the best fit line

# Note x1.ind is NOT evenly spaced, this is due to random sampling from a 
# uniform distribution. Each x value is exact (an implicit assumption of OLS).

# Assumption #2: Random Sample

# Review of distributional modeling

set.seed(310101) # Next number in sequence

unif.rv.exp <- # random variates (rv) are modeled  (expected); Values of a 
               # Random Variable (RV) are observed
  runif( # n random variates from a uniform distribution
    n = n, 
    min = unif.min, 
    max = unif.max
    ) 

plot(
  x = unif.rv.exp
  )

hist(
  x = unif.rv.exp
  )

unif.cdf.the <- # Theoretical (.the) because exactly derived from functional
                # form of a uniform distribution
  qunif( # n evenly spaced uniform cumulative frequencies
    p = ppoints(n), # n evenly spaced points between 0 and 1
    min = unif.min, 
    max = unif.max
    ) 

# These two plots illustrate the difference between theoretical and expected
# Remembering that expected and modeled are the same. Note the noise due to 
# random sampling in the unif.rv.exp plot
plot(
  x = unif.cdf.the
  )

plot(
  sort(unif.rv.exp)
  )

unif.pdf.exp <- # Expected probabilities for specific observed RV values
  punif(
    q = unif.rv.exp, # "Observed" RV values (quotes because these are actually
                     # modeled rv's)
    min = unif.min, 
    max = unif.max
    ) 

plot(
  unif.pdf.exp
  )

plot(
  sort(unif.pdf.exp)
)

unif.pdf.the <-
  dunif( # n evenly spaced uniform probabilities
    x = ppoints(n), # n evenly spaced points between 0 and 1
    min = unif.min, 
    max = unif.max
    )

plot(unif.pdf.the)

# Kolmorgorov-Smirnov Test
# One-sample Test
# H0: CDF of Observed RV = Expected CDF of Reference Distribution 
# If p < 0.05 then reject H0
# Else fail to disprove H0 (colloquially: then RV likely sampled from the 
# Reference Distribution)
# https://jasdumas.github.io/tech-short-papers/Example_of_Kolmogorov_Smirnov_test2.html

htest.ks.unif.SLR <-
  ks.test(
    x = sort(x1.ind), # Observed values of Random Variable, sorted. Remember'
                      # these are actually expected values because modeled rv's
    y = unif.cdf.the # Reference Distribution (must always be cumulative)
  )

print(htest.ks.unif.SLR)

# D ~ 0 and p >> 0.05 therefore we fail to reject the Null Hypothesis.
# This result is not surprising given that our "observed" sample data is
# actually expected random variates. This confirms that our sampling was not
# (by chance sampling) biased.

# Positive Control

ks.test(
  x = sort(unif.rv.exp), # Observed values of Random Variable, sorted. Remember
                         # these are actually expected values because modeled 
                         # rv's
  y = unif.cdf.the # Reference Distribution (must always be cumulative)
  )

# Again, D ~ 0 and p >> 0.05 so fail to reject the Null Hypothesis.

# Negative Control (Non-Event)

ks.test(
  x = sort(x1.ind), # Observed values of Random Variable, sorted. Remember
  # these are actually expected values because modeled rv's
  y = qnorm(
    p = ppoints(n), 
    mean = mean(x1.ind), 
    sd = sd(x1.ind)
  ) # Reference Distribution (must always be cumulative)
)

# D ~ 0 and p >> 0.05 therefore fail to reject the NULL Hypothesis (that the
# two distributions are identical). The two distributions may be different.
# See why it is always important to run controls!!!

# Negative Control (Not-Event)

ks.test(
  x = sort(x1.ind), # Observed values of Random Variable, sorted. Remember
  # these are actually expected values because modeled rv's
  y = qnorm(
    p = ppoints(n), 
    mean = 0, 
    sd = 1
  ) # Reference Distribution (must always be cumulative)
)

# D ~ 1 and p << 0.05 therefore reject the NULL Hypothesis, these two
# distributions are NOT the same.
# See why it is always important to run controls!!!

# Assumption #3 Zero Condtional Mean rho(residuals, x.sub.i) = 0

m.slr.ind.the.end <-
  lm(
    m.slr.ind.the$residuals ~ x1.ind # regressing Residuals v. Independents
    )

summary(m.slr.ind.the.end)

summary(m.slr.ind.the.end)$coefficients[[2]] # Slope of correlation ~ 0, so 
#                                            # essentially zero conditional
#                                            # mean

cov(
  m.slr.ind.the$residuals, 
  x1.ind
  ) # 5.90707e-28 is _really_ close to zero, so again, essentially zero 
#   # conditional mean


# Assumption #4 Independence of Regressors rho(x.sub.i, x.sub.j) = 0 for all
# i and all j where i <> j

# Only one Regressor in this model

# Assumption #5 Homoscedasticity of Residuals wrt to Independents

plot(
  m.slr.ind.the$residuals ~ x1.ind
)
abline(m.slr.ind.the.end)

# Note scale on y-axis. 'Outliers' aren't really outliers and residuals are 
# homoscedastic

# Assumption #6 Normally Distributed Regressands, Residuals and Coefficients

hist(
  df.slr.ind.the$y1.slr.the # Regressand
  ) 

#Shapiro-Wilk Normality Test. 
#Null Hypothesis = Distribution is Normally Distributed
#p >  0.05 Fail to Reject Null Hypothesis
#p <= 0.05 Reject the Null Hypothesis (therefore: Distribution is not normal)

shapiro.test(
  df.slr.ind.the$y1.slr.the # Regressand
)

# Reject the Null Hypothesis because p << 0.05. Regressand is NOT normally
# distributed

hist(
  m.slr.ind.the$residuals # Residual
)

# Shapiro-Wilk Normality Test. 
# Null Hypothesis = Distribution is Normally Distributed
# p >  0.05 Fail to Reject Null Hypothesis
# p <= 0.05 Reject the Null Hypothesis (therefore: Distribution is not normal)

shapiro.test(
  m.slr.ind.the$residuals # Residual
)

# Reject the Null Hypothesis because p < 0.05. Residual is not normally
# distributed. Note also how tiny residuals are, this is always concerning.

# ASSUMPTION #6 IS VIOLATED

# Assumption #7 Independence of Experimental Units (within  v between subjects)

# By experimental design. No easy way to automate the testing of this
# assumption, you have to have a pretty good guess as to the number of subjects
# and the number of within-subjects observations

# END NOTE
# In addition to Kolmogorov-Smirnov test, can also try Kuiper's Test
# https://en.wikipedia.org/wiki/Kuiper%27s_test

# Predicting and Comparing Predictions to Theoreticals

max(x1.ind) # 997.8109
min(x1.ind) # 4.141298

# Vector containing New, Novel and Out-of-Range data to run through model for
# predictions
x1.pred <-
  c(
    -2*unif.max, # Out-of-Range
    -unif.max, # Out-of-Range
    -max(x1.ind), # Out-of-Range 
    (unif.min - unif.max)/2,  # Out-of-Range
    -min(x1.ind), # Out-of-Range
    unif.min,  # Novel
    min(x1.ind), # New
    (unif.max - unif.min)/2, # New
    max(x1.ind), # New
    unif.max, # Novel
    2*unif.max # Out-of-Range
  )

is.vector(x1.pred)  

# In order to run the predict() function, it requires newdata in a dataframe
# with columns named as in the model
df.x1.pred <-
  as.data.frame(
    x1.pred
  )

colnames(df.x1.pred) <- c('x1.ind')

# Predictions based on model and newdata
y1.slr.pred <-
  predict(
    object = m.slr.ind.the,
    newdata = df.x1.pred
    )

# Since we know the True linear relationship because we modeled it to begin
# with, we can run the New, Novel and Out-of-Range data through the true model
y1.slr.the.pred <- # Theoretical value of dependent RV y as a simple linear
#                  # model of predicted RV x1
  theta.x0 + (theta.x1.ind * x1.pred) 

df.slr.ind.the.pred <-
  as.data.frame(
    cbind(
      y1.slr.the.pred,
      x1.pred
    )
  )

m.slr.ind.the.pred <-
  lm(
    y1.slr.the.pred ~ x1.pred,
    data = df.slr.ind.the.pred
  )

summary(m.slr.ind.the.pred) # Perfect fit, correct slope and intercept

# Create dataset of Modeled v. Predicted for x1.pred input

df.slr.theVpred <-
  as.data.frame(
    cbind(
      y1.slr.pred,
      y1.slr.the.pred
    )
  )

m.slr.theVpred <-
  lm(
    y1.slr.the.pred ~ y1.slr.pred,
    data = df.slr.theVpred
  )

summary(m.slr.theVpred) # Perfect Fit: we'd expect slope to be 1 and 
#                       # intercept to be 0 as 'the' and 'pred' are identical  

plot(
  x = y1.slr.the.pred,
  y = y1.slr.pred
  )

df.slr.theVpred

# Note that the same x1.pred values went into both of these response variables.
# We will start to see deviations when we add bias, noise, endogeneity, etc.

# One thing to think about is that a line extends to infinity in both
# directions. If we blindly apply the model it may well make ridiculous real
# world predictions. Most often when a model is applied, it is applied under
# a series of constraints. For example, height can never be negative, cost 
# can't be more than a fraction of the GDP, fractional values may or may not
# be allowed, irrational numbers may have no meaning, linearity only applies
# over a certain range of input values, etc.

# When making predictions with a model, always consider both common sense and
# observed constraints relative to the system of variables you are modeling
# (i.e. the feature space) and make note of these.  When your model goes into
# production, make sure that the deployment and maintenance teams are aware of
# these constraints. Ideally, these constraints would be coded as sanity checks
# and would throw warnings when input data approached these boundaries.

script3.1.01 <- 'Complete'

#END