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

# Reject the Null Hypothesis because p << 0.05. Regressand is not normally
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

script3.1.01 <- 'Complete'

#END
