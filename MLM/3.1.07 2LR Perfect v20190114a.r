####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456


# Following the example code of 3.2.1 Perfect SLR, let's create another
# independent variable

# CONFIRM THAT: script3.1.01 = 'Complete'
# CONFIRM THAT: script3.1.02 = 'Complete'
# CONFIRM THAT: script3.1.03 = 'Complete'
# CONFIRM THAT: script3.1.04 = 'Complete'

set.seed(310700) # To ensure reproducible results 

x2.ind <- # Random variable vector of independent x2 values
  runif( # pulled from a random uniform distribution
    n = n, # of sample size n
    min = unif.min, # Maintain scale of independent variable for simplicity
    max = unif.max  # Maintain scale of independent variable for simplicity
  )

mean(x2.ind) # 522.9574 v 506.9302
sd(x2.ind) # 283.1509 v 287.9923

plot(x2.ind) # Plot random variates by index
abline( # add a
  h = mean(x2.ind) # horizontal line with y-intercept of mean(x2.ind)
)

plot( # Plot random variates
  sort(x2.ind) # sorted by value
)

hist(x2.ind) # Plot a histogram of random variates

theta.x2.ind <- 1.5 # Set ground truth linear coefficient for independent x2
                    # variable. Again for simplicity's sake identical to x1

y1.2lr.the <- # Theoretical value of dependent RV y as a multiple linear model 
              # of independent RV x1 and x2
  theta.x0 + (theta.x1.ind * x1.ind)  + (theta.x2.ind * x2.ind) 

df.2lr.ind.the <-
  as.data.frame(
    cbind(
      y1.2lr.the,
      x1.ind,
      x2.ind
    )
  )

m.2lr.ind.the <-
  lm(
    y1.2lr.the ~ x1.ind + x2.ind, # It's that simple!
    data = df.2lr.ind.the
  )

# ASSUMPTION #1: Linearity of Parameters
# y1.2lr.the = theta.x0 + (theta.x1.ind * x1.ind)  + (theta.x2.ind * x2.ind)
summary(m.2lr.ind.the) # Perfect fit because No noise or bias in equation

summary(m.2lr.ind.the)$r.squared # Can't get more perfect than 1

plot(
  y1.2lr.the ~ x1.ind,
  data = df.2lr.ind.the
) # 'Noise' is due to the impact of x2 on y1
abline(
  lm(
    y1.2lr.the ~ x1.ind + x2.ind,
    data = df.2lr.ind.the
  ),
  col = 'green'
)
abline(
  lm(
    y1.2lr.the ~ x1.ind,
    data = df.2lr.ind.the
  ),
  col = 'red'
)

plot(
  y1.2lr.the ~ x2.ind,
  data = df.2lr.ind.the
) # 'Noise' is due to the impact of x1 on y1
abline(
  lm(
    y1.2lr.the ~ x1.ind + x2.ind,
    data = df.2lr.ind.the
  ),
  col = 'green'
)
abline(
  lm(
    y1.2lr.the ~ x2.ind,
    data = df.2lr.ind.the
  ),
  col = 'red'
)

par(mfrow = c(2,2))
plot(m.2lr.ind.the)

par(mfrow = c(1,1)) # Resets par(mfrow) to default

plot(
  x = x1.ind, 
  y = x2.ind
  )

# ASSUMPTION #2: Random Sample

set.seed(310701)

# RESAMPLING THE EXPECTED VALUES HERE, BEST PRACTICE FOR POSITIVE CONTROLS

unif.rv.exp2 <- # random variates (rv) are modeled  (expected); Values of a Random
  # Variable (RV) are observed
  runif( # n random variates from a uniform distribution
    n = n, 
    min = unif.min, 
    max = unif.max
  ) 

plot(unif.rv.exp2)
hist(unif.rv.exp2)

# Kolmorgorov-Smirnov Test
# One-sample Test
# H0: CDF of Observed RV = Expected CDF of Reference Distribution 
# If p < 0.05 then reject H0
# Else fail to disprove H0 (colloquially: then RV likely sampled from the 
# Reference Distribution)
# https://jasdumas.github.io/tech-short-papers/Example_of_Kolmogorov_Smirnov_test2.html

htest.ks.unif.2LR.x2 <-
  ks.test(
    x = sort(x2.ind), # Observed values of Random Variable, sorted. Remember'
    # these are actually expected values because modeled rv's
    y = unif.cdf.the # Reference Distribution (must always be cumulative)
                     # Since this is theoretical there is no need to resample
                     # for a given max and min and number of poitns this will
                     # always be the same.
  )

print(htest.ks.unif.2LR.x2)

# D ~ 0 and p >> 0.05 therefore we fail to reject the Null Hypothesis.
# This result is not surprising given that our "observed" sample data is
# actually expected random variates. This confirms that our sampling was not
# (by chance sampling) biased.

# Positive Control

ks.test(
  x = sort(unif.rv.exp2), # Observed values of Random Variable, sorted. Remember
  # these are actually expected values because modeled 
  # rv's
  y = unif.cdf.the # Reference Distribution (must always be cumulative)
)

# Again, D ~ 0 and p > 0.05 so fail to reject the Null Hypothesis - a good
# postive control result.

# Negative Control (Non-Event)

ks.test(
  x = sort(x2.ind), # Observed values of Random Variable, sorted. Remember
  # these are actually expected values because modeled rv's
  y = qnorm(
    p = ppoints(n), 
    mean = mean(x2.ind), 
    sd = sd(x2.ind)
  ) # Reference Distribution (must always be cumulative)
)

# D ~ 0 and p >> 0.05 therefore fail to reject the NULL Hypothesis. A bad
# negative control result. See why it is always important to run controls!!!

# Negative Control (Not-Event)

ks.test(
  x = sort(x2.ind), # Observed values of Random Variable, sorted. Remember
  # these are actually expected values because modeled rv's
  y = qnorm(
    p = ppoints(n), 
    mean = 0, 
    sd = 1
  ) # Reference Distribution (must always be cumulative)
)

# D ~ 1 and p << 0.05 therefore reject the NULL Hypothesis. A good negative
# control result. See why it is always important to run controls!!!

# ASSUMPTION #3 Zero Condtional Mean rho(residuals, x.sub.i) = 0

m.2lr.ind.the.end <-
  lm(
    m.2lr.ind.the$residuals ~ x1.ind + x2.ind # regressing Residuals v. 
                                              # Independents
  )

summary(m.2lr.ind.the.end)

summary(m.2lr.ind.the.end)$coefficients[c(2,3)] # Slope of correlation

summary(
  lm(
    m.2lr.ind.the$residuals ~ x1.ind
    )
  )

# Fail to reject H0 so analysis is consistent with no dependence of residuals
# on x1.ind

summary(
  lm(
    m.2lr.ind.the$residuals ~ x2.ind
    )
  )

# Fail to reject H0 so analysis is consistent with no dependence of residuals
# on x2.ind

cor( 
  m.2lr.ind.the$residuals, 
  x1.ind
  ) # 9.836716e-17 is _really_ close to zero

cor(
  m.2lr.ind.the$residuals, 
  x2.ind
) # 3.136543e-16 is _really_ close to zero


# ASSUMPTION #4 Independence of Regressors rho(x.sub.i, x.sub.j) = 0 for all
# i and all j where i <> j

# Only two Regressors in this model so a simple correlation will suffice

cor.test(
  x = df.2lr.ind.the$x1.ind,
  y = df.2lr.ind.the$x2.ind,
  alternative = 'two.sided',
  method = 'pearson',
  exact = NULL,
  conf.level = 0.95
)

# Fail to reject the Null Hypothesis so data are consistent with a true
# correlation = 0

# ASSUMPTION #5 Homoscedasticity of Residuals wrt to Independents

plot(
  m.2lr.ind.the$residuals ~ x1.ind
)
abline(m.2lr.ind.the.end)

plot(
  m.2lr.ind.the$residuals ~ x2.ind
)
abline(m.2lr.ind.the.end)

# ASSUMPTION #6 Normally Distributed Regressands, Residuals and Coefficients

hist(
  df.2lr.ind.the$y1.2lr.the # Regressand
) 

#Shapiro-Wilk Normality Test. 
#Null Hypothesis = Distribution is Normally Distributed
#p >  0.05 Fail to Reject Null Hypothesis
#p <= 0.05 Reject the Null Hypothesis (therefore: Distribution is not normal)

shapiro.test(
  df.2lr.ind.the$y1.2lr.the # Regressand
)

# Fail to Reject the Null Hypothesis because p >> 0.05. Regressand is
# consistent with a normal distribution

hist(
  m.2lr.ind.the$residuals # Residual
)

#Shapiro-Wilk Normality Test. 
#Null Hypothesis = Distribution is Normally Distributed
#p >  0.05 Fail to Reject Null Hypothesis
#p <= 0.05 Reject the Null Hypothesis (therefore: Distribution is not normal)

shapiro.test(
  m.2lr.ind.the$residuals # Residual
)

# Reject the Null Hypothesis because p << 0.05. Residual is not normally
# distributed - however the scale of the residuals argues for this being an
# artifact rather than a real violation.

# ASSUMPTION #7 Independence of Experimental Units (within  v between subjects)

# TBD

script3.1.07 <- 'Complete'

#END