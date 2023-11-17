####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

# Based on: 3.2 Endogeneity and Collinearity
# By: Marc d. Paradis

# CONFIRM THAT: script3.1.01 = 'Complete'
# CONFIRM THAT: script3.1.02 = 'Complete'

set.seed(31000) # To ensure reproducible results

delta.y1.ind <- # Set ground truth residuals
  rnorm( # pulled from a random normal distribution
    n = n, # of sample size n
    mean = 0, # with theoretical mean of zero
    sd = 0.1 * 1000 # and theoretical standard devation that is 10% error of
    # absolute range (max - min) of independent RV x1
  )

y1.slr.noi.the <-
  y1.slr.the + delta.y1.ind # element-wise addition of residuals

df.slr.ind.noi.the <-
  as.data.frame(
    cbind(
      y1.slr.noi.the,
      x1.ind
    )
  )

m.slr.ind.noi.the <-
  lm(
    y1.slr.noi.the ~ x1.ind,
    data = df.slr.ind.noi.the
  )

# Assumption #1: Linearity of Parameters
summary(m.slr.ind.noi.the)
# y = 35 + (1.5 * x1)

summary(m.slr.ind.noi.the)$r.squared

plot(
  y1.slr.noi.the ~ x1.ind,
  data = df.slr.ind.noi.the
)
abline( # Best fit residual line
  m.slr.ind.noi.the, 
  col = 'red'
)
abline( # Best fit bias line
  m.slr.ind.bi.the, 
  col = 'blue'
)
abline( # Ground Truth line
  a = theta.x0, 
  b = theta.x1.ind, 
  col = 'black'
)

# Note that Noise and Ground vary slightly in slope

# Assumption #2: Random Sample

# Because epsilon effects only the y values all analysis under this condtion
# will be identical to the analysis under the SLR condition

# Assumption #3 Zero Condtional Mean rho(residuals, x.sub.i) = 0

m.slr.ind.noi.the.end <-
  lm(
    m.slr.ind.noi.the$residuals ~ x1.ind # regressing Residuals v. Independents
  )

summary(m.slr.ind.noi.the.end)

summary(m.slr.ind.noi.the.end)$coefficients[[2]] # Slope of correlation

cov(
  m.slr.ind.noi.the$residuals, 
  x1.ind
) # 1.853153e-13 is _really_ close to zero, note identical to value without
# error {May want to write some code to make this an explicit test}

# Assumption #4 Independence of Regressors rho(x.sub.i, x.sub.j) = 0 for all
# i and all j where i <> j

# Only one Regressor in this model

# Assumption #5 Homoscedasticity of Residuals wrt to Independents

plot(
  m.slr.ind.noi.the$residuals ~ x1.ind
)
abline(m.slr.ind.noi.the.end)

# Assumption #6 Normally Distributed Regressands, Residuals and Coefficients

hist(
  df.slr.ind.noi.the$y1.slr.noi.the # Regressand
) 

# Shapiro-Wilk Normality Test. 
# Null Hypothesis = Distribution is Normally Distributed
# p >  0.05 Fail to Reject Null Hypothesis
# p <= 0.05 Reject the Null Hypothesis (therefore: Distribution is not normal)

shapiro.test(
  df.slr.ind.noi.the$y1.slr.noi.the # Regressand
)

# Reject the Null Hypothesis because p < 0.05. Regressand is not normally
# distributed

hist(
  m.slr.ind.noi.the$residuals # Residual
)

# Shapiro-Wilk Normality Test. 
# Null Hypothesis = Distribution is Normally Distributed
# p >  0.05 Fail to Reject Null Hypothesis
# p <= 0.05 Reject the Null Hypothesis (therefore: Distribution is not normal)

shapiro.test(
  m.slr.ind.noi.the$residuals # Residual
)

# Fail to reject the Null Hypothesis because p > 0.05. Residual may be normally
# distributed

# ASSUMPTION #6 IS VIOLATED

# Assumption #7 Independence of Experimental Units (within  v between subjects)

# By experimental design. No easy way to automate the testing of this
# assumption, you have to have a pretty good guess as to the number of subjects
# and the number of within-subjects observations

# In addition to Kolmogorov-Smirnov test, can also try Kuiper's Test
# https://en.wikipedia.org/wiki/Kuiper%27s_test


script3.1.03 <- 'Complete'

#END
