####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

#Marc d. Paradis
#R 3.5.1 for Windows
#RStudio 1.1.456

# CONFIRM THAT: script3.1.01 = 'Complete'
# CONFIRM THAT: script3.1.02 = 'Complete'
# CONFIRM THAT: script3.1.03 = 'Complete'
# CONFIRM THAT: script3.1.04 = 'Complete'
# CONFIRM THAT: script3.1.07 = 'Complete'
# CONFIRM THAT: script3.1.08 = 'Complete'

# Note working with the Theoretical, that is the no noise and no bias, case
# for calculating y (y1.2lr.the)

m.2lr.ind.the.end <-
  lm(
    y1.2lr.the ~ x1.ind, # x2.ind is missing from model specification
    data = df.2lr.ind.the
  )

# ASSUMPTION #1: Linearity of Parameters
# y1.2lr.the = theta.x0 + (theta.x1.ind * x1.ind)  + (theta.x2.ind * x2.ind)
summary(m.2lr.ind.the.end) # No surprise roughly half the variance is 
#                          # unaccounted for

# Estimated slope = 1.4323 +/- 0.1488. Remember theoretical = 1.5
# Estimated intercept = 853.7617 +/- 86.6590. Remember theoretical = 35 and
# theoretical + bias = 135

summary(m.2lr.ind.the.end)$r.squared 

plot(
  y1.2lr.the ~ x1.ind,
  data = df.2lr.ind.the
) # Some of the 'noise' is due to the impact of x2 on y1
abline(
  lm(
    y1.2lr.the ~ x1.ind, # x2.ind is missing from model specification
    data = df.2lr.ind.the
  ),
  col = 'green'
)

par(mfrow = c(2,2))
plot(m.2lr.ind.the.end) # Note Q-Q Residuals definitely non-normal

par(mfrow = c(1,1)) # Resets par(mfrow) to default

# FAIL Assumption #1

# ASSUMPTION #2: Random Sample

# Already assessed under slr.the and 2lr.the scripts

# ASSUMPTION #3 Zero Condtional Mean rho(residuals, x.sub.i) = 0

m.2lr.ind.the.end.end <- # Clunky naming convention (probably need to back
                         # correct)
  lm(
    m.2lr.ind.the.end$residuals ~ x1.ind # regressing Residuals v. Independents
                                         # remembering that the model is mis-
                                         # specified because it is missing x2
  )

summary(m.2lr.ind.the.end.end)

summary(m.2lr.ind.the.end.end)$coefficients[c(1,2)] # Slope of correlations

# Fail to reject H0 so analysis is consistent with no dependence of residuals
# on x1.ind

cor( 
  m.2lr.ind.the.end$residuals, 
  x1.ind
) # 3.444135e-17 is _really_ close to zero


# ASSUMPTION #4 Independence of Regressors rho(x.sub.i, x.sub.j) = 0 for all
# i and all j where i <> j

# Only one Regressor in model specification

# ASSUMPTION #5 Homoscedasticity of Residuals wrt to Independents

plot(
  m.2lr.ind.the.end$residuals ~ x1.ind
)
abline(m.2lr.ind.the.end.end)

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

# Fail to Reject the Null Hypothesis because p > 0.05. Regressand is consistent
# with a normal distribution

hist(
  m.2lr.ind.the.end$residuals # Residual
)

#Shapiro-Wilk Normality Test. 
#Null Hypothesis = Distribution is Normally Distributed
#p >  0.05 Fail to Reject Null Hypothesis
#p <= 0.05 Reject the Null Hypothesis (therefore: Distribution is not normal)

shapiro.test(
  m.2lr.ind.the.end$residuals # Residual
)

# Reject the Null Hypothesis because p < 0.05. Residual is NOT normally
# distributed.

# FAIL Assumption #6

# ASSUMPTION #7 Independence of Experimental Units (within  v between subjects)

# Depends on Investigational Design

script3.2.01 <- 'Complete'

# END