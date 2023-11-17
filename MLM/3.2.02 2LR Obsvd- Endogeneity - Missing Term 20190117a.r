####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

# CONFIRM THAT: script3.1.01 = 'Complete'
# CONFIRM THAT: script3.1.02 = 'Complete'
# CONFIRM THAT: script3.1.03 = 'Complete'
# CONFIRM THAT: script3.1.04 = 'Complete'
# CONFIRM THAT: script3.1.07 = 'Complete'
# CONFIRM THAT: script3.1.08 = 'Complete'
# CONFIRM THAT: script3.2.01 = 'Complete'

# Note working with the Observed, that is with noise and bias, case for
# calculating y (y1.2lr.obs)

m.2lr.ind.obs.end <-
  lm(
    y1.2lr.obs ~ x1.ind, # x2.ind is missing from model specification
    data = df.2lr.ind.obs
  )

# ASSUMPTION #1: Linearity of Parameters
# y1.2lr.obs = theta.x0 + (theta.x1.ind * x1.ind)  + (theta.x2.ind * x2.ind)
summary(m.2lr.ind.obs.end) # No surprise roughly half the variance is 
#                          # unaccounted for

# Estimated slope = 1.3709 +/- 0.1554. Remember theoretical = 1.5
# Estimated intercept = 982.6983 +/- 90.4916. Remember theoretical = 35 and
# theoretical + bias = 135

summary(m.2lr.ind.obs.end)$r.squared 

plot(
  y1.2lr.obs ~ x1.ind,
  data = df.2lr.ind.obs
) # 'Noise' is a combination of impact of x2 on y1 as well as noise and bias
abline(
  lm(
    y1.2lr.obs ~ x1.ind, # x2.ind is missing from model specification
    data = df.2lr.ind.obs
  ),
  col = 'green'
)

par(mfrow = c(2,2))
plot(m.2lr.ind.obs.end) # again Q-Q Residuals definitely non-normal

par(mfrow = c(1,1)) # Resets par(mfrow) to default

# FAIL Assumption #1

# ASSUMPTION #2: Random Sample

# Already assessed under slr.obs and 2lr.obs scripts

# ASSUMPTION #3 Zero Condtional Mean rho(residuals, x.sub.i) = 0

m.2lr.ind.obs.end.end <- # Clunky naming convention (probably need to back
                         # correct)
  lm(
    m.2lr.ind.obs.end$residuals ~ x1.ind # regressing Residuals v. Independents
                                         # remembering that the model is mis-
                                         # specified because it is missing x2
  )

summary(m.2lr.ind.obs.end.end)

summary(m.2lr.ind.obs.end.end)$coefficients[c(1,2)] # Slope of correlation

# Fail to reject H0 so analysis is consistent with no dependence of residuals
# on x1.ind

cor( 
  m.2lr.ind.obs.end$residuals, 
  x1.ind
) # 8.825784e-17 is _really_ close to zero


# ASSUMPTION #4 Independence of Regressors rho(x.sub.i, x.sub.j) = 0 for all
# i and all j where i <> j

# Only one Regressor in model specification

# ASSUMPTION #5 Homoscedasticity of Residuals wrt to Independents

plot(
  m.2lr.ind.obs.end$residuals ~ x1.ind
)
abline(m.2lr.ind.obs.end.end)

# ASSUMPTION #6 Normally Distributed Regressands, Residuals and Coefficients

hist(
  df.2lr.ind.obs$y1.2lr.obs # Regressand
) 

#Shapiro-Wilk Normality Test. 
#Null Hypothesis = Distribution is Normally Distributed
#p >  0.05 Fail to Reject Null Hypothesis
#p <= 0.05 Reject the Null Hypothesis (therefore: Distribution is not normal)

shapiro.test(
  df.2lr.ind.obs$y1.2lr.obs # Regressand
)

# Fail to Reject the Null Hypothesis because p > 0.05. Regressand is consistent
# with a normal distribution

hist(
  m.2lr.ind.obs.end$residuals # Residual
)

#Shapiro-Wilk Normality Test. 
#Null Hypothesis = Distribution is Normally Distributed
#p >  0.05 Fail to Reject Null Hypothesis
#p <= 0.05 Reject the Null Hypothesis (therefore: Distribution is not normal)

shapiro.test(
  m.2lr.ind.obs.end$residuals # Residual
)

# Fail to Reject the Null Hypothesis because p > 0.05. Residual is consistent
# with a normal distribution

# ASSUMPTION #7 Independence of Experimental Units (within  v between subjects)

# Depends on Investigational Design

script3.2.02 <- 'Complete'

# END