####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

# Based on: 3.2 Endogeneity and Collinearity
# By: Marc d. Paradis

# CONFIRM THAT script3.1.01 = 'Complete'. This script used variables and
# objects from 3.1.01

epsilon <- 100 # Set ground truth statistical bias parameter for linear model
# PERHAPS TRY TO MAKE SAY 10% OF ABSOLUTE RANGE, HARD IN MULTI-DIM CASE

y1.slr.bi.the <-
  y1.slr.the + epsilon # note recycling of epsilon for element-wise addition

df.slr.ind.bi.the <-
  as.data.frame(
    cbind(
      y1.slr.bi.the,
      x1.ind
    )
  )

m.slr.ind.bi.the <-
  lm(
    y1.slr.bi.the ~ x1.ind,
    data = df.slr.ind.bi.the
  )

# Assumption #1: Linearity of Parameters
summary(m.slr.ind.bi.the)
# y = 35 + (1.5 * x1)

summary(m.slr.ind.bi.the)$r.squared

plot(
  y1.slr.bi.the ~ x1.ind,
  data = df.slr.ind.bi.the
)
abline( # Best fit bias line
  m.slr.ind.bi.the, 
  col = 'blue'
  )
abline( # Ground Truth line
  a = theta.x0, 
  b = theta.x1.ind, 
  col = 'black'
  ) # Ground Truth line

# Assumption #2: Random Sample

# Because epsilon effects only the y values all analysis under this condtion
# will be identical to the analysis under the SLR condition

# Assumption #3 Zero Condtional Mean rho(residuals, x.sub.i) = 0

m.slr.ind.bi.the.end <-
  lm(
    m.slr.ind.bi.the$residuals ~ x1.ind # regressing Residuals v. Independents
  )

summary(m.slr.ind.bi.the.end)

summary(m.slr.ind.bi.the.end)$coefficients[[2]] # Slope of correlation

cov(
  m.slr.ind.bi.the$residuals, 
  x1.ind
) # -1.218807e-26 is _really_ close to zero, note identical to value without
  # bias {May want to write some code to make this an explicit test}


# Assumption #4 Independence of Regressors rho(x.sub.i, x.sub.j) = 0 for all
# i and all j where i <> j

# Only one Regressor in this model

# Assumption #5 Homoscedasticity of Residuals wrt to Independents

plot(
  m.slr.ind.bi.the$residuals ~ x1.ind
  )
abline(m.slr.ind.bi.the.end)

# Assumption #6 Normally Distributed Regressands, Residuals and Coefficients

hist(
  df.slr.ind.bi.the$y1.slr.bi.the # Regressand
  ) 

# Shapiro-Wilk Normality Test. 
# Null Hypothesis = Distribution is Normally Distributed
# p >  0.05 Fail to Reject Null Hypothesis
# p <= 0.05 Reject the Null Hypothesis (therefore: Distribution is not normal)

shapiro.test(
  df.slr.ind.bi.the$y1.slr.bi.the # Regressand
  )

# Reject the Null Hypothesis because p < 0.05. Regressand is not normally
# distributed

hist(
  m.slr.ind.bi.the$residuals # Residual
)

# Shapiro-Wilk Normality Test. 
# Null Hypothesis = Distribution is Normally Distributed
# p >  0.05 Fail to Reject Null Hypothesis
# p <= 0.05 Reject the Null Hypothesis (therefore: Distribution is not normal)

shapiro.test(
  m.slr.ind.bi.the$residuals # Residual
)

# Reject the Null Hypothesis because p << 0.05. Residual is not normally
# distributed

# ASSUMPTION #6 IS VIOLATED

# Assumption #7 Independence of Experimental Units (within  v between subjects)

# By experimental design. No easy way to automate the testing of this
# assumption, you have to have a pretty good guess as to the number of subjects
# and the number of within-subjects observations

# In addition to Kolmogorov-Smirnov test, can also try Kuiper's Test
# https://en.wikipedia.org/wiki/Kuiper%27s_test

# Predicting and Comparing Predictions to Theoreticals

# Predictions based on model and newdata
y1.slr.bi.pred <-
  predict(
    object = m.slr.ind.bi.the,
    newdata = df.x1.pred
  )

# We have already run the New, Novel and Out-of-Range data through the true 
# model. Now we reuse that model as epsilon technically is a deviation from the
# true values as captured in y1.slr.the.pred

# Create dataset of Modeled v. Predicted for x1.pred input

df.slr.bi.theVpred <-
  as.data.frame(
    cbind(
      y1.slr.bi.pred,
      y1.slr.the.pred
    )
  )

m.slr.bi.theVpred <-
  lm(
    y1.slr.the.pred ~ y1.slr.bi.pred,
    data = df.slr.bi.theVpred
  )

summary(m.slr.bi.theVpred) # Perfect Fit: Slope = 1 and Intercept = -100. Makes
#                          # sense given that we are modeling against bias only

plot(
  x = y1.slr.bi.the.pred,
  y = y1.slr.bi.pred
)

df.slr.bi.theVpred # See that each is off by exactly 100, our value of epsilon

script3.1.02 <- 'Complete'

#END