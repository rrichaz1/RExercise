####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

# Based on: 3.2 Endogeneity and Collinearity
# By: Marc d. Paradis

# CONFIRM THAT: script3.1.01 = 'Complete'
# CONFIRM THAT: script3.1.02 = 'Complete'
# CONFIRM THAT: script3.1.03 = 'Complete'

y1.slr.obs <-
  (y1.slr.the + delta.y1.ind) + epsilon # element-wise addition of residuals and
                                        # addition of bias term, recycled across
                                        # dv and residual vectors

df.slr.ind.obs <-
  as.data.frame(
    cbind(
      y1.slr.obs,
      x1.ind
    )
  )

m.slr.ind.obs <-
  lm(
    y1.slr.obs ~ x1.ind,
    data = df.slr.ind.obs
  )

# Assumption #1: Linearity of Parameters
summary(m.slr.ind.obs)
# y = 35 + (1.5 * x1)

summary(m.slr.ind.obs)$r.squared

plot(
  y1.slr.obs ~ x1.ind,
  data = df.slr.ind.obs
)
abline( # Best fit observed line
  m.slr.ind.obs,
  col = 'green'
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

# Note that noise and obsvd have same slope, different intercepts. Same for 
# ground and bias.

# Assumption #2: Random Sample

# Because epsilon effects only the y values all analysis under this condtion
# will be identical to the analysis under the SLR condition

# Assumption #3 Zero Condtional Mean rho(residuals, x.sub.i) = 0

m.slr.ind.obs.end <-
  lm(
    m.slr.ind.obs$residuals ~ x1.ind # regressing Residuals v. Independents
  )

summary(m.slr.ind.obs.end)

summary(m.slr.ind.obs.end)$coefficients[[2]] # Slope of correlation

cov(
  m.slr.ind.obs$residuals, 
  x1.ind
) # 2.767349e-12 is _really_ close to zero, note identical to value without
# error {May want to write some code to make this an explicit test}

# Assumption #4 Independence of Regressors rho(x.sub.i, x.sub.j) = 0 for all
# i and all j where i <> j

# Only one Regressor in this model

# Assumption #5 Homoscedasticity of Residuals wrt to Independents

plot(
  m.slr.ind.obs$residuals ~ x1.ind
)
abline(m.slr.ind.obs.end)

# Assumption #6 Normally Distributed Regressands, Residuals and Coefficients

hist(
  df.slr.ind.obs$y1.slr.obs # Regressand
) 

# Shapiro-Wilk Normality Test. 
# Null Hypothesis = Distribution is Normally Distributed
# p >  0.05 Fail to Reject Null Hypothesis
# p <= 0.05 Reject the Null Hypothesis (therefore: Distribution is not normal)

shapiro.test(
  df.slr.ind.obs$y1.slr.obs # Regressand
)

# Reject the Null Hypothesis because p < 0.05. Regressand is not normally
# distributed

hist(
  m.slr.ind.obs$residuals # Residual
)

# Shapiro-Wilk Normality Test. 
# Null Hypothesis = Distribution is Normally Distributed
# p >  0.05 Fail to Reject Null Hypothesis
# p <= 0.05 Reject the Null Hypothesis (therefore: Distribution is not normal)

shapiro.test(
  m.slr.ind.obs$residuals # Residual
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

# Predictions based on model and newdata
y1.slr.obs.pred <-
  predict(
    object = m.slr.ind.obs,
    newdata = df.x1.pred
  )

# We have already run the New, Novel and Out-of-Range data through the true 
# model. Now we reuse that model as epsilon technically is a deviation from the
# true values as captured in y1.slr.the.pred

# Create dataset of Modeled v. Predicted for x1.pred input

df.slr.obs.theVpred <-
  as.data.frame(
    cbind(
      y1.slr.obs.pred,
      y1.slr.the.pred
    )
  )

m.slr.obs.theVpred <-
  lm(
    y1.slr.the.pred ~ y1.slr.obs.pred,
    data = df.slr.obs.theVpred
  )

summary(m.slr.obs.theVpred) # Perfect Fit: Slope = 1.016 and Intercept = -117.9 
#                           # Represents relatively minor impact of noise and
#                           # bias over such a broad range of x and y.

plot(
  x = y1.slr.the.pred,
  y = y1.slr.obs.pred
)

df.slr.obs.theVpred # See that each is off by a residual related to a slope not
#                   # equal to 1 and an incorrect bias

y1.slr.obs.pred.absresidual <-
  (y1.slr.obs.pred - y1.slr.the.pred)

y1.slr.obs.pred.absresidual # Note decreasing absolute residuals

y1.slr.obs.pred.pctresidual <-
  ((y1.slr.obs.pred - y1.slr.the.pred) / y1.slr.the.pred) * 100

y1.slr.obs.pred.pctresidual # Note large pct differences at values of y1 ~ 0

script3.1.04 <- 'Complete'

#END