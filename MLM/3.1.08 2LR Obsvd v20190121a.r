####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

#Marc d. Paradis
#R 3.5.1 for Windows
#RStudio 1.1.456

# CONFIRM THAT: script3.1.01 = 'Complete'
# CONFIRM THAT: script3.1.02 = 'Complete'
# CONFIRM THAT: script3.1.03 = 'Complete'
# CONFIRM THAT: script3.1.04 = 'Complete'
# CONFIRM THAT: script3.1.07 = 'Complete'

set.seed(310800) # To ensure reproducible results

delta.y2.ind <- # Set ground truth residuals
  rnorm( # pulled from a random normal distribution
    n = n, # of sample size n
    mean = 0, # with theoretical mean of zero
    sd = 0.1 * 1000 # and theoretical standard devation that is 10% error of
    # absolute range (max - min) of independent RV x1
  )

y1.2lr.obs <-
  (y1.2lr.the + delta.y1.ind + delta.y2.ind) + epsilon # element-wise addition 
                                                       # of residuals and
                                                       # addition of bias term,
                                                       # recycled across dv and
                                                       # residual vectors

df.2lr.ind.obs <-
  as.data.frame(
    cbind(
      y1.2lr.obs,
      x1.ind,
      x2.ind
    )
  )

m.2lr.ind.obs <-
  lm(
    y1.2lr.obs ~ x1.ind + x2.ind,
    data = df.2lr.ind.obs
  )

# ASSUMPTION #1: Linearity of Parameters
summary(m.2lr.ind.obs)
# y = 35 + (1.5 * x1) + (1.5 * x2)

summary(m.2lr.ind.obs)$r.squared

plot(
  y1.2lr.obs ~ x1.ind,
  data = df.2lr.ind.obs
)
abline(
  lm(
    y1.2lr.obs ~ x1.ind + x2.ind,
    data = df.2lr.ind.obs
    ),
  col = 'green'
  )
abline(
  lm(
    y1.2lr.obs ~ x1.ind,
    data = df.2lr.ind.obs
  ),
  col = 'red'
)

plot(
  y1.2lr.obs ~ x2.ind,
  data = df.2lr.ind.obs
)
abline(
  lm(
    y1.2lr.obs ~ x1.ind + x2.ind,
    data = df.2lr.ind.obs
  ),
  col = 'green'
)
abline(
  lm(
    y1.2lr.obs ~ x2.ind,
    data = df.2lr.ind.obs
  ),
  col = 'red'
)

par(mfrow = c(2,2))
plot(m.2lr.ind.obs)

par(mfrow = c(1,1)) # Resets par(mfrow) to default setting

# ASSUMPTION #2: Random Sample

# Because epsilon effects only the y values all analysis under this condtion
# will be identical to the analysis under the 2LR condition

# ASSUMPTION #3 Zero Condtional Mean rho(residuals, x.sub.i) = 0

m.2lr.ind.obs.end <-
  lm(
    m.2lr.ind.obs$residuals ~ x1.ind + x2.ind # regressing Residuals v. Independents
  )

summary(m.2lr.ind.obs.end)

summary(m.2lr.ind.obs.end)$coefficients[c(2,3)] # Slope of correlation

summary(
  lm(
    m.2lr.ind.obs$residuals ~ x1.ind # regressing Residuals v. Independents
  )
)

# Fail to reject H0 so analysis is consistent with no dependence of residuals
# on x1.ind

summary(
  lm(
    m.2lr.ind.obs$residuals ~ x2.ind # regressing Residuals v. Independents
  )
)

# Fail to reject H0 so analysis is consistent with no dependence of residuals
# on x2.ind

cor(
  m.2lr.ind.obs$residuals, 
  x1.ind
) # 6.519833e-17 is _really_ close to zero (No correlation)

cor(
  m.2lr.ind.obs$residuals, 
  x2.ind
) # -2.168095e-17 is _really_ close to zero (No correlation)

# ASSUMPTION #4 Independence of Regressors rho(x.sub.i, x.sub.j) = 0 for all
# i and all j where i <> j

# Only two Regressors in this model so a simple correlation will suffice

cor.test(
  x = df.2lr.ind.obs$x1.ind,
  y = df.2lr.ind.obs$x2.ind,
  alternative = 'two.sided',
  method = 'pearson',
  exact = NULL,
  conf.level = 0.95
)

par(mfrow = c(1,1))

# Fail to reject the Null Hypothesis so data are consistent with a true
# correlation = 0

# ASSUMPTION #5 Homoscedasticity of Residuals wrt to Independents

plot(
  m.2lr.ind.obs$residuals ~ x1.ind
)
abline(m.2lr.ind.obs.end)

plot(
  m.2lr.ind.obs$residuals ~ x2.ind
)
abline(m.2lr.ind.obs.end)

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
# with a Normal distribution

hist(
  m.2lr.ind.obs$residuals # Residual
)

#Shapiro-Wilk Normality Test. 
#Null Hypothesis = Distribution is Normally Distributed
#p >  0.05 Fail to Reject Null Hypothesis
#p <= 0.05 Reject the Null Hypothesis (therefore: Distribution is not normal)

shapiro.test(
  m.2lr.ind.obs$residuals # Residual
)

# Fail to Reject the Null Hypothesis because p > 0.05. Reseiduals are consistent
# with a Normal distribution

# ASSUMPTION #7 Independence of Experimental Units (within  v between subjects)

# TBD

# Predictions based on model and newdata
y1.2lr.obs.pred <-
  predict(
    object = m.2lr.ind.obs,
    newdata = df.x1x2.pred
  )

# We have already run the New, Novel and Out-of-Range data through the true 
# model. Now we reuse that model as epsilon technically is a deviation from the
# true values as captured in y1.slr.the.pred

# Create dataset of Modeled v. Predicted for x1.pred and x2.pred input

df.2lr.obs.theVpred <-
  as.data.frame(
    cbind(
      y1.2lr.obs.pred,
      y1.2lr.the.pred
    )
  )

m.2lr.obs.theVpred <-
  lm(
    y1.2lr.the.pred ~ y1.2lr.obs.pred,
    data = df.2lr.obs.theVpred
  )

summary(m.2lr.obs.theVpred) # Perfect Fit: Slope = 1.027 and Intercept = -142.3 
#                           # Represents relatively minor impact of noise and
#                           # bias over such a broad range of x and y.

plot(
  x = y1.2lr.the.pred,
  y = y1.2lr.obs.pred
)

df.2lr.obs.theVpred # See that each is off by a residual related to a slope not
#                   # equal to 1 and an incorrect bias

y1.2lr.obs.pred.absresidual <-
  (y1.2lr.obs.pred - y1.2lr.the.pred)

y1.2lr.obs.pred.absresidual # Note decreasing absolute residuals

y1.2lr.obs.pred.pctresidual <-
  ((y1.2lr.obs.pred - y1.2lr.the.pred) / y1.2lr.the.pred) * 100

y1.2lr.obs.pred.pctresidual # Note large differences at values of y1 ~ 0

script3.1.08 <- 'Complete'

# END