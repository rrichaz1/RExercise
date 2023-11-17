####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# It would be good to add an example where we use glm() and lm() to show that
# they are equivalent

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

# glm_example.R
# Yezhou Sun

# Example of Generalized Linear Models (GLM).
# We will use data sets from MASS and faraway libraries.
library(MASS)
library(faraway)

# GLM with Binomial distribution of errors.
# Usually used for binary outcome.
data(biopsy)
str(biopsy)

# Biopsy data includes sample ID, sample class (benign or malignant) 
# and 9 different measurements on biopsy samples.
# Use ? biopsy to get details.
# Remove sample IDs.

biopsy <- 
  biopsy[, -1] # Removes first column

# Note that we will be repeating this format for much of the rest of this
# script: model, summary, anova, chi-square, prediction
m1 <- 
  glm(
    class ~ ., # Kitchen-sink modeling
    data = biopsy, 
    family = binomial # This is what makes it Logit/Logistic Regression
    )

str(m1)
summary(m1)


m1a <- 
  glm(
    class ~ ., # Kitchen-sink modeling
    data = biopsy, 
    family = binomial(
      link = 'logit' # This is really what makes it Logit/Logistic Regression
      ) # the link function was implicit above with default value = 'logit'
    )

str(m1a)
summary(m1a)

# Let's specify the model

m1b <- 
  glm(
    class ~ V1 + V4 + V6 + V7, # Specified model
    data = biopsy, 
    family = binomial # Remember link = 'logit' is default
    ) # This is an example of how forgiving R can be in its core functions.
#     # Works even if you do logit instead of 'logit'

summary(m1b)

# Just because you can do this (m1) doesn't mean that you should. You should
# explicitly test for endogeneity / multicollinearity and determine whether 
# dropping or combining (or both) two or more of your independent variables is
# the reasoned / right thing to do. We are not going to do that here, but you
# should do it as an exercise on your own.

# When given a glm() model object, the anova() function returns an Analysis of
# Deviance table which step-wise adds features to the model and assess the 
# reductions in variance from the NULL model.

anova(m1)

# Test for goodness-of-fit of the model.
# A significant p value indicates model does NOT fit well with the data.

pchisq( # Will return the p-value for ...
  q = summary(m1)$deviance, # ... this quantile ...
  df = summary(m1)$df.residual # ... with this many degress of freedom
  )

# That's a pretty convincing p-value

# Make predictions using model.
pred1 <- 
  predict(
    m1, 
    newdata = biopsy, 
    type = 'response'
    )

str(pred1)
head(pred1)
summary(pred1)

plot(pred1)
plot(sort(pred1))

pred1.se <- 
  predict(
    m1, 
    newdata = biopsy, 
    type = 'response', 
    se.fit = TRUE # Default is FALSE
  )$se.fit

plot(
  pred1, 
  pred1.se
  )

# A vector of probability of each sample being malignant.

# GLM with Poisson distribution of errors.
# Usually used when outcome is count data and not over-dispersed.
data(quine)
str(quine)
head(quine)

# Note that Sex and Age are already factors and so they can be used directly
# in this example with a Poisson regression (i.e. counts)

m2 <- 
  glm(
    Days ~ Age + Sex, 
    data = quine, 
    family = poisson(
      link = 'log' # Default value for family = poisson
      )
    )

summary(m2)

anova(m2)

pchisq(
  summary(m2)$deviance,  
  summary(m2)$df.residual
  )

# Because outcome is continuous value, we can also get standard error of 
# prediction.
pred2 <- 
  predict(
    m2, 
    newdata = quine, 
    type = 'response'
    )

plot(pred2)
plot(sort(pred2)) # Note "steps" this is b/c a count response which is why we
                  # used family = poisson

pred2.se <- 
  predict(
    m2, 
    newdata = quine, 
    type = 'response', 
    se.fit = T
    )$se.fit

plot(
  pred2, 
  pred2.se
  )

# GLM with Gamma distribution of errors.
# Can be used when outcome is zero bound positive values whose mean and variance
# are 'linked'. One example is the cost of care. Or silicon wafer production...
data(wafer)
str(wafer)

# Assuming the effects of independent variables are additive
m3 <- 
  glm(
    formula = resist ~ ., 
    data = wafer, 
    family = Gamma( # Try to think a little bit about what it means to assert
                    # a Gamma distribution as the best fit link function
                    # a Poisson or binomial or linear
      link = 'identity' # N.B. default link for Gamma is 'inverse'
      )
    )

summary(m3)

anova(m3)

pchisq(
  summary(m3)$deviance,
  summary(m3)$df.residual
  )

pred3 <- 
  predict(
    m3, 
    newdata = wafer, 
    type = 'response'
    )

plot(pred3)
plot(sort(pred3))

pred3.se <- 
  predict(
    m3, 
    newdata = wafer, 
    type = 'response', 
    se.fit = T
    )$se.fit

plot(
  pred3, 
  pred3.se
  )

# Assuming the effects of independent variables are multiplicative.
m4 <- 
  glm(
    formula = resist ~ ., 
    data = wafer, 
    family = Gamma(
      link = 'log'
      )
    )

summary(m4)

anova(m4)

pchisq(
  summary(m4)$deviance,  
  summary(m4)$df.residual
  )

pred4 <- 
  predict(
    m4, 
    newdata = wafer, 
    type = 'response'
    )

plot(pred4)
plot(sort(pred4))

pred4.se <- 
  predict(
    m4, 
    newdata = wafer, 
    type = 'response', 
    se.fit = T
    )$se.fit

plot(
  pred4, 
  pred4.se
  )

####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

rm(
  list = ls()
  ) # Clears global environment of all visible objects (some hidden
#   # objects may remain)

# End