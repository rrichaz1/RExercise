####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

#Marc d. Paradis
#R 3.4.1 for Windows
#RStudio 1.1.383

set.seed(183)
# Normally distributed numbers
x.norm <- 
  rnorm(
    n = 80, 
    mean = 50, 
    sd = 5
    )
hist(
  x.norm
  )
# Take these numbers to the fourth power to create a non-normal distribution
hist(x.norm^4)

# Uniformly distributed numbers
x.unif <- 
  runif(n = 80)
hist(
  x = x.unif
  )
# Take these numbers to the fourth power to create a non-uniform distribution
hist(
  x = x.unif^4
  )

# Compare the numbers sampled with rnorm() against normal distribution
qqnorm(
  y = x.norm
  )
qqline(
  y = x.norm, 
  distribution = qnorm, 
  probs = c(0.25, 0.75), 
  qtype = 7
  ) 
# defaults from documentation

# Sometimes getAnywere() gives us insight into the function formula and
# defaults

getAnywhere(
  x = qqline
  )

# We can see that the distribution parameter must be a function for which the
# parameter probs is the argument.  In other words:

qnorm(
  p = c(0.25, 0.75)
  )

# The same rnorm() numbers to the 4th power, compared to the normal 
# distribution
qqnorm(
  y = x.norm^4
  )
qqline(
  y = x.norm^4
  )

# Numbers sampled from the flat distribution, compared to normal
qqnorm(
  y = x.unif
  )
qqline(
  y = x.unif
  )

# Numbers sampled from the flat distribution, compared to normal
qqnorm(
  y = x.unif^4
  )
qqline(
  y = x.unif^4
  )

# Compare x.normand u
qqplot(
  x = x.norm, 
  y = x.unif
  )
qqline(
  y = x.unif
  )

# Where is the line? The issue here is that x.norm and x.unif are not scaled and 
# qqline() by default calculates relative to a standard normal distribution.
# It pays to read documentation and experiment until things make sense.

# Scale x.norm and u
x.norm.scale <- 
  (x.norm - mean(x.norm)) / sd(x.norm)
hist(
  x = x.norm.scale
  )
qqnorm(
  y = x.norm.scale
  )
qqline(
  y = x.norm.scale
  )

x.unif.scale <- 
  (x.unif - mean(x.unif)) / sd(x.unif)
hist(
  x = x.unif.scale
  )
qqnorm(
  y = x.unif.scale
  )
qqline(
  y = x.unif.scale
  )

# Q-Q Plot compare scaled x.norm and u
qqplot(
  x = x.norm.scale, 
  y = x.unif.scale
  )
qqline(
  y = x.unif.scale
  )

# Note that all scaling does in a Q-Q Plot is change the axis labels, it does
# not change any of the relationships between distributions. It is important
# however, when using qqline(). If you don't believe me, run this:
qqplot(
  x = x.norm, 
  y = x.unif
  )

# and then toggle back and forth. So qqplot() doesn't care about scaling, but
# qqline() does

# Begs the question then, is there a better way to plot a qqline, one which
# might also give us slope and intercept, r-squared, etc? Yes, but we have to
# do a little work.  We will use R's standard linear modeling function lm().
# We need to remember that implicit in a Q-Q Plot is an ordering of the 
# distributions from lowest to highest, so simple approach of:

lm(
  formula = x.unif ~ x.norm
  )

abline(  # abline() "understands" how to plot a linear model object
  lm(
    formula = x.unif ~ x.norm
    )
  )

# doesn't work. But there is a simple fix, just sort both distributions:

lm(
  formula = sort(x.unif) ~ sort(x.norm)
  )

abline(
  lm(
    formula = sort(x.unif) ~ sort(x.norm)
    )
  )

# If we put output of lm into the variable qqfit, we can get all kinds of
# information on the linear regression and its fit:

qqplot(
  x = x.norm, 
  y = x.unif
  )
qqfit.ux <- 
  lm(
    formula = sort(x.unif) ~ sort(x.norm)
    )
abline(qqfit.ux) # qqfit.ux is a fit or model object abline() "understands"
summary.lm(qqfit.ux)

# Note that we don't need to standardize in order for this work. This is a 
# significant improvement over qqline() where we do

# Let's try to use a Q-Q Plot on a non-normal distribution such as the chi-
# square distribution

# Let's generate some observed data:
x.chi <-
  rchisq(
    n = 100, 
    df = 2
    )

# Need to scale so we can apply qqline()
x.chi.scale <- 
  (x.chi - mean(x.chi)) / sd(x.chi)
qqnorm(
  y = x.chi.scale
  )
qqline(
  y = x.chi.scale
  )

# Clearly non-normal, but what if we want to assess is it what we would 
# expect for a random sample from a chi-square distribution. We could just
# random sample again, but that doesn't seem right, remember that we want to
# compare against the theoretical chi-square, it is that theoretical 
# distribution which allows us to generate an expected set of values.

# In R, we can use the function ppoints() to generate n equally spaced points
# between 0 and 1, inclusive

x.pp <- 
  ppoints(
    n = 100 # because n = 100 in observed sample, this is a best practice
    ) 

# If we put x.pp into the chi-square quantile function with the same df, then
# we get a string of 100 evenly space expected chi-square values

x.chi.expect <- 
  qchisq(
    p = x.pp, 
    df = 2
    )

# Note that these values are not normalized

qqplot(
  x = x.chi.expect, 
  y = x.chi
  )

# Remember that part of what a Q-Q Plot does is sort both observed and
# expected, we need to sort our observed data before we fit it to a linear
# model

qqfit.chi <- 
  lm(
    formula = sort(x.chi) ~ x.chi.expect
    )
summary.lm(object = qqfit.chi)

# We can now add this line to our Q-Q Plot

abline(
  lm(
    formula = sort(x.chi) ~ x.chi.expect
    )
  )

# and if we just try to do a qqline command, it fails because of scaling
qqline(
  y = x.chi
  )

# What if we take these same observed x.chi values and compare them to a
# theoretical standard normal expected distribution?

x.norm.expect <- 
  qnorm(
    p = x.pp
    )
qqplot(
  x = x.norm.expect, 
  y = x.chi
  )
qqfit.chinorm <- 
  lm(
    formula = sort(x.chi) ~ x.norm.expect
    )
summary.lm(
  object = qqfit.chinorm
  )
abline(qqfit.chinorm)

#Q-Q Plot compare normal distribution to a t distribution
n1 <- rnorm(n = 1000, mean = 0, sd = 1) #note already scaled
qqnorm(
  y = n1
  )
t1 <- rt(n = 1000, df = 10)
qqnorm(
  y = t1
  )

# So let's now plot n1 v t1
qqplot(x = n1, y = t1) #Curve is different because x-axis is different, it is derived
                       #from a theoretical normal curve, rather than the observed n1

qqfit.n1t1 <- 
  lm(
    formula = sort(t1) ~ sort(n1)
    )
summary.lm(qqfit.n1t1)

abline(qqfit.n1t1)

#Q-Q Plot compare two standard normal distributions
n2 <- rnorm(n = 1000, mean = 0, sd = 1) #note already scaled
qqplot(x = n1, y = n2)
qqfit.n1n2 <- 
  lm(
    formula = sort(n2) ~ sort(n1)
    )
abline(qqfit.n1n2)
summary.lm(qqfit.n1n2)

#Q-Q Plot compare two very non-standard normal distributions
n3 <- rnorm(n = 1000, mean = 10, sd = 0.5) #note raw, unscaled
qqnorm(y = n3)

n4 <- rnorm(n = 1000, mean = 150, sd = 30) #note raw, unscaled and different
qqnorm(
  y = n4
  )

qqplot(x = n3, y = n4)
qqfit.n3n4 <- 
  lm(
    formula = sort(n4) ~ sort(n3)
    )

abline(qqfit.n3n4)

summary.lm(qqfit.n3n4)

#Shapiro-Wilk Normality Test. 
#Null Hypothesis = Distribution is Normally Distributed
#p >  0.05 Fail to Reject Null Hypothesis
#p <= 0.05 Reject the Null Hypothesis (therefore: Distribution is not normal)
shapiro.test(x.norm)
shapiro.test(x.norm^4)
shapiro.test(x.norm.scale)
shapiro.test(x.unif)
shapiro.test(x.unif^4)
shapiro.test(x.unif.scale)
shapiro.test(x.chi)
shapiro.test(x.chi.scale)
shapiro.test(x.chi.expect)
shapiro.test(n1)  #dance of the p-values!
shapiro.test(n2)
shapiro.test(t1)
shapiro.test(n3)  #dance of the p-values!
shapiro.test(n4)

# Recreate the classic Q-Q Plot series
library(rmutil) #necessary for Laplace Distributional Model

# Note the masked function calls.  Both rmutil and psych packages have a
# function called plot.residuals.  Both rmutil and stats packages have a
# function called nobs.  They may or may not do the same thing, but if I type
# either function, R will use the rmutil defintion of the function.

# In R, the order of loading packages makes a difference, if two or more
# packages have identically named functions, then the package loaded last
# will mask the identically named functions of all preceding packages.

# You can use the following syntax to specify which library to run a
# particular function from:

# packagename :: functionname()
# psych :: plot.residuals()
# stats :: nobs()

# But we are okay with rmutil for now as we not using either masked function

n <- 1000
# Expected Standard Normal Theoretical Quantile
x.norm.expct <- 
  sort(
    qnorm(
      p = ppoints(
        n = n
        )
      )
    )

library(e1071) #Necessary for kurtosis() and skewness() functions below

# Observed Non-Standard/Non-Normal Quantiles
x.norm.obsvd <- 
  sort(
    rnorm(
      n = n, 
      mean = 137, 
      sd = 7.39
      )
    )
hist(
  x = x.norm.obsvd, 
  prob = T
  )
curve(
  dnorm(
    x = x, 
    mean = 137, 
    sd = 7.39
    ), 
  add = T
  )
kurtosis(x.norm.obsvd)
skewness(x.norm.obsvd)

x.laplace.obsvd <- 
  sort(
    rlaplace(
      n = n, 
      m = 5, 
      s = 0.5
      )
    ) #Fat/Heavy tailed
#I believe that s < 1 makes this leptokurtic
# http://ugrad.stat.ubc.ca/R/library/rmutil/html/Laplace.html
hist(
  x = x.laplace.obsvd, 
  prob = T
  )
curve(
  dnorm(
    x = x, 
    mean = mean(x.laplace.obsvd), 
    sd = sd(x.laplace.obsvd)), 
  add = T
  )
kurtosis(x.laplace.obsvd)
skewness(x.laplace.obsvd)

x.cosine.obsvd <- 
  sort(
    cos(
      seq(
        from = -pi, 
        to = pi,
        length.out = n
        )
      )
    ) #Narrow/Light tailed
hist(
  x = x.cosine.obsvd, 
  prob = T
  )
curve(
  dnorm(
    x = x, 
    mean = mean(x.cosine.obsvd), 
    sd = sd(x.cosine.obsvd)
    ), 
  add = T
  )
kurtosis(x.cosine.obsvd)
skewness(x.cosine.obsvd)

x.uniform.obsvd <- 
  sort(
    runif(
      n = n, 
      max = 1
      )
    ) #Narrow/Light tailed
hist(
  x = x.uniform.obsvd, 
  prob = T
  )
curve(
  dnorm(
    x = x, 
    mean = mean(x.uniform.obsvd), 
    sd = sd(x.uniform.obsvd)), 
  add = T
  )
kurtosis(x.uniform.obsvd)
skewness(x.uniform.obsvd)

x.beta.obsvd <- 
  sort(
    rbeta(
      n = n, 
      shape1 = 2, 
      shape2 = 2
      )
    ) #Narrow/Light tailed
hist(
  x = x.beta.obsvd, 
  prob = T
  )
curve(
  dnorm(
    x = x, 
    mean = mean(x.beta.obsvd), 
    sd = sd(x.beta.obsvd)
    ), 
  add = T
  )
kurtosis(x.beta.obsvd)
skewness(x.beta.obsvd)

x.lognorm.obsvd <- 
  sort(
    rlnorm(
      n = n, 
      meanlog = 1, 
      sdlog = 0.5
      )
    ) #Right skewed
hist(
  x = x.lognorm.obsvd, 
  prob = T
  )
curve(
  dnorm(
    x = x, 
    mean = mean(x.lognorm.obsvd), 
    sd = sd(x.lognorm.obsvd)
    ), 
  add = T
  )
kurtosis(x.lognorm.obsvd)
skewness(x.lognorm.obsvd)

library(fGarch) #Necessary for skew normal distribution modeling
# LOOKS LIKE fGarch HAS KURTOSIS AND SKEW FUNCTIONS THAT MASK psych
x.negskewnorm.obsvd <- 
  sort(
    rsnorm(
      n = n, 
      mean = 0, 
      sd = 1, 
      xi = 0.1
      )
    ) #Left Skewed
hist(
  x = x.negskewnorm.obsvd, 
  prob = T
  )
curve(
  dnorm(
    x = x, 
    mean = mean(x.negskewnorm.obsvd), 
    sd = sd(x.negskewnorm.obsvd)
    ), 
  add = T
  )
kurtosis(x.negskewnorm.obsvd)
skewness(x.negskewnorm.obsvd)

x.posskewnorm.obsvd <- 
  sort(
    rsnorm(
      n = n, 
      mean = 0, 
      sd = 1, 
      xi = 10
      )
    ) #Right Skewed
hist(
  x = x.posskewnorm.obsvd, 
  prob = T
  )
curve(
  dnorm(
    x = x, 
    mean = mean(x.posskewnorm.obsvd), 
    sd = sd(x.posskewnorm.obsvd)
    ), 
  add = T
  )
kurtosis(x.posskewnorm.obsvd)
skewness(x.posskewnorm.obsvd)
plot(x.posskewnorm.obsvd)

x.bimodal.obsvd <- 
  sort(
    rbind(
      n2 + 5,
      n3 - 1
      )
    )
x.bimodal.obsvd <- 
  cbind(
    x.bimodal.obsvd, 
    c(1,2)
    )
x.bimodal.obsvd <- 
  x.bimodal.obsvd[x.bimodal.obsvd[, 2] == 1 , 1]
hist(
  x = x.bimodal.obsvd, 
  prob = T
  )
curve(
  dnorm(
    x = x, 
    mean = mean(x.bimodal.obsvd), 
    sd = sd(x.bimodal.obsvd)
    ), 
  add = T
  )
kurtosis(x.bimodal.obsvd)
skewness(x.bimodal.obsvd)


# Let's start plotting
qqplot(
  x = x.norm.expct, 
  y = x.norm.obsvd, 
  main = 'Observed Normal'
  )
abline(
  lm(
    formula = x.norm.obsvd ~ x.norm.expct
    )
  )
shapiro.test(x.norm.obsvd)

qqplot(
  x = x.norm.expct, 
  y = x.laplace.obsvd, 
  main = 'Observed Leptokurtic'
  )
abline(
  lm(
    formula = x.laplace.obsvd ~ x.norm.expct
    )
  )
shapiro.test(x.laplace.obsvd)

qqplot(
  x = x.norm.expct, 
  y = x.cosine.obsvd, 
  main = 'Observed Platykurtic'
  )
abline(
  lm(
    formula = x.cosine.obsvd ~ x.norm.expct
    )
  )
shapiro.test(x.cosine.obsvd)

qqplot(
  x = x.norm.expct, 
  y = x.uniform.obsvd, 
  main = 'Observed Platykurtic'
  )
abline(
  lm(
    formula = x.uniform.obsvd ~ x.norm.expct
    )
  )
shapiro.test(x.uniform.obsvd)

qqplot(
  x = x.norm.expct, 
  y = x.beta.obsvd, 
  main = 'Observed Platykurtic'
  )
abline(
  lm(
    formula = x.beta.obsvd ~ x.norm.expct
    )
  )
shapiro.test(x.beta.obsvd)

qqplot(
  x = x.norm.expct, 
  y = x.lognorm.obsvd, 
  main = 'Observed Positive/Right Skew'
  )
abline(
  lm(
    formula = x.lognorm.obsvd ~ x.norm.expct
    )
  )
shapiro.test(x.lognorm.obsvd)

qqplot(
  x = x.norm.expct, 
  y = x.posskewnorm.obsvd, 
  main = 'Observed Positive / Right Skew'
  )
abline(
  lm(
    formula = x.posskewnorm.obsvd ~ x.norm.expct
    )
  )
shapiro.test(x.posskewnorm.obsvd)

qqplot(
  x = x.norm.expct, 
  y = x.negskewnorm.obsvd, 
  main = 'Observed Negative / Left Skew'
  )
abline(
  lm(
    formula = x.negskewnorm.obsvd ~ x.norm.expct
    )
  )
shapiro.test(x.negskewnorm.obsvd)

qqplot(
  x = x.norm.expct, 
  y = x.bimodal.obsvd, 
  main = 'Observed Bimodal'
  )
abline(
  lm(
    formula = x.bimodal.obsvd ~ x.norm.expct
    )
  )
shapiro.test(x.bimodal.obsvd)

# Comparing Theoretical Distributions Against Each Other

n <- 100 # Nbr of Sample points (or draws)
N <- 10 # Nbr of Trials
p <- ppoints(n)
xlim <- c(-10,10)

x.unif.expct <- 
  qunif(
    p = p, 
    min = -1, 
    max = 1
    )
hist(
  x = x.unif.expct, 
  probability = T, 
  xlim = xlim
  )
curve(
  dunif(
    x = x, 
    min = -1, 
    max = 1
    ), 
  add = T
  ) #doesn't appear to work with discrete
plot(x.unif.expct)

x.bern.expct <- 
  qbinom(
    p = p, 
    size = 1, 
    prob = 0.5
    )
hist(
  x = x.bern.expct, 
  probability = T, 
  xlim = xlim
  )
curve(
  dbinom(
    x = x, 
    size = 1, 
    prob = 0.5
    ), 
  add = T
  ) #not sure what is going on with density (y-axis)

x.binom.expct <- 
  qbinom(
    p = p, 
    size = N, 
    prob = 0.5
    )
hist(
  x = x.binom.expct, 
  probability = T, 
  xlim = xlim
  )
curve(
  dbinom(
    x = x, 
    size = N, 
    prob = 0.5
    ), 
  add = T
  ) #doesn't appear to work with discrete

x.geom.expct <- 
  qgeom(
    p = p, 
    prob = 0.5
    )
hist(
  x = x.geom.expct
  )
hist(
  x = x.geom.expct, 
  probability = T, 
  xlim = xlim
  )
curve(
  dgeom(
    x = x, 
    prob = 0.5
    ), 
  add = T
  )

x.hyper.expct <- 
  qhyper(
    p = 0.9, 
    m = 5, 
    n = 45, 
    k = 10
    )
dhyper(
  x = 4, 
  m = 5, 
  n = 45, 
  k = 10
  ) #0.4 %Chance of pulling 4 whites (Density)
dhyper(
  x = 0:11, 
  m = 5, 
  n = 45, 
  k = 10
  )
hist(
  dhyper(
    x = 0:11, 
    m = 5, 
    n = 45, 
    k = 10
    )
  )

plot(
  x = 0:11, 
  y = dhyper(
    x = 0:11, 
    m = 5, 
    n = 45, 
    k = 10
    )
  )

phyper(
  q = 4, 
  m = 5, 
  n = 45, 
  k = 10
  ) #99.98% (PDF)
phyper(
  q = 0:11, 
  m = 5, 
  n = 45, 
  k = 10
  ) 
hist(
  phyper(
    q = 0:11, 
    m = 5, 
    n = 45, 
    k = 10
    )
  ) 
qhyper(
  p = 0.9998811, 
  m = 5, 
  n = 45, 
  k = 10
  ) #5 (quantile)
qhyper(
  p = 0.9, 
  m = 5, 
  n = 45, 
  k = 10
  ) #5 (quantile)
rhyper(
  nn = 10, 
  m = 5, 
  n = 45, 
  k = 10
  )

qhyper(
  p = dhyper(
    x = 0:11, 
    m = 5, 
    n = 45, 
    k = 10
    ), 
  m = 5, 
  n = 45, 
  k = 10
  ) #5 (quantile)

x.hyper.expct
hist(
  x = x.hyper.expct
  )
hist(
  x = x.hyper.expct, 
  probability = T, 
  xlim = xlim
  )
curve(
  dhyper(
    x = x, 
    m = 5, 
    n = 5, 
    k = 5
    ), 
  add = T
  )

rm(list=ls()) #Clears global environment of all visible objects (some hidden
              #objects may remain)

# END


