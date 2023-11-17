####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

#Marc d. Paradis
#R 3.5.1 for Windows
#RStudio 1.1.456

n <- 10000 # Number of units pulled per sample (Rule of Thumb minimum n > 30)
r <- 1000 # Number of samples (Trials) pulled from population (No Rule of Thumb
          # but larger is better)

set.seed(213)
z <- 
  rnorm(
    n = n, 
    mean = 0, 
    sd = 1
    )

set.seed(214)
b <- 
  rbeta(
    n = n, 
    shape1 = 0.5, 
    shape2 = 0.5
    )

set.seed(215)
u <- 
  runif(
    n = n, 
    min = 0, 
    max = 1
    )

hist(
  x = z,
  main = 'Histogram of Std Normal Curve (z)'
  )
plot(
   x = z
  )
mean(z)
sd(z)
# Theoretical mean = 0

hist(
  x = b,
  main = 'Histogram of Beta Distribution (b)'
  )
plot(
  x = b
  )
mean(b)
sd(b)
# Theoretical mean = 0.5

hist(
  x = u,
  main = 'Histogram of Uniform Continuous Distrbution (u)'
  )
plot(
  x = u
  )
mean(u)
sd(u)
# Theoretical mean = 0.5

# R likes to have a defined vector to iterate over. It is a best practice to 
# create either an empty vector, or an appropriately sized and typed vector.
clt.mean <- NULL

# For loop to run through multiple r-samples of n-pulls each
# Population is a Standard Normal Distribution
for(i in 1:r){
  z <- 
    rnorm(
      n = n, 
      mean = 0, 
      sd = 1
      )
  i <- 
    mean(z)
  clt.mean <- 
    rbind(
      clt.mean, 
      i
      )
  }

hist(
  x = clt.mean
  )
mean(clt.mean) #should be 0
mean(z)
sd(clt.mean)
sd(z)
sd(z) / sqrt(n)
sd(clt.mean) * sqrt(n)
qqplot(
  x = qnorm(
    ppoints(r)
    ), 
  y = clt.mean
  )
abline(
  lm(
    formula = sort(clt.mean) ~ sort(qnorm(ppoints(r)))
    )
  )
summary.lm(
  object = lm(
    formula = sort(clt.mean) ~ sort(qnorm(ppoints(r)))
    )
  )
shapiro.test(clt.mean) #p > 0.05 means failed to disprove normality

# Clearing out vector so I can re-use it below for the beta distribution
clt.mean <- NULL

# For loop to run through multiple r-samples of n-pulls each
# Population is a beta distribution
for(i in 1:r){
  b <- 
    rbeta(
      n = n, 
      shape1 = 0.5, 
      shape2 = 0.5
      )
  i <- 
    mean(b)
  clt.mean <- 
    rbind(
      clt.mean, 
      i
      )
  }

hist(
  x = clt.mean
  )
mean(clt.mean) #should be 0.5
mean(b)
sd(clt.mean)
sd(b)
sd(b) / sqrt(n)
sd(clt.mean) * sqrt(n)
qqplot(
  x = qnorm(
    ppoints(r)
    ), 
  y = clt.mean
  )
abline(
  lm(
    formula = sort(clt.mean) ~ sort(qnorm(ppoints(r)))
    )
  )
summary.lm(
  object = lm(
    formula = sort(clt.mean) ~ sort(qnorm(ppoints(r)))
    )
  )
shapiro.test(clt.mean) #p > 0.05 means failed to disprove normality

# Clearing out vector so I can re-use it below for the beta distribution
clt.mean <- NULL

# For loop to run through multiple r-samples of n-pulls each
# Population is a beta distribution
for(i in 1:r){
  u <- 
    runif(
      n = n, 
      min = 0, 
      max = 1
      )
  i <- 
    mean(u)
  clt.mean <- 
    rbind(
      clt.mean, 
      i
      )
  }

hist(
  x = clt.mean
  )
mean(clt.mean) #should be 0.5
mean(u)
sd(clt.mean)
sd(u)
sd(u) / sqrt(n)
sd(clt.mean) * sqrt(n)
qqplot(
  x = qnorm(
    ppoints(r)
    ), 
  y = clt.mean
  )
abline(
  lm(
    formula = sort(clt.mean) ~ sort(qnorm(ppoints(r)))
    )
  )
summary.lm(
  object = lm(
    formula = sort(clt.mean) ~ sort(qnorm(ppoints(r)))
    )
  )
shapiro.test(clt.mean) #p > 0.05 means failed to disprove normality

# Clearing out vector so I can re-use it below for the beta distribution
clt.mean <- NULL

# For loop to run through multiple r-samples of n-pulls each
# Population is a Standard Normal distribution
for(i in 1:r){
  z.sd <- 
    rnorm(
      n = n, 
      mean = 0, 
      sd = 1
      )
  i <- 
    sd(z.sd)
  clt.mean <- 
    rbind(
      clt.mean, 
      i
      )
  }

hist(
  x = clt.mean
  )
mean(clt.mean) #should be 1
mean(z.sd)
sd(clt.mean)
sd(z.sd)
sd(z.sd) / sqrt(n)
sd(clt.mean) * sqrt(n)
qqplot(
  x = qnorm(
    p = ppoints(r)
    ), 
  y = clt.mean
  )
abline(
  lm(
    formula = sort(clt.mean) ~ sort(qnorm(ppoints(r)))
    )
  )
summary.lm(
  object = lm(
    formula = sort(clt.mean) ~ sort(qnorm(ppoints(r)))
    )
  )
shapiro.test(clt.mean) #p > 0.05 means failed to disprove normality

rm(
  list = ls()
  ) # Clears global environment of all visible objects (some hidden objects may
    # remain)

# END
