####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

#Marc d. Paradis
#R 3.5.1 for Windows
#RStudio 1.1.456


# continuous_distribution.R
# Created by Yezhou Sun

options(
  scipen = 999
  )

set.seed(241)

sample.size <- 100000

# Normal distribution
set.seed(241)
x1 <- 
  rnorm(
    sample.size, 
    50, 
    5
    )

set.seed(242)
x2 <- 
  rnorm(
    sample.size, 
    50, 
    3
    )

plot(
  density(x1), 
  lwd = 2, 
  ylim = c(0, 0.18), 
  col = 'red', 
  main = 'Normal Distributions', 
  xlab = 'Value'
  )
points(
  density(x2), 
  type = 'l', 
  lwd = 2, 
  col = 'blue'
  )
legend(
  'topright', 
  legend = c(
    expression(
      paste(
        mu, 
        '=50',
        ', ',
        sigma, 
        '=5', 
        sep = ''
        )
      ),
    expression(
      paste(
        mu, 
        '=50', 
        ', ', 
        sigma, 
        '=3', 
        sep = ''
        )
      )
    ),
  lty = c(1, 1), 
  lwd = c(1, 1), 
  col = c('red', 'blue')
  )

# Log normal distribution

set.seed(243)
x1 <- 
  rlnorm(
    sample.size, 
    1, 
    0.4
    )

set.seed(244)
x2 <- 
  rlnorm(
    sample.size, 
    1, 
    0.6
    )

plot(
  density(x1), 
  lwd = 2, 
  col = 'blue', 
  main = 'Log Normal Distributions', 
  xlab = 'Value'
  )
points(
  density(x2), 
  type = 'l', 
  lwd = 2, 
  col = 'red'
  )
legend(
  'topright', 
  legend = c(
    expression(
      paste(
        'log(', 
        mu, 
        ')=1', 
        ', log(', 
        sigma, 
        ')=0.6', 
        sep = ''
        )
      ),
    expression(
      paste(
        'log(', 
        mu, 
        ')=1', 
        ', log(', 
        sigma, 
        ')=0.4', 
        sep = ''
        )
      )
    ),
  lty = c(1, 1), 
  lwd = c(1, 1), 
  col = c('red', 'blue')
  )

# Standard normal distribution

set.seed(245)
x1 <- 
  rnorm(
    sample.size, 
    0, 
    1
    )

plot(
  density(x1), 
  lwd = 2, 
  col = 'blue', 
  main = 'Standard Normal Distributions', 
  xlab = 'Value'
  )
legend(
  'topright', 
  legend = c(
    expression(
      paste(
        mu, 
        '=0', 
        ', ', 
        sigma, 
        '=1', 
        sep = ''
        )
      )
    ),
  lty = c(1), 
  lwd = c(1), 
  col = c('blue')
  )

# Student's t distribution

set.seed(246)
x1 <- 
  rt(
    sample.size, 
    sample.size-1
    )

plot(
  density(x1), 
  lwd = 2, 
  col = 'blue',  
  main = 'Students t distribution', 
  xlab = 'Value'
  )
legend(
  'topright', 
  legend = c(
    paste(
      'N=', 
      sample.size, 
      '\nDF=', 
      sample.size - 1, 
      sep = ''
      )
    ),
  lty = c(1), 
  lwd = c(1), 
  col = c('blue')
  )

# Chi squared distribution

set.seed(247)
x1 <- 
  rchisq(
    sample.size, 
    10
    )

set.seed(248)
x2 <- 
  rchisq(
    sample.size, 
    15
    )

plot(
  density(x1), 
  lwd = 2, 
  col = 'red', 
  main = expression(
    paste(
      displaystyle(chi^2), 
      ' Distributions', 
      sep = ''
      )
    ), 
  xlab = 'Value'
  )
points(
  density(x2), 
  type = 'l', 
  lwd = 2, 
  col = 'blue'
  )
legend(
  'topright', 
  legend = c('DF=10', 'DF=15'), 
  lty = c(1, 1), 
  lwd = c(1, 1), 
  col = c('red', 'blue')
  )

# F distribution

set.seed(249)
x1 <- 
  rf(
    sample.size, 
    100, 
    100
    )

set.seed(250)
x2 <- 
  rf(
    sample.size, 
    100, 
    1000
    )

plot(
  density(x2), 
  lwd = 2, 
  col = 'blue', 
  main = 'F Distributions', 
  xlab = 'Value'
  )
points(
  density(x1), 
  type = 'l', 
  lwd = 2, 
  col = 'red'
  )
legend(
  'topright', 
  legend = c('DF1=100, DF2=100', 'DF1=100, DF2=1000'), 
  lty = c(1, 1), 
  lwd = c(1, 1), 
  col = c('red', 'blue')
  )

# Exponential distribution

set.seed(251)
x1 <- 
  rexp(
    sample.size, 
    1
    )

set.seed(252)
x2 <- 
  rexp(
    sample.size, 
    2
    )

plot(
  density(x2), 
  lwd = 2, 
  col = 'blue', 
  main = 'Exponential Distributions', 
  xlab = 'Value'
  )
points(
  density(x1), 
  type = 'l', 
  lwd = 2, 
  col = 'red'
  )
legend(
  'topright', 
  legend = c('rate=1', 'rate=2'), 
  lty = c(1, 1), 
  lwd = c(1, 1), 
  col = c('red', 'blue')
  )

# Weibull distribution

set.seed(253)
x1 <- 
  rweibull(
    sample.size, 
    1, 
    1
    )

set.seed(254)
x2 <- 
  rweibull(
    sample.size, 
    2, 
    1
    )

set.seed(255)
x3 <- 
  rweibull(
    sample.size, 
    2, 
    2
    )

plot(
  density(x2), 
  lwd = 2, 
  col = 'blue', 
  main = 'Weibull Distributions', 
  xlab = 'Value'
  )
points(
  density(x1), 
  type = 'l', 
  lwd = 2, 
  col = 'red'
  )
points(
  density(x3), 
  type = 'l', 
  lwd = 2, 
  col = 'green')
legend(
  'topright', 
  legend = c(
    'shape=1, 
    scale=1', 
    'shape=2, scale=1', 
    'shape=2, scale=2'
    ), 
  lty = c(1, 1, 1), 
  lwd = c(1, 1, 1), 
  col = c('red', 'blue', 'green')
  )

# Logistic distribution

set.seed(256)
x1 <- 
  rlogis(
    sample.size, 
    location = 0, 
    scale = 1
    )

set.seed(257)
x2 <- 
  rlogis(
    sample.size, 
    location = 0, 
    scale = 2
    )

plot(
  density(x1), 
  lwd = 2, 
  col = 'blue', 
  main = 'Logistic Distributions', 
  xlab = 'Value'
  )
points(
  density(x2), 
  type = 'l', 
  lwd = 2, 
  col = 'red'
  )
legend(
  'topright', 
  legend = c(
    'scale=2', 
    'scale=1'
    ), 
  lty = c(1, 1), 
  lwd = c(1, 1), 
  col = c('red', 'blue')
  )

# Gamma distribution

set.seed(258)
x1 <- 
  rgamma(
    sample.size, 
    shape = 2, 
    rate = 1
    )

set.seed(259)
x2 <- 
  rgamma(
    sample.size, 
    shape = 1, 
    rate = 1
    )

set.seed(260)
x3 <- 
  rgamma(
    sample.size, 
    shape = 2, 
    rate = 2
    )

plot(
  density(x2), 
  lwd = 2, 
  col = 'blue', 
  main = expression(
    paste(
      displaystyle(Gamma), 
      ' Distributions', 
      sep = ''
      )
    ),
  xlab = 'Value'
  )
points(
  density(x1), 
  type = 'l', 
  lwd = 2, 
  col = 'red'
  )
points(
  density(x3), 
  type = 'l', 
  lwd = 2, 
  col = 'green'
  )
legend(
  'topright', 
  legend = c(
    'shape=2, rate=1', 
    'shape=1, rate=1', 
    'shape=2, rate=2'
    ), 
  lty = c(1, 1, 1), 
  lwd = c(1, 1, 1), 
  col = c('red', 'blue', 'green')
  )

# Beta distribution

set.seed(261)
x1 <- 
  rbeta(
    sample.size, 
    shape1 = 1.5, 
    shape2 = 1
    )

set.seed(262)
x2 <- 
  rbeta(
    sample.size,
    shape1 = 1, 
    shape2 = 1
    )

set.seed(263)
x3 <- 
  rbeta(
    sample.size, 
    shape1 = 1.5, 
    shape2 = 1.5
    )

plot(
  density(x1), 
  lwd = 2, 
  col = 'blue', 
  main = expression(
    paste(
      displaystyle(Beta), 
      ' Distributions', 
      sep = ''
      )
    ),
  xlab = 'Value'
  )
points(
  density(x2), 
  type = 'l', 
  lwd = 2, 
  col = 'red'
  )
points(
  density(x3), 
  type = 'l', 
  lwd = 2, 
  col = 'green'
  )
legend(
  'topleft', 
  legend = c(
    'shape1=1, shape2=1', 
    'shape=1.5, shape2=1', 
    'shape=1.5, shape=1.5'
    ),
  lty = c(1, 1, 1), 
  lwd = c(1, 1, 1), 
  col = c('red', 'blue', 'green')
  )

rm(
  list = ls()
  ) # Clears global environment of all visible objects (some hidden
    # objects may remain)

# END
