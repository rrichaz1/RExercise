####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

#Marc d. Paradis
#R 3.5.1 for Windows
#RStudio 1.1.456

# discrete_distribution.R
# Created by Yezhou Sun

options(
  scipen = 999 # Limits significant digits when printing numbers
  ) 

sample.size <- 100000 # Number (n) of experimental units: die rolls, coin 
                      # flips, draws from an urn, )

# Uniform Discrete Distribution - Rolling a 10-Sided Die

set.seed(231)
ud.rv <- # Sample of random variates from a Uniform Discrete Distribution 
          # (y-axis)
  sample( # Function to pull a random sample a range of discrete values
    x = 1:10, # Range of potential values in generative population (x-axis)
    size = sample.size, # Number of observations (trials)
    replace = T # Whether values are replaced after each trial
    )

table(ud.rv)

ud.hist <- 
  hist(
    ud.rv, 
    plot = F, 
    breaks = seq(
      min(ud.rv) - 0.5, 
      max(ud.rv) + 0.5
      )
    )

barplot(
  height = ud.hist$density, 
  ylim = c(0, max(ud.hist$density) + 0.02), 
  names.arg = ud.hist$mids,
  #lwd = 2, 
  #pch = 17, 
  #type = 'b', 
  col = 'white', 
  main = 'Uniform Discrete Distribution (10-Sided Die)', 
  xlab = 'Outcome (x)', 
  ylab = 'Random Variate Probability Mass'
  )
points(
  x = ud.hist$mids,
  y = dunif(
    x = ud.hist$mids,
    min = 0,
    max = 10 # Because dunif calculates max - min (11 - 1 = 10)
    ), 
  xlim = c(1:10)
  )

# Binomial distribution
set.seed(232)
binom1.rv <- 
  rbinom(
    n = sample.size, 
    size = 1000, 
    prob = 0.51
    )

table(binom1.rv)

set.seed(233)
binom2.rv <- 
  rbinom(
    n = sample.size, 
    size = 1000, 
    prob = 0.49)

table(binom2.rv)

d1 <- 
  hist(
    binom1.rv, 
    plot = F, 
    breaks = seq(
      min(binom1.rv) - 0.5, 
      max(binom1.rv) + 0.5
      )
    )

d2 <- 
  hist(
    binom2.rv, 
    plot = F, 
    breaks = seq(
      min(binom2.rv) - 0.5, 
      max(binom2.rv) + 0.5
      )
    )

plot(
  d1$mids, 
  d1$density, 
  ylim = c(0, max(d1$density) + 0.02), 
  lwd = 2, 
  pch = 15, 
  type = 'b', 
  col = 'red', 
  main = 'Binomial Distributions', 
  xlab = 'Number of Successes (k)', 
  ylab = 'Random Variate Probability Mass'
  )
points(
  d2$mids, 
  d2$density, 
  type = 'b', 
  pch = 17, 
  lwd = 2, 
  col = 'blue'
  )
legend(
  'topright', 
  legend = c(
    'n (trials) = 1,000', 
    'p = 0.51', 
    'p = 0.49'
    ), 
  lty = c(0, 1, 1), 
  lwd = c(1, 1, 1), 
  col = c('white', 'red', 'blue')
  )

# Hypergeometric distribution

set.seed(234)
x1 <- 
  rhyper(
    nn = sample.size, # Number of observations (trials)
    m = sample.size * 0.5, # Number of white balls in the urn
    n = sample.size * 0.5, # Number of black balls in the urn
    k = 1000 # Number of balls drawn from the urn
    )

set.seed(235)
x2 <- 
  rhyper(
    nn = sample.size, 
    m = sample.size * 0.51,  # Number of white balls in the urn
    n = sample.size * 0.49,  # Number of black balls in the urn
    k = 1000)

d1 <- 
  hist(
    x1, 
    plot = F, 
    breaks = seq(
      min(x1) - 0.5, 
      max(x1) + 0.5
      )
    )

d2 <- 
  hist(
    x2, 
    plot = F, 
    breaks = seq(
      min(x2) - 0.5, 
      max(x2) + 0.5
      )
    )

plot(
  d1$mids, 
  d1$density, 
  ylim = c(0, max(d1$density) + 0.02), 
  lwd = 2, 
  pch = 15, 
  type = 'b', 
  col = 'red', 
  main = 'Hypergeometric Distributions', 
  xlab = 'Number of k Successes in n Draws', 
  ylab = 'Random Variate Probability Density'
  )
points(
  d2$mids, 
  d2$density, 
  type = 'b', 
  pch = 17, 
  lwd = 2, 
  col = 'blue'
  )
legend(
  'topright', 
  legend = c(
    'r = 100,000 \n n (draws) = 1,000', 
    paste(
      'white =', 
      sample.size * 0.5, 
      ', black = ', 
      sample.size * 0.5, 
      sep = ''
      ), 
    paste(
      'white = ', 
      sample.size * 0.51, 
      ', black = ', 
      sample.size * 0.49, 
      sep = ''
      )
    ), 
  lty = c(0, 1, 1), 
  lwd = c(1, 1, 1), 
  col = c('white', 'red', 'blue')
  )

# Geometric distribution

set.seed(236)
x1 <- 
  rgeom(
    n = sample.size, # number of observations (trials)
    prob = 0.6 # 
    )

set.seed(237)
x2 <- 
  rgeom(
    n = sample.size, 
    prob = 0.4
    )

d1 <- 
  hist(
    x1, 
    plot = F, 
    breaks = seq(
      min(x1) - 0.5, 
      max(x1) + 0.5
      )
    )

d2 <- 
  hist(
    x2, 
    plot = F, 
    breaks = seq(
      min(x2) - 0.5, 
      max(x2) + 0.5
      )
    )

plot(
  d1$mids, 
  d1$density, 
  ylim = c(0, max(d1$density) + 0.02), 
  lwd = 2, 
  pch = 15, 
  type = 'b', 
  col = 'red', 
  main = 'Geometric Distributions', 
  xlab = 'Value', 
  ylab = 'Density'
  )
points(
  d2$mids, 
  d2$density, 
  type = 'b', 
  pch = 17, 
  lwd = 2, 
  col = 'blue'
  )
legend(
  'topright', 
  legend = c('p=0.6', 'p=0.4'), 
  lty = c(1, 1), 
  lwd = c(1, 1), 
  col = c('red', 'blue')
  )

# Poisson distribution

set.seed(238)
x1 <- 
  rpois(
    n = sample.size, 
    lambda = 2
    )

set.seed(239)
x2 <- 
  rpois(
    n = sample.size, 
    lambda = 3
    )

d1 <- 
  hist(
    x1, 
    plot = F, 
    breaks = seq(
      min(x1) - 0.5, 
      max(x1) + 0.5
      )
    )

d2 <- 
  hist(
    x2, 
    plot = F, 
    breaks = seq(
      min(x2) - 0.5, 
      max(x2) + 0.5
      )
    )

plot(
  d1$mids, 
  d1$density, 
  ylim = c(0, max(d1$density) + 0.02),
  lwd = 2, 
  pch = 15, 
  #type = 'b', 
  col = 'red', 
  main = 'Poisson Distributions', 
  xlab = 'Value', 
  ylab = 'Density'
  )
points(
  d2$mids, 
  d2$density, 
  #type = 'b', 
  pch = 17, 
  lwd = 2, 
  col = 'blue'
  )
legend(
  'topright', 
  legend = c(
    expression(
      paste(
        lambda, 
        '= 2', 
        sep = ''
        )
      ), 
    expression(
      paste(
        lambda, 
        '= 3', 
        sep = ''
        )
      )
    ), 
  lty = c(1, 1), 
  lwd = c(1, 1), 
  col = c('red', 'blue')
  )

# Negative binomial distribution

set.seed(240)
x1 <- 
  rnbinom(
    n = sample.size, 
    size = 1, 
    p = 0.5
    )

set.seed(241)
x2 <- 
  rnbinom(
    n = sample.size, 
    size = 6, 
    p = 0.5
    )

set.seed(242)
x3 <- 
  rnbinom(
    n = sample.size, 
    size = 6, 
    p = 0.7
    )

d1 <- 
  hist(
    x1, 
    plot = F, 
    breaks = seq(
      min(x1) - 0.5, 
      max(x1) + 0.5
      )
    )

d2 <- 
  hist(
    x2, 
    plot = F, 
    breaks = seq(
      min(x2) - 0.5, 
      max(x2) + 0.5
      )
    )

d3 <- 
  hist(
    x3, 
    plot = F, 
    breaks = seq(
      min(x3) - 0.5, 
      max(x3) + 0.5
      )
    )

plot(
  d1$mids, 
  d1$density, 
  ylim = c(0, max(d1$density) + 0.02), 
  lwd = 2, 
  pch = 15, 
  #type = 'b', 
  col = 'red', 
  main = 'Negative Binomial Distributions', 
  xlab = 'Value', 
  ylab = 'Density'
  )
points(
  d2$mids, 
  d2$density, 
  #type = 'b', 
  pch = 17, 
  lwd = 2, 
  col = 'blue'
  )
points(
  d3$mids,
  d3$density, 
  #type = 'b', 
  pch = 16, 
  lwd = 2, 
  col = 'green'
  )
legend(
  'topright', 
  legend = c(
    'size = 1, p = 0.5',
    'size = 6, p = 0.5', 
    'size = 6, p = 0.7'), 
  lty = c(1, 1, 1), 
  lwd = c(1, 1, 1), 
  col = c('red', 'blue', 'green')
  )

rm(
  list = ls()
  ) # Clears global environment of all visible objects (some hidden objects
    # may remain)

# END

