####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

#Marc d. Paradis
#R 3.5.1 for Windows
#RStudio 1.1.456

# Example of the Discrete Uniform Distribution
# A Fair Six-Sided Die

x.outcome.classical <- 1:6 # By default R coerces this vector to be integer

# This is actually a Random Variable representation of pips 1 to 6, which are
# the actual physical outcomes. There is also an implicit interval scale
# associated with this RV i.e. that the outcome 2 is twice as large as the
# outcome 1, and the outcome 4 is twice as large as the outcome 2, etc. While
# this is technically incorrect as the actual scale of pips is nominal. The
# pips on the die could as easily be a pictures of a dog, a cat, a bird, a
# house, a car and a banana as easily as the numbers 1, 2, 3, 4, 5, 6.

# The first R distribution function we will look at is dunif() which will take
# an interval RV input as x-values and output the corresponding probability as
# a y-value. d stands for distribution.

# dunif(
#   x = , # vector of outcomes
#   min = 0, # lower limit of distribution
#   max = 1, # upper limit of distribution
#   log = FALSE # do not log transform probabilities
# )

# So let's use the min() function in R to set the lower limit on our 
# distribution.

x.min.classical <- min(x.outcome.classical)

# and the max() functionto set the upper limit on our distribution.

x.max.classical <- max(x.outcome.classical)

# We can now calculate the theoretical probability densities that for our 
# discrete Random Variable.

y.probdens <- # y-axis output: vector of outcome probabilities
  dunif(
    x = x.outcome.classical, # x-axis input: vector of outcomes as  
    # discrete, interval RV
    min = x.min.classical,
    max = x.max.classical
  )

#Let's take a look at the values in our new vector:

y.probdens

# but wait, there is something wrong.  We have six outcomes and each outcome
# appears to have a probability of 0.2, 6 * 0.2 = 1.2 and you cannot have a
# probability greater than 1!

# It turns out that this is due to a quirk in the dunif() in R.  When
# calculating the range, R subtracts the minimum (1) from the maximum (6) and
# thinks that there are 5 discrete values for the Random Variable.  For this 
# reason we need to make a small tweak to the min argument in the dunif():

y.probdens <- # y-axis output: vector of outcome probabilities
  dunif(
    x = x.outcome.classical, # x-axis input: vector of outcomes as  
    # discrete, interval RV
    min = x.min.classical - 1, # quirk of R dunif() range calculation
    max = x.max.classical
  )

y.probdens

# Much better! 6 * .16666667 = 1.0, just as we would expect from the
# theoretical discrete uniform distribution for a Random Variable with 6
# discrete outcomes.

# The histogram function in R takes the following form:

# hist(
#   x = , # values to be binned in the histogram
#   breaks = , # calculates the number of bins in the histogram
#   freq = TRUE, # if True then y-axis = freq; if False then 
                 # y-axis = probability mass
#   xlab = , # x-axis label
#   plot =  TRUE #
# )

# For the moment, let's take the defaults and see what happens

hist(
  x = y.probdens
  )

# Whoa! that doesn't look right...

# The hist() function is counting the number of identical values in the vector
# y.pdf and finds 6 values, each equal to 0.1667. So it puts 6 on the y-axis
# as "Frequency" and creates one bin bounded by 0.1 and 0.2 to place those
# six identical values into. Not very useful or very accurate.

# Let's try another, more explicit approach. Let's put our x values and our 
# y values into a dataframe and try again with a barplot.

barplot(
  height = y.probdens, # height of discrete uniform distribution (y-axis)
  main = 'PDF for Discrete Uniform Distribution', # Plot title
  names.arg = x.outcome.classical, # discrete outcomes (x-axis)
  xlab = 'Outcome (x)', # x-axis label
  ylab = 'Probability Mass', # y-axis label
  ylim = c(
    0, 
    round(
      x = max(y.probdens),
      digits = 1
      )
    ) # creates vector of values from 0 to 0.2
  )

# By contrast, the barplot graphs ordinal RV outcomes on the 'x-axis' and the
# discrete probability density on the y-axis

# The next R distribution function, punif is really exactly the same as dunif,
# it takes the same outputs, but rather than returning the probability density
# at each input x, it returns the cumulative, that is the sum of all previous,
# probability densities output by the distribution function.

y.cumprobdens <- # y-axis output: vector of cumulative probabilities of ordinal 
                 # outcomes
  punif(
    q = x.outcome.classical, # x-axis input: vector of outcomes as discrete, 
                             # interval RV
    min = x.min.classical,
    max = x.max.classical
    )

hist(y.cumprobdens)

# Default hist() function is even worse here, namely because it gets the count
# of bins wrong - this is a common problem with the hist() function in R.

df.unif.discrete.cdf <- 
  as.data.frame(
    cbind(
      x.outcome.classical,
      y.cumprobdens
      )
    )

plot(
  df.unif.discrete.cdf,
  main = 'CDF for Discrete Uniform Distribution'
  )

barplot(
  height = y.cumprobdens,
  main = 'CDF for Discrete Uniform Distrubtion',
  names.arg = c(1:6),
  ylab = 'y.cumprobdens'
  )

# The simplest way to understand qunif(), R's quantile distribution function
# is to think of it as the inverse of punif().  qunif() takes a set of 
# probabilities as inputs (x-axis) and outputs a set of outcomes (y-axis),
# expressed as a continous RV.

x.outcome.rv <- # y-axis output: vector of outcomes as a continuous RV
  qunif(
    p = y.cumprobdens, # x-axis input: vector of cumulative probability 
                       # densities
    min = x.min.classical,
    max = x.max.classical
    )

x.outcome.rv 

# N.B. if input cumulative probability densities, then output is identical to
# the classical outcomes expressed as an RV that we started with

x.outcome.rv == x.outcome.classical

# That can be a problematic output if our range is large and there is a small
# number of FALSEs. A more elegant way to assess the same is

all.equal(
  x.outcome.rv, 
  x.outcome.classical
  )

# Just to show you that the output is a continuous RV, we can input a vector
# of probabilities which do not map to discrete outcomes

x.outcome.rv <- 
  qunif(
    p = y.cumprobdens / pi, #Try replacing pi with values like 10 or 7
    min = x.min.classical,
    max = x.max.classical
    )

x.outcome.rv

# No surprise, if you input identical probabilities, the qunif() function
# outputs identical outcomes

x.outcome.rv <- 
  qunif(
    p = y.probdens,
    min = x.min.classical,
    max = x.max.classical
    )

# The last type of uniform distribution function we will look at is runif().
# runif() models a random sample of outcomes from a given number of trials.
# In our example here, runif() will simulate 10 independent rolls of our fair
# six-sided die. As an input, runif() takes a number of rolls (sometimes
# also called trials or observations) and outputs that number of observed
# outcomes. If n is equal to 10, then 

round(
  runif(
    n = 10, #x-axis input: n = number of rolls, trials or observations
    min = x.min.classical + 0.5,
    max = x.max.classical + 0.5
    )
  )

# You will notice some extra "stuff", namely a rounding function and an
# offset or bias of 0.5 added to both the minimum and maximum. This is
# necessary because the uniform distribution modeled in R is a continuous
# distribution which we need to convert into a discrete distribution. The
# offset insures that continuous outcomes less than 0.5 are rounded up and
# that there are is an equal number of opportunities to round values between
# 5.5 and 6.0 up.

# [0.5 - 1.5) -> 1
# [1.5 - 2.5) -> 2
# [2.5 - 3.5) -> 3
# [3.5 - 4.5) -> 4
# [4.5 - 5.5) -> 5
# [5.5 - 6.5) -> 6

# If we didn't do this we would be missing out on half the opportunities to
# round up to 6

# [0.5 - 1.5) -> 1
# [1.5 - 2.5) -> 2
# [2.5 - 3.5) -> 3
# [3.5 - 4.5) -> 4
# [4.5 - 5.5) -> 5
# [5.5 - 6.0] -> 6

# Note that in the very rare chance that 6.0000000 is pulled from the 
# continuous uniform distribution, this code will actually round up to 7. The
# chances of this occurring are EXCEEDINGLY small, but to be safe I could
# write defensive code such as

round(
  runif(
    n = 10, # x-axis input: n = number of rolls, trials or observations
    min = x.min.classical + 0.5,
    max = x.max.classical + 0.499999999 # Note 9 sig figs
    )
  )

# This is a bit of a clunky way to model discrete uniform distributions, so
# R has a much more elegant way using the sample() function

sample(
  x = 1:6, # Range of discrete outcomes
  size = 10, # Number of rolls, trials or observations
  replace = T # For every roll, all discrete outcomes are available
  )

# An important aspect of how R, or any programming language for that matter,
# models random selection is the concept of pseudorandom number generation.
# Pseudorandom refers to the fact that the generation of the random number
# actually requires a seed, by default a system timestamp. If you want to 
# return the same random number each time you run a function you can use the
# set.seed() function.

# In order to demonstrate this, I am going to first create a simple for-loop.
# In R the template for a for-loop is as follows

# variable.to.repeat <- NULL
# for(i in seq){
#   variable.to.repeat <- function()
#   print(variable.to.repeate)
#   }

x.outcome.loop <- NULL
for(i in 1:5){
  x.outcome.loop <- 
    sample(
      x = 1:6, # Range of discrete outcomes
      size = 10, # number of rolls, trials or observations
      replace = T # For every roll, all discrete outcomes are available
  )
  print(x.outcome.loop)
  }

# First you will notice that each run through the loop generates a different
# set of 10 outcomes. Try running the loop a few times and you will also
# notice that the outcomes (rolls of the die) are different every time.

# Now let's set the seed

x.outcome.loop <- NULL
for(i in 1:5){
  set.seed(1234) # Note that the seed needs to be set INSIDE the loop (lexical 
                 # scope)
  x.outcome.loop <- 
    sample(
      x = 1:6, # Range of discrete outcomes
      size = 10, # Number of rolls, trials or observations
      replace = T # For every roll, all discrete outcomes are available
  )
  print(x.outcome.loop)
}

# Back to our discrete RV uniform distribution. For simplicity we will use
# the sample() function

n <- 10 # x-axis input: number of rolls, trials or observations
y.sample.obsvd <- # y-axis output: observed outcomes
  sample(
    x = 1:6, # Range of discrete outcomes
    size = n, # x-axis input: number of rolls, trials or observations
    replace = T # For every roll, all discrete outcomes are available
         )

hist(y.sample.obsvd)

df.unif.discrete.sample <- 
  as.data.frame(
    cbind(
      c(1:n),
      y.sample.obsvd
      )
    )

plot(
  df.unif.discrete.sample,
  main = 'Observed Sample for Discrete Uniform Distribution'
  )

barplot(
  y.sample.obsvd,
  main = 'Observed Sample for Discrete Uniform Distribution',
  names.arg = c(1:n),
  ylab = 'y.sample.obsvd'
  )

table(y.sample.obsvd)
barplot(table(y.sample.obsvd))

n.frequentist <- 1000000 # x-axis input: 1,000,000 rolls, trials or 
                         # observations
y.sample.frequentist <- # y-axis output: observed outcomes
  sample(
    x = 1:6, # Range of discrete outcomes
    size = n.frequentist, # x-axis input: number of rolls, trials or 
                          # observations
    replace = T # For every roll, all discrete outcomes are available
    )
y.sample.frequentist <- table(y.sample.frequentist)

y.sample.frequentist <- y.sample.frequentist / n.frequentist

barplot(
  y.sample.frequentist,
  main = 'Frequentist Sample for Discrete Uniform Distribution'
  )

# Example of a Bernoulli Distribution/Process
# Flipping a fair coin

# Bernoulli Distribution is a special case of the Binomial Distribution where
# there is only one flip of the coin

size.bern <- 1 #single trial, flip or observation
prob.fair <- 0.5 #50-50 fair coin

x.outcome.bern <- c(0,1)

y.probdens.bern <- # y-axis output: vector of probabilities
  dbinom(
    x = x.outcome.bern, # x-axis input: vector of outcomes as discrete RV
    size = size.bern, # Single flip of the coin
    prob = prob.fair # Fair coin
    )

# We know that hist() has problems with discrete distributions so we will 
# stay with plot(s)) and barplot()

df.bern.discrete.pdf <- 
  as.data.frame(
    cbind(
      x.outcome.bern,
      y.probdens.bern
      )
    )

plot(df.bern.discrete.pdf)

barplot(y.probdens.bern)

y.cumdens.bern <- # y-axis output: 
  pbinom(
    q = x.outcome.bern, # x-axis input:
    size = size.bern,
    prob = prob.fair
    )

df.bern.discrete.cdf <-
  as.data.frame(
    cbind(
      x.outcome.bern,
      y.cumdens.bern
      )
    )

plot(df.bern.discrete.cdf)

barplot(y.cumdens.bern)

y.outcome.rv.bern <- #y-axis output: 
  qbinom(p = y.cumdens.bern, #x-axis input:
         size = size.bern,
         prob = prob.fair
         )

y.outcome.rv.bern

y.sample.frequentist.bern <- # y-axis output: observed outcomes
  sample(
    x = 0:1, # Range of discrete outcomes
    size = n.frequentist, # x-axis input: number of rolls, trials or 
                               # observations
    replace = T # For every roll, all discrete outcomes are available
    )
y.sample.frequentist.bern <- table(y.sample.frequentist.bern)

y.sample.frequentist.bern <- y.sample.frequentist.bern / n.frequentist

barplot(
  y.sample.frequentist.bern,
  main = 'Frequentist Sample for Discrete Bernoulli Distribution'
  )

# Example of a Binomial Distribution
# Flipping a fair coin many times

size.binom <- 100 # Number of flips, trials or observations
nbr.successes <- c(0:size.binom) # Number of successes, whether heads or tails, 
                                 # just choose one to be the success

y.probdens.binom <- # y-axis output: probability of that number of successes
  dbinom(
    x = nbr.successes, # x-axis input: number of successes
    size = size.binom,
    prob = prob.fair
    )

df.binom.discrete.pdf <-
  cbind(
    nbr.successes,
    y.probdens.binom
    )

plot(df.binom.discrete.pdf)

barplot(y.probdens.binom)

y.cumdens.binom <-
  pbinom(
    q = nbr.successes,
    size = size.binom,
    prob = prob.fair
    )

df.binom.discrete.cdf <-
  cbind(
    nbr.successes,
    y.cumdens.binom
    )

plot(df.binom.discrete.cdf)

barplot(y.cumdens.binom)

x.outcomes.binom <-
  qbinom(
    p = y.cumdens.binom,
    size = size.binom,
    prob = prob.fair
    )

y.sample.frequentist.binom <-
  rbinom(
    n = n.frequentist, 
    size = size.binom, 
    prob = prob.fair
    )

head(y.sample.frequentist.binom)

plot(y.sample.frequentist.binom) # Because of large n, takes time to plot

# Super Important Point: Even though the y-axis on all probability
# distributions is the same, the x-axis differs significantly between them!

rm(list=ls()) #Clears global environment of all visible objects (some hidden
              #objects may remain)

#END
