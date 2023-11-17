####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.4.1 for Windows
# RStudio 1.1.383

# Adapted from http://dwoll.de/rexrepos/posts/multRegression.html#TOC

n <- 100 # Our standard value for n in these scripts, if you have changed it 
         # elsewhere then update it here too

set.seed(351)
X1 <- 
  rnorm(
    n, 
    175, 
    7
    )
hist(X1)

X2 <- 
  rnorm(
    n, 
    30, 
    8
    )
hist(X2)

X3 <- 
  abs(
    rnorm(
      n, 
      60, 
      30
      )
    )
hist(X3)

Y1.err <-
  rnorm(
    n, 
    0, 
    10
  )
hist(Y1.err)

Y2.err <-
  rnorm(
    n, 
    10
  )
hist(Y2.err)

Y1 <- 
  10 + (0.2 * X1) + (-0.3 * X2) + (-0.4 * X3) + Y1.err

Y2 <- -
  5 + (0.3 * X2) + (0.2 * X3) + Y2.err

Y  <- 
  cbind(Y1, Y2)

plot(Y) # Note Y1 and Y2 are NOT independent. This is b/c they share
        # explanatory variables

df.MvLR <- 
  data.frame(
    X1, 
    X2, 
    X3, 
    Y1, 
    Y2
    )

m.MvLR <- 
  lm(
    cbind(Y1, Y2) ~ X1 + X2 + X3, 
    data = df.MvLR
    )

summary(m.MvLR)

coef(
  lm(
    Y1 ~ X1 + X2 + X3, 
    data = df.MvLR
    )
  )

coef(
  lm(
    Y2 ~ X1 + X2 + X3, 
    data = df.MvLR
    )
  )

par(mfrow = c(2,2))
plot(
  lm(
    Y1 ~ X1 + X2 + X3, 
    data = df.MvLR
  )
)
par(mfrow = c(1,1))

par(mfrow = c(2,2))
plot(
  lm(
    Y2 ~ X1 + X2 + X3, 
    data = df.MvLR
  )
)
par(mfrow = c(1,1))

# Type I Sum of Squares

summary(
  manova(m.MvLR), 
  test = 'Hotelling-Lawley'
  )

summary(
  manova(m.MvLR), 
  test = 'Wilks'
  )

summary(
  manova(m.MvLR), 
  test = 'Roy'
  )

summary(
  manova(m.MvLR), 
  test = 'Pillai'
  )

# Type II / III Sum of Squares

library(car) # for Manova()

Manova(
  m.MvLR, 
  type = 'II'
  )

Manova(
  m.MvLR, 
  type = 'III'
  )

# Type II and Type III essentially identical because no interaction term

rm(list=ls())

