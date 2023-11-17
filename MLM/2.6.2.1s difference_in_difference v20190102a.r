####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

#Marc d. Paradis
#R 3.5.1 for Windows
#RStudio 1.1.456


# difference_in_difference.R
# Yezhou Sun

# To study the effect of EITC (Earned Income Tax Credit) on labor force 
# participation rate of single women when it went into effect in 1994. Eissa, 
# Nada, and Jeffrey B. Liebman. 1996. Labor Supply Responses to the Earned 
# Income Tax Credit. Quarterly Journal of Economics. 111(2): 605-637

wrkng.path <- 
  'C:/Users/mparadis/Desktop/DSU/Immersion College/02 MLM/02. Statistics/2.6 Investigational Design/'

# Note you have to change backslashes (\) to forward slashes (/) Note setwd()
# is a more elegant approach but most UHG/UHC/Optum machines are locked down
# and you can't change the working directory or add folders to the default 
# working directory.

eitc <- 
  read.csv(
    paste(
      wrkng.path,
      '2.6.2.1 eitc.csv',
      sep = ''
      )
    )

str(eitc)

# Create two additional dummy variables to indicate before/after EITC and 
# treatment/control groups.

eitc$post93 <- 
  as.numeric(eitc$year >= 1994)

# EITC only affects women with at least one child.

eitc$anykids <- 
  as.numeric(eitc$children >= 1)
 
# Compute the four data points needed in the DID calculation:

a <- 
  sapply(
    subset(
      eitc, 
      post93 == 0 & anykids == 0, 
      select = work
      ), 
    mean
    )

b <- 
  sapply(
    subset(
      eitc, 
      post93 == 0 & anykids == 1, 
      select = work
      ), 
    mean
    )

c <- 
  sapply(
    subset(
      eitc, 
      post93 == 1 & anykids == 0, 
      select = work
      ), 
    mean
    )

d <- 
  sapply(
    subset(
      eitc, 
      post93 == 1 & anykids == 1, 
      select = work
      ), 
    mean
    )
 
# Compute the effect of EITC on the employment of women with children:

(d-c)-(b-a)

#       work 
# 0.04687313 

# Simple D-I-D regression

summary(
  lm(
    work ~ post93 + anykids + post93*anykids, 
    data = eitc
    )
  )

# Coefficients:
#                 Estimate Std. Error t value             Pr(>|t|)    
# (Intercept)     0.575460   0.008845  65.060 < 0.0000000000000002 ***
# post93         -0.002074   0.012931  -0.160              0.87261    
# anykids        -0.129498   0.011676 -11.091 < 0.0000000000000002 ***
# post93:anykids  0.046873   0.017158   2.732              0.00631 ** 

# Note that both methods show that EITC will increase labor force participation
# rate of women with at least one child by ~4.7%.

x <- 
  tapply(
    eitc$work, 
    list(
      eitc$anykids, 
      eitc$year
      ), 
    mean)

xx <- 
  as.numeric(colnames(x))

aa <- 
  matrix(
    c(a, 1991, a, 1994), 
    byrow = T, 
    ncol = 2
    )

bb <- 
  matrix(
    c(b, 1991, b, 1994), 
    byrow = T, 
    ncol = 2
    )

cc <- 
  matrix(
    c(c, 1994, c, 1996), 
    byrow = T, 
    ncol = 2
    )

dd <- 
  matrix(
    c(d, 1994, d, 1996), 
    byrow = T, 
    ncol = 2
    )

png(
  file = paste(
    wrkng.path,
    'DID.png',
    sep = ''
    ), 
  width = 800, 
  height = 800
  )

plot(
  xx, 
  x[1, ], 
  xlab = 'Year', 
  ylab = 'Rate', 
  ylim = c(0.4, 0.65), 
  cex = 2, 
  cex.main = 2, 
  cex.lab = 1.5, 
  main = 'Single Women\n Labor Force Participation Rate', 
  sub = 'Horizontal line represents overall average', 
  col = 'blue'
  )

points(
  xx, 
  x[2, ], 
  cex = 2, 
  col = 'red'
  )

abline(
  v = 1994, 
  col = 'yellow', 
  lwd = 2)

points(
  aa[ ,2], 
  aa[ ,1], 
  col = 'blue', 
  lwd = 2, 
  type = 'l'
  )

points(
  bb[ ,2], 
  bb[ ,1], 
  col = 'red', 
  lwd = 2, 
  type = 'l'
  )

points(
  cc[ ,2], 
  cc[ ,1], 
  col = 'blue', 
  lwd = 2, 
  type = 'l'
  )

points(
  dd[ ,2], 
  dd[ ,1], 
  col = 'red', 
  lwd = 2, 
  type = 'l'
  )

legend(
  'topright', 
  cex = 2, 
  legend = c('No child', '1+ children'), 
  lty = c(1, 1), 
  lwd = c(1, 1), 
  col = c('blue', 'red')
  )

dev.off()

rm(
  list = ls()
  ) # Clears global environment of all visible objects (some hidden objects
    # may remain)

# END