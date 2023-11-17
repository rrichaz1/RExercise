####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

#Marc d. Paradis
#R 3.5.1 for Windows
#RStudio 1.1.456

# Based on: simple_linear_regression
# By: Yezhou Sun

options(
  scipen = 999
  )

# This script borrowed materials from 
# http://tutorials.iq.harvard.edu/R/Rstatistics/Rstatistics.html

wrkng.path <- 
  'C:/Users/mparadis/Desktop/DSU/Immersion College/02 MLM/03. Supervised Regression/3.1 Linear Regression/Example Scripts/' 
#note you have to change backslashes (\) to forward slashes (/)

states <- 
  readRDS(
    paste(
      wrkng.path,
      '3.1.1.0 states.rds',
      sep = ''
    )
  )

# Meta data of states
str(states)

# Select and profile subset of data
sat.expense <- 
  states[, c('csat', 'expense', 'percent')]

summary(sat.expense)

cor(sat.expense)

plot(
  csat ~ expense, 
  data = sat.expense
  )

# Fit a simple linear regression model
m1 <- 
  lm(
    formula = csat ~ expense, 
    data = sat.expense
    )

summary(m1)

str(m1)

summary(m1)$r.squared
# R-squared=0.2174337
# Results indicate that per-capita expenditure on students is a strong 
# (p = 0.000563) predictor for SAT score.

plot(
  csat ~ expense, 
  data = sat.expense
  )
abline(m1)

# Regression diagnostics
# Residuals plot
plot(
  m1$fitted.values, 
  m1$residuals, 
  xlab = 'Fitted values', 
  ylab = 'Residuals', 
  main = 'Residuals ~ Fitted Values'
  )
abline(
  h = 0, 
  col = 'red'
  )

plot(
  m1$residuals,
  ylab = 'Residuals',
  main = 'Residual Plot')
abline(
  h = 0, 
  col = 'red'
  )

hist(m1$residuals)

shapiro.test(m1$residuals)

# Q-Q plot
qqnorm(
  m1$residuals, 
  ylab = 'Residuals', 
  main = 'Residuals Normal Q-Q Plot'
  )
qqline(
  m1$residuals, 
  col = 'red'
  )

# Fit another regression model
m2 <- 
  lm(
    csat ~ expense + percent, # Note that this is multi-variable linear 
                              # regression because there is more than one
                              # independent variable (feature)
    data = sat.expense
    )

summary(m2)

str(m2)

summary(m2)$r.squared
# R-squared=0.7857307
# Results indicate that after controlling percentage of students taking SAT in
# the state, per-capita expenditure on students only marginally (p = 0.0462) 
# predicts SAT score.

# R-squared indicates that m2 fits a better reression line than m1.

plot(
  csat ~ percent, 
  data = sat.expense
  )

plot(
  expense ~ percent, 
  data = sat.expense
  )

# Regression diagnostics
plot(
  m2$fitted.values, 
  m2$residuals, 
  xlab = 'Fitted values', 
  ylab = 'Residuals', 
  main = 'Residuals ~ Fitted Values'
  )
abline(
  h = 0, 
  col = 'red'
  )

plot(
  m2$residuals,
  ylab = 'Residuals',
  main = 'Residual Plot'
  )
abline(
  h = 0, 
  col = 'red'
  )

hist(m2$residuals)

shapiro.test(m2$residuals)

qqnorm(
  m2$residuals, 
  ylab = 'Residuals', 
  main = 'Residuals Normal Q-Q Plot'
  )
qqline(
  m2$residuals, 
  col = 'red'
  )

# Fit 'Kitchen Sink' Model
m3 <- 
  lm(
    csat ~ ., 
    data = states
    )

summary(m3)

m4 <- 
  lm(
    csat ~ pop+area+density+metro+waste+energy+miles+toxic+green+house+senate+
      vsat+msat+percent+expense+income+high+college, 
    data = states
    )

summary(m4) # Note that this is a farily obvious example of label leakage 
            # (csat = vsat + msat)

m4a<-
  summary(
    lm(
      csat ~ pop+area+density+metro+waste+energy+miles+toxic+green+house+senate+
        vsat+percent+expense+income+high+college, 
      data = states
      )
    )

summary(m4a) # This is a less obvious example of label leakage because vsat is
             # only one component of csat. But look closely at the coefficients
             # table and see if you can spot any warning signs.

m5 <- 
  lm(csat ~ pop+area+density+metro+waste+energy+miles+toxic+green+house+senate+
       percent+expense+income+high+college, 
     data = states
     )

summary(m5)

summary(m5)$r.squared

# Regression diagnostics
# Residuals plot
plot(
  m5$fitted.values, 
  m5$residuals, 
  xlab = 'Fitted values', 
  ylab = 'Residuals', 
  main = 'Residuals ~ Fitted Values'
  )
abline(
  h = 0, 
  col = 'red'
  )

plot(
  m5$residuals,
  ylab = 'Residuals',
  main = 'Residual Plot'
  )
abline(
  h = 0, 
  col = 'red'
  )

hist(m5$residuals)

shapiro.test(m5$residuals)

# Q-Q plot
qqnorm(
  m5$residuals, 
  ylab = 'Residuals', 
  main = 'Residuals Normal Q-Q Plot'
  )
qqline(
  m5$residuals, 
  col = 'red'
  )

# Compare model using ANOVA
anova(m1, m2)
# Results indicate that two models are significantly different (p=4.206e-15), 
# and if we use residual sum of squares (RSS) as metric, m2 is better than m1.

rm(
  list = ls()
  ) # Clears global environment of all visible objects (some hidden objects
    # may remain)

# END
