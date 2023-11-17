####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

#Marc d. Paradis
#R 3.5.1 for Windows
#RStudio 1.1.456


# collinearity.R
# Yezhou Sun/Marc d. Paradis

# Multicollinearity
wrkng.path <- 
  'C:/Users/mparadis/Desktop/DSU/Immersion College/02 MLM/03. Supervised Regression/3.2 Collinearity and Endogeneity/Example Scripts/'

bp <- 
  read.delim(
    paste(
      wrkng.path,
      '3.2.0 bloodpressure.txt', 
      sep = ''
      ), 
    sep = '\t', 
    header = T
    )

head(bp, 5)

cor(bp[, -1])

library(psych)

cor.plot(
  r = cor(
    x = bp[, -1],
    use = 'everything',
    method = 'pearson' # Other: 'kendall' or 'spearman'
    ), 
  numbers = TRUE
  )

pairs(bp[, -1])
# Note that strong correlations between BSA, weight and age.

m4 <- 
  lm(
    BP ~ . , # Kitchen-sink Modeling
    data = bp[, -1]
    )

summary(m4) # Collinearity here is not as obvious as it was in the model data.
#           # Will need to use a more sophisticated technique

# Use Variance Inflation Factor (VIF) to detect multicollinearity.
# install.packages('pbkrtest')
# install.packages('car', dependencies=T) 
# library(car) #has VIF but limited additional functionality
# install.packages('usdm')
library(usdm) #has VIF as well as some clever functions to eliminate correlated
              #variables

vif(bp[, -1]) # Note insanely high VIF for BP, Weight and Age (First column
#             # Patient ID has been removed)

vifcor(bp[, -1]) # Remove first column (Patient ID)

vif(bp[, -c(1,2)]) # Remove first and second (BP) column. VIF story becomes
#                  # less clear

vifcor(bp[, -c(1,2)]) # BSA and Weight have highest correlation so remove one

# VIF > 5 is considered having multicollinearity.

# Remove BSA and refit the model.
m5 <- 
  lm(
    BP ~ . , 
    data = bp[, -c(1, 5)]
    )

cor.plot(
  r = cor(
    x = bp[, -c(1,5)],
    use = 'everything',
    method = 'pearson' # Other: 'kendall' or 'spearman'
    ), 
  numbers = TRUE
  )

summary(m5)

m6 <- 
  lm(
    BP ~ . , 
    data = bp[, -c(1, 4)] # Remove Patient ID and Weight
    )

cor.plot(
  r = cor(
    x = bp[, -c(1,4)],
    use = 'everything',
    method = 'pearson'
    ), # 'kendall' or 'spearman'
  numbers = T
  )

summary(m6)
# Note that pulse becomes significant after refitting.

m4$coefficients
m5$coefficients
m6$coefficients

# Notice sign flip on Intercept and Pulse, diferences in Age, BSA and Stress
# (although Stress is a relatively low impact variable, perhaps because the
# variables were not normalized)

rm(
  list = ls()
  )

# END
