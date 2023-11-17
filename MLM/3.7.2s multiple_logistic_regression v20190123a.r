####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

# multiple_logistic_regression
# Yezhou Sun

#install.packages('effects')
library(effects)

options(
  scipen = 999
  )

# This script will use a subset of diabetes data for Immersion program to test 
# the impact of a patient's hospital stay (characterized by days of stay, 
# number of lab tests, procedures and medication administered during
# encounter) on 30-day readmission.

wrkng.path <- 
  'C:/Users/mparadis/Desktop/DSU/Immersion College/02 MLM/03. Supervised Regression/3.7 GLM Regression/' 
# note you have to change backslashes (\) to forward slashes (/) or 
# double-backslashes (\\)

d <- 
  read.csv(
    paste(
      wrkng.path,
      '3.7.2.0 diabetes_subset.csv',
      sep = ''
    )
  )

str(d)

summary(
  d[, c(
    'race', 
    'gender', 
    'age', 
    'time_in_hospital', 
    'num_lab_procedures', 
    'num_procedures', 
    'num_medications', 
    'readmitted'
    )]
  )

m <- 
  glm( # function to fit Generalized Linear Models
    readmitted ~ race + gender + age + time_in_hospital + num_lab_procedures + 
      num_procedures + num_medications,
		family = binomial(link = 'logit'), 
		data = d
		)

summary(m)

coef(summary(m))
# Results indicate that number of medication administered during encounter 
# significantly affects 30-day readmission and compared to female, male are less 
# likely to have 30-day readmission.

png(
  file = paste(
    wrkng.path,
    'all.logistic.effects.png',
    sep = ''
  ), 
  width = 800, 
  height = 800
)
plot(allEffects(m))
dev.off()

plot(allEffects(m))

rm(
  list = ls()
  ) # Clears global environment of all visible objects (some hidden
#   # objects may remain)

# END

