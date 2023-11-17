####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

#Based on scripts by Chris Hane

require(data.table)
require(ggplot2)
require(e1071)

wrkng.path <- 'C:/Users/mparadis/Desktop/DSU/Immersion College/02 MLM/04. Supervised Classification/4.1 Distance-Based Classification/'

load(
  paste(
    wrkng.path,
    '4.0d train_test.RData',
    sep = ''
    )
  )

#-------------------Modeling-------------------------------------------------
# Please note that all models in this section use basic configurations.
# These configurations may not be optimal and may need tuning in practice.

set.seed(44100) # Note changed from 123 so may differ from lecture

fmla <- 
  as.formula(
    paste0(
      'factor(y) ~ ', 
      paste(
        vars2Use, 
        collapse = '+'
        )
      )
    )

# linear kernel and this will take a few minutes to run, or an hour...
# what is happening inside tune?
system.time(
 svm.tune.lin <- 
   tune.svm(
     fmla, 
     data = train, 
     type = 'C-classification',
     kernel = 'linear', 
     scale = FALSE, 
     cost = 10^(-6:-2),
     probability = TRUE,
     tunecontrol = tune.control(
       sampling = 'cross', 
       cross = 5, 
       best.model = TRUE
       ),
     cache = 128
     )
 )

# you may try more values of cost, but it can be very time consuming.

svm.tune.lin

svm.tune.lin$performances

# we run tune and not just best... to look at the performance and if the cost 
# profile is non-linear
m <- 
  svm.tune.lin$best.model

m

summary(m)

#prepare to plot 2-D
colM <- 
  colMeans(
    train[
      , 
      .SD, 
      .SDcols = vars2Use
      ]
    )

plotFmla  <- 
  as.formula(
    paste(
      vars2Use[1], 
      vars2Use[2], 
      sep = '~'
      )
    )

plot(
  m, 
  train, 
  plotFmla
  )

# the model completely failed to predict any TRUE events

set.seed(44101) # Note changed from 123 so may differ from lecture

system.time(
 svm.tune.rbf  <- # Note rbf takes longer to run than lin
   tune.svm(
     fmla, 
     data = train, 
     type = 'C-classification',
     kernel = 'radial', 
     scale = FALSE, 
     cost = 10^(-5:0), 
     gamma = 10^(-10:10),
     probability = TRUE,
     tunecontrol = tune.control(
       sampling = 'cross', 
       cross = 5, 
       best.model = TRUE
       ),
     cache=256
     )
 )

# Run-time: 11 minutes

svm.tune.rbf 

svm.tune.rbf$performance

# narrow rage of parameters

system.time(
 svm.tune.rbf.restrict  <- 
   tune.svm(
     fmla, 
     data = train, 
     type = 'C-classification',
     kernel = 'radial', 
     scale = FALSE, 
     cost = 10^seq(
       -1,
       2, 
       by = 0.5
       ), 
     gamma = 10^seq(
       -2,
       1, 
       by = 0.5
       ),
     probability = TRUE,
     tunecontrol = tune.control(
       sampling = 'cross', 
       cross = 5, 
       best.model = TRUE
       ),
     cache = 256
     )
 )

svm.tune.rbf.restrict 

svm.tune.rbf.restrict$performance

# we run tune and not just best... to look at the performance 
m.restrict <- 
  svm.tune.rbf.restrict$best.model

m.restrict

summary(m.restrict)

plotFmla  <- 
  as.formula(
    paste(
      vars2Use[2], 
      vars2Use[6], 
      sep = '~'
      )
    )

plot(
  m.restrict, 
  train, 
  plotFmla, 
  dataSymbol = '', 
  svSymbol = ''
  )

########################################################
##mpoly # BE CAREFUL THIS CODE TAKES A LONG TIME TO RUN
########################################################
set.seed(44102) # Note changed from 123 so may differ from lecture

### THIS TAKES ~2h 40m to run!

system.time(
  svm.tune.poly3  <- 
    tune.svm(
      fmla, 
      data = train, 
      type = 'C-classification',
      kernel = 'polynomial', 
      scale = FALSE, 
      degree = 3,
      gamma = 10^seq(
        -3,
        2, 
        by = 1
        ),
      probability = TRUE,
      tunecontrol = tune.control(
        sampling = 'cross', 
        cross = 5, 
        best.model = TRUE
        ),
      cache = 256
      )
  )

svm.tune.poly3

svm.tune.poly3$performance

# we run tune and not just best... to look at the performance 
m.poly <- 
  svm.tune.poly3$best.model

m.poly

summary(m.poly)

plotFmla  <- 
  as.formula(
    paste(
      vars2Use[2], 
      vars2Use[6], 
      sep = '~'
      )
    )

plot(
  m.poly, 
  train, 
  plotFmla, 
  dataSymbol = '', 
  svSymbol = ''
  )

########################################################

### Change to regression

fmla <- 
  as.formula(
    paste0(
      'Hospital_asr ~ ', 
      paste(
        vars2Use, 
        collapse = '+'
        )
      )
    )

set.seed(44103) # Note changed from 123 so may differ from lecture

## set SCALE = TRUE because we have a new outcome that is not scaled.  
## Use nu to more easily choose parameters.  This is incomplete parameter exploration!
system.time(
  svm.tune.reg  <- 
    tune.svm(
      fmla, 
      data = train, 
      type = 'nu-regression',
      kernel = 'radial', 
      scale = TRUE, 
      nu = seq(
        0.05,
        0.5, 
        0.05
        ), #cost=1,gamma=1,epsilon = 1,
      probability = TRUE,
      tunecontrol = tune.control(
        sampling = 'cross', 
        cross = 5, 
        best.model = TRUE
        ),
      cache = 256
      )
  )

svm.tune.reg

svm.tune.reg$performance

m.reg <- 
  svm.tune.reg$best.model

yhat  <- 
  predict(
    m.reg, 
    train
    )

ymean <- 
  mean(train$Hospital_asr)

res <- 
  train$Hospital_asr-yhat

ss <- 
  train$Hospital_asr-ymean

reg <- 
  yhat - ymean

sum(reg^2) / sum(ss^2)

1 - sum(res^2) / sum(ss^2)

plot(res)

plot(train$Hospital_asr, yhat)

rm(
  list = ls()
  ) # Clears global environment of all visible objects (some hidden objects
#   # may remain)

# END