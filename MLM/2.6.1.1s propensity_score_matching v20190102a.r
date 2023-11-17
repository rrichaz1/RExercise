####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

# propensity_score_matching.R
# Yezhou Sun

options(
  scipen = 999 # Great example of where help is...not so helpful. So google it!
  )

# Install and load R package for propensity score matching
# Reference:
# Daniel Ho, Kosuke Imai, Gary King, and Elizabeth Stuart (2007). 
# 'Matching as Nonparametric Preprocessing for Reducing Model Dependence in 
# Parametric Causal Inference'.' Political Analysis 15(3): 199-236.

library(MatchIt)

# Data set from a classic paper: Lalonde, Robert. (1986). 'Evaluating the 
# Econometric Evaluations of Training Programs' American Economic Review 76:
# 604-620.

# It has 614 observations and 10 variables.
# age (in years), 
# educ (years of schooling), 
# black (indicator for blacks) -> one-hot encoding, 
# hispan (indicator for Hispanics) -> one-hot encoding, 
# married (indicator for martial status),
# nodegree (indicator for high school diploma), 
# re74 (real earning in 1974)  
# re75 (real earning in 1975) -> one-hot encoding, 
# re78 (real earning in 1978) -> one-hot encoding,
# treat (indicator for treatment)

# The goal is to see the impact of treatment(training) on earning in 1978.

data(lalonde)

# Description of data
str(lalonde)

# RAW differences in means for treatments and controls
summary(
  lm(
    formula = re78 ~ treat, 
    data = lalonde
    )
  )

t.test(
  lalonde$re78[lalonde$treat == 1], # Subsetting of dataset by column and row
  lalonde$re78[lalonde$treat == 0], # Subsetting of dataset by column and row
  paired = F
  )

# Note that the result from both linear regression and t-test using RAW data 
# show that treatment will DECREASE earning in 1978 by $635. No surprise that
# they are identical decreases as a t-test (as we will learn in Unit 3) is a
# simple form of linear model.

# Estimate propensity scores and create matched data set
matching <- 
  matchit(
    treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, 
    data = lalonde, 
    method = 'nearest', # 'exact', 'full', 'optimal', 'subclass', 'genetic'
    distance = 'logit' # Many options for measuring distance
    )

summary(matching)

# Summary of balance for all data:
#          Means Treated Means Control SD Control  Mean Diff   eQQ Med  eQQ Mean   eQQ Max
# distance        0.5774        0.1822     0.2295     0.3952    0.5176    0.3955    0.5966
# age            25.8162       28.0303    10.7867    -2.2141    1.0000    3.2649   10.0000
# educ           10.3459       10.2354     2.8552     0.1105    1.0000    0.7027    4.0000
# black           0.8432        0.2028     0.4026     0.6404    1.0000    0.6432    1.0000
# hispan          0.0595        0.1422     0.3497    -0.0827    0.0000    0.0811    1.0000
# nodegree        0.7081        0.5967     0.4911     0.1114    0.0000    0.1135    1.0000
# married         0.1892        0.5128     0.5004    -0.3236    0.0000    0.3243    1.0000
# re74         2095.5737     5619.2365  6788.7508 -3523.6628 2425.5720 3620.9240 9216.5000
# re75         1532.0553     2466.4844  3291.9962  -934.4291  981.0968 1060.6582 6795.0100

# Summary of balance for matched data:
#          Means Treated Means Control SD Control Mean Diff  eQQ Med eQQ Mean    eQQ Max
# distance        0.5774        0.3629     0.2533    0.2145   0.1646   0.2146     0.4492
# age            25.8162       25.3027    10.5864    0.5135   3.0000   3.3892     9.0000
# educ           10.3459       10.6054     2.6582   -0.2595   0.0000   0.4541     3.0000
# black           0.8432        0.4703     0.5005    0.3730   0.0000   0.3730     1.0000
# hispan          0.0595        0.2162     0.4128   -0.1568   0.0000   0.1568     1.0000
# nodegree        0.7081        0.6378     0.4819    0.0703   0.0000   0.0703     1.0000
# married         0.1892        0.2108     0.4090   -0.0216   0.0000   0.0216     1.0000
# re74         2095.5737     2342.1076  4238.9757 -246.5339 131.2709 545.1182 13121.7500
# re75         1532.0553     1614.7451  2632.3533  -82.6898 152.1774 349.5371 11365.7100

# Percent Balance Improvement:
#          Mean Diff.   eQQ Med eQQ Mean  eQQ Max
# distance    45.7140   68.1921  45.7536  24.7011
# age         76.8070 -200.0000  -3.8079  10.0000
# educ      -134.7737  100.0000  35.3846  25.0000
# black       41.7636  100.0000  42.0168   0.0000
# hispan     -89.4761    0.0000 -93.3333   0.0000
# nodegree    36.9046    0.0000  38.0952   0.0000
# married     93.3191    0.0000  93.3333   0.0000
# re74        93.0035   94.5880  84.9453 -42.3724
# re75        91.1508   84.4891  67.0453 -67.2655

# Sample sizes:
#           Control Treated
# All           429     185
# Matched       185     185
# Unmatched     244       0
# Discarded       0       0

matched.data <- 
  match.data(
    object = matching, 
    distance ='pscore'
    )

str(matched.data)

# MATCHED differences in means for treatments and controls

summary(
  lm(
    formula = re78 ~ treat, 
    data = matched.data
    )
  )

t.test(
  matched.data$re78[matched.data$treat == 1], 
  matched.data$re78[matched.data$treat == 0], 
  paired = F
  )

# Note that the result from both linear regression and t test using MATCHED 
# data show that treatment will INCREASE earning in 1978 by $908 (which believe 
# it or not was a lot of money in 1978 when average household incomes were
# close to $20k / year.

wrkng.path <- 
  'C:/Users/mparadis/Desktop/DSU/Immersion College/02 MLM/02. Statistics/2.6 Investigational Design/'

# Note you have to change backslashes (\) to forward slashes (/) Note setwd()
# is a more elegant approach but most UHG/UHC/Optum machines are locked down
# and you can't change the working directory or add folders to the default  
# working directory.

### png() plotting function is commented out due to varying permissions issues
### across participant's machines

png(
 file = paste(
   wrkng.path,
   'obs_pscore.png',
   sep = ''
   ), 
 width = 800, 
 height = 800
 )

plot(
 matching, 
 type = 'jitter'
 ) #NOTE: HIT THE ESC KEY TO EXIT INTERACTIVE GRAPH

dev.off()

png(
 file = paste(
   wrkng.path,
   'pscore_distribution.png',
   sep = ''
   ), 
 width = 800, 
 height = 800
 )

plot(
  matching, 
  type = 'hist'
  )

dev.off()

rm(
  list = ls()
  ) # Clears global environment of all visible objects (some hidden objects
    # may remain)

# END
