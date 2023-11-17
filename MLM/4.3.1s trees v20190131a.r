####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

# trees.R
# Chris Hane

require(data.table)
require(ggplot2)
require(rpart)
require(rpart.plot)

wrkng.path <- 'C:/Users/mparadis/Desktop/DSU/Immersion College/02 MLM/04. Supervised Classification/4.1 Distance-Based Classification/'

load(
  paste(
    wrkng.path,
    '4.0d train_test.RData',
    sep = ''
  )
) 

str(train)
str(test)
str(vars2Use)

set.seed(43100) # Note that I changed this from 123, so figures may differ from
                # lecture

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

rp1.train <- 
  rpart(
    fmla, 
    train, 
    method = 'class', # class => classification
    control = rpart.control(
      minsplit = 20,  # minimum # obserations in a node
      cp = 0.01 # complexity: minimum GINI score improvement
      )
    )

rp1.train
summary(rp1.train)
plot(rp1.train)
text(rp1.train)

rpart.plot(rp1.train)

#rpart prediction in train dataset
yhat.rp1.train <- 
  predict(
    rp1.train, # rpart model object 
    newdata = train, # data to apply rpart model to
    type = 'class' # type of predicted value returned:
                   # class => a factor of classifications based on responses
    )

# Confusion Matrix
( # Extra parens are a little trick in R
  twoByTwo.rp1.train <- 
    table(
      train$y, 
      yhat.rp1.train
      )
  )

# Accuracy ((TP + TN) / Total)
sum(diag(twoByTwo.rp1.train)) / sum(twoByTwo.rp1.train) 

# rpart prediction in test dataset
yhat.rp1.test <- 
  predict(
    rp1.train, 
    newdata = test, 
    type = 'class'
    )

# Confusion Matrix
( # Remember this parens trick implicitly prints the object
  twoByTwo.rp1.test <- 
    table(
      y = test$y, 
      predicted = yhat.rp1.test
      )
  )

# Accuracy ((TP + TN) / Total)
sum(diag(twoByTwo.rp1.test)) / sum(twoByTwo.rp1.test) 

####
require(randomForest)

system.time( # Captures the length of time that the enclosed fxn takes to run
  rf1.train <- 
    randomForest(
      train[, .SD, .SDcols = vars2Use], # data.table notation
      factor(train$y),
      ntree = 100, # Hyperparameter: Nbr of trees in forest
      maxnodes = 16, # Hyperparameter: Max nbr of terminal nodes (leaves or 
                     # depth)
      nodesize = 20, # Hyperparameter: minimum size of terminal nodes
      importance = TRUE # Collect the stats for variable importance
      )
  )

rf1.train
summary(rf1.train)
varImpPlot(rf1.train) # N.B. Accuracy and GINI disagree on mammogram_All_Rate
                      # and DM_All_Rate

###############################

## CURRENTLY NOT WORKING START ##

require(inTrees)
X <- 
  train[, .SD, .SDcols = vars2Use]

treeList <- 
  RF2List(rf1.train)  # change format

ruleExec <- 
  extractRules(treeList, X) # transform to R-executable rules

ruleExec <- 
  unique(ruleExec) # these strings are filters in data.frame syntax
head(ruleExec)

ruleMetric <- 
  getRuleMetric(
    ruleExec,
    X,
    factor(train$y)
    ) # measure rules
head(ruleMetric)

learner <- 
  buildLearner(
    ruleMetric,
    X,
    factor(train$y)
    )

pred.inTree.train <- 
  applyLearner(
    learner,
    X
    )

# Confusion Matrix
(
  twoByTwo.inTree.train <-
    table(
      pred.inTree.train, 
      truth = factor(train$y)
      )
  )

# 1 - Accuracy = Error Rate
1 - sum(diag(twoByTwo.inTree.train)) / sum(twoByTwo.inTree.train)
print(rf1.train)

shortRules <- 
  presentRules(
    learner,
    colnames(X)
    )

pred.inTree.test <- 
  applyLearner(
    learner,
    test
    )

# Confusion Matrix
(
  twoByTwo.inTree.test <-
    table(
      pred.inTree.test, 
      truth = factor(test$y)
      )
  )

# 1 - Accuracy = ?
1-sum(diag(twoByTwo.inTree.test)) / sum(twoByTwo.inTree.test) 

## CURRENTLY NOT WORKING END ##

#################################
require(Rborist)

system.time(
  rb1.train <- 
    Rborist(
      train[, .SD, .SDcols = vars2Use], 
      factor(train$y),
      nTree = 100, 
      maxLeaf = 16, 
      minNode=20
      )
  )
## no print, no summary
rb1.train$validation$oobError
rb1.train$validation$misprediction
rb1.train$validation$confusion

## randomForest can continue to grow the forest, Rborist does not
system.time(
  rb1.train <- 
    Rborist(
      train[, .SD, .SDcols = vars2Use], 
      factor(train$y),
      nTree = 500, 
      maxLeaf = 32, 
      minNode = 20
      )
  )

## no print, no summary
rb1.train$validation$oobError
rb1.train$validation$misprediction
rb1.train$validation$confusion

############
require(xgboost)

dtrain <- 
  xgb.DMatrix( # xgb specific data structure
    data = as.matrix(train[, .SD, .SDcols = vars2Use]),
    label = train$y
    )

set.seed(43100) # all random forests start with a random set of features and
                # trees if you want to build the same model each time, then you
                # must set.seed

system.time(
  xgb5.train <- 
    xgb.cv(
      data = dtrain, 
      nrounds = 5, 
      nthread = 3,
      metrics = list("auc", "error"),
      nfold = 5,
      max_depth = 8, 
      eta = 1,  
      objective = "binary:logistic",
      prediction = TRUE
      )
  )

system.time(
  xgb5.train <- 
    xgboost(
      data = dtrain, 
      nrounds = 5, 
      nthread = 3,
      metrics = list("rmse","auc", "error"),
      max_depth = 8, 
      eta = 1,  
      objective = "binary:logistic",
      prediction = TRUE
      )
  )

xgb.importance(
  feature_names = vars2Use,
  model = xgb5.train
  )

yhat.xgb.train <- 
  predict(
    xgb5.train, 
    dtrain
    )

(
  twoByTwo.xgb.train <-
    table(
      yhat.xgb.train > 0.5, 
      truth = factor(train$y)
      )
  )

# 1 - Accuracy = ?
1 - sum(
  diag(
    twoByTwo.xgb.train
    )
  ) / 
  sum(twoByTwo.xgb.train) 

yhat.xgb.test <- 
  predict(
    xgb5.train, 
    newdata = xgb.DMatrix(
      data = as.matrix(
        test[, .SD, .SDcols = vars2Use]
        )
      )
    )

# Confusion Matrix
(
  twoByTwo.xgb.test <-
    table(
      yhat.xgb.test > 0.5, 
      truth = factor(test$y)
      )
  )

1 - sum(
  diag(
    twoByTwo.xgb.test
    )
  ) / 
  sum(twoByTwo.xgb.test)  
# why is this 12.7% and not 15%  CHECK the test$y prevalence!  Its not 15%

#########
require(Boruta)
set.seed(43101)

# Boruta will take some time to run because it is performing multiple RFs, each 
# of which is building multiple trees
system.time(
  bor1.train <- 
    Boruta(
      x = train[, .SD, .SDcols = vars2Use], 
      y = factor(train$y)
      )
  )

plot(bor1.train)

rm(
  list = ls()
  ) # Clears global environment of all visible objects (some hidden objects
#   # may remain)


# End
