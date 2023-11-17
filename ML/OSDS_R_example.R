
# Exploring Data and Creating Classifier
# Created by Yezhou Sun
# Version: 0.1
#
# In this exercise, we will use a breast cancer data set from UCI machine learning repository.
# This data set has 699 observations, 10 attributes and 1 outcome variable with 2 classes.
# Data has been normalized and packaged into R library mlbench.

#--------------------------------------------------------------------------------------------------
# Install libraries. This section of code need to be run only once. 
# All these libraries will be installed into local R library repository permanently.

# library for model performance evaluation and visualization
install.packages("ROCR")

# library of useful functions for data processing and model building
install.packages("caret")

# library for Random Forest algorithm
install.packages("randomForest")

# library for SVM algorithm
install.packages("e1071")

# library including many data sets
install.packages("mlbench")

# library for exploratory data analysis
install.packages("devtools")
devtools::install_github("ujjwalkarn/xda")
#--------------------------------------------------------------------------------------------------

# Load libraries into current working enviroment.
library(devtools)
library(caret)
library(ROCR)
library(randomForest)
library(e1071)
library(mlbench)
library(xda)

# Check configuration of current working enviroment and loaded libraries.
# When asking for help, always run this command first to get current setup in your working enviroment.
sessionInfo()

# Load data from mlbench.
data(BreastCancer)

# Check objects exist in your working enviroment.
# You should see BreastCancer.
objects()

# Get dimension of data set.
# [1] 699  11  699 rows and 11 columns
dim(BreastCancer)

# Get structure and metadata.
# 'data.frame':   699 obs. of  11 variables:
# $ Id             : chr  "1000025" "1002945" "1015425" "1016277" ...
# $ Cl.thickness   : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 5 5 3 6 4 8 1 2 2 4 ...
# $ Cell.size      : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 4 1 8 1 10 1 1 1 2 ...
# $ Cell.shape     : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 4 1 8 1 10 1 2 1 1 ...
# $ Marg.adhesion  : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 5 1 1 3 8 1 1 1 1 ...
# $ Epith.c.size   : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 2 7 2 3 2 7 2 2 2 2 ...
# $ Bare.nuclei    : Factor w/ 10 levels "1","2","3","4",..: 1 10 2 4 1 10 10 1 1 1 ...
# $ Bl.cromatin    : Factor w/ 10 levels "1","2","3","4",..: 3 3 3 3 3 9 3 3 1 2 ...
# $ Normal.nucleoli: Factor w/ 10 levels "1","2","3","4",..: 1 2 1 7 1 7 1 1 1 1 ...
# $ Mitoses        : Factor w/ 9 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 5 1 ...
# $ Class          : Factor w/ 2 levels "benign","malignant": 1 1 1 1 1 2 1 1 1 1 ...
str(BreastCancer)

# Simple summary of data
summary(BreastCancer)

# Some processing specifically for this data set.
d <- apply(BreastCancer[, 2:6], 2, as.numeric)
d <- data.frame(Id=as.character(BreastCancer$Id), d, BreastCancer[, 7:11])

# Profile numeric variables.
#                n mean   sd max min range nunique nzeros iqr lowerbound upperbound noutlier kurtosis skewness mode miss miss% 1% 5% 25% 50% 75% 95% 99%
# Cl.thickness  699 4.42 2.82  10   1     9      10      0   4       -4.0       12.0        0  -0.6346     0.59    1    0     0  1  1   2   4   6  10  10
# Cell.size     699 3.13 3.05  10   1     9      10      0   4       -5.0       11.0        0   0.0807     1.23    1    0     0  1  1   1   1   5  10  10
# Cell.shape    699 3.21 2.97  10   1     9      10      0   4       -5.0       11.0        0  -0.0102     1.16    1    0     0  1  1   1   1   5  10  10
# Marg.adhesion 699 2.81 2.86  10   1     9      10      0   3       -3.5        8.5       60   0.9610     1.52    1    0     0  1  1   1   1   4  10  10
# Epith.c.size  699 3.22 2.21  10   1     9      10      0   2       -1.0        7.0       54   2.1303     1.70    2    0     0  1  1   2   2   4   8  10
numSummary(d)

# Profile non-numeric and factor variables. 
# Notice that variable Bare.nuclei has missing values and some Ids have multiple records.
#                  n miss miss% unique                                      top5levels:count
# Id              699    0  0.00    645 1182404:6, 1276091:5, 1198641:3, 1017023:2, 1033078:2
# Bare.nuclei     683   16  2.29     11                       1:402, 10:132, 2:30, 5:30, 3:28
# Bl.cromatin     699    0  0.00     10                       2:166, 3:165, 1:152, 7:73, 4:40
# Normal.nucleoli 699    0  0.00     10                        1:443, 10:61, 3:44, 2:36, 8:24
# Mitoses         699    0  0.00      9                        1:579, 2:35, 3:33, 10:14, 4:12
# Class           699    0  0.00      2                             benign:458, malignant:241
charSummary(d)

# Remove rows with missing values.
# Sometimes missing values can be filled by imputation.

#Get rows with NA  
missing <- is.na(d)
which(rowSums(missing) == 1)
d <- d[rowSums(is.na(d))==0, ]
dim(d)

# Find Ids with multiple records and remove them.
# Other methods, like average over duplicates, maybe proper as well.
duplicated.ids <- tapply(d$Id, factor(d$Id), length)
duplicated.ids <- names(duplicated.ids[duplicated.ids > 1])
d <- d[!(d$Id %in% duplicated.ids), ]
dim(d)

# Convert Class variable to numeric factor and create matrix for modeling.
d$Class <- as.character(d$Class)
d$Class[d$Class=="benign"] <- 0
d$Class[d$Class=="malignant"] <- 1
d$Class <- as.factor(d$Class)
d <- d[, 2:ncol(d)]

# Split data 4 to 1 for training and test based on the distribution of outcome variable.
y <- d[, c("Class")]
y
in.train <- createDataPartition(y, p=0.8, list=F)
train.d <- d[in.train, ]
test.d <- d[-in.train, ]
dim(train.d)
dim(test.d)

#-------------------Modeling--------------------------------------------------------------------
# Please note that all models in this section use basic configurations.
# These configurations may not be optimal and may need tuning in practice.

EvaluateModel <- function(predicted, test.label, method.name) {	
	
	prediction.obj <- prediction(predicted, test.label)
	perf <- performance(prediction.obj, "tpr", "fpr")
	auc <- performance(prediction.obj, "auc") 
	
	# Generate ROC curve.
	plot(perf, colorize=F, col="blue", lwd=3, cex.main=1.5, main=method.name)
	lines(c(0,1), c(0,1), col="gray", lwd=2)
	legend("topleft", legend=paste("AUC=", round(auc@y.values[[1]], digits=4), sep=""), bty="n", cex=2)
	
	# Generate confusion matrix and other model statistics.
	pred <- factor(ifelse(predicted > 0.5, 1, 0))
	confusionMatrix(pred, test.label, positive="1")	
}

# Logistic regression model
# Train model with training data.
m <- glm(Class ~ ., family=binomial(link="logit"), data=train.d)

# Get model details and summary.
m
summary(m)

# Evaluate model performance with test data.
predicted <- predict(m, newdata=test.d, type="response")
EvaluateModel(predicted, test.d$Class, "Logistic Regression")

# Random Forest model
m <- randomForest(Class ~ ., data=train.d)
m
summary(m)
predicted <- predict(m, newdata=test.d, type="prob")
predicted <- predicted[, 2]
EvaluateModel(predicted, test.d$Class, "Random Forest")

# SVM model
# Find best parameters through grid search.
# This will take few minutes to finish.
svm.tune <- tune.svm(Class ~ ., data=train.d, type="C-classification", kernel="radial", scale=T, cost=10^(-5:5), gamma=10^(-10:10))
svm.tune
m <- svm(Class ~ ., data=train.d, type="C-classification", kernel="radial", scale=T, probability=T, cost=svm.tune$best.parameters$cost, gamma=svm.tune$best.parameters$gamma)
m
summary(m)
predicted <- predict(m, newdata=test.d, probability=T)
predicted <- attr(predicted, "probabilities")[, 2]
EvaluateModel(predicted, test.d$Class, "Support Vector Machine")

# End

