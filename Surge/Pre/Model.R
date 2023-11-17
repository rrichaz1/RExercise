library("devtools")
install.packages("car")
install.packages("ROCR")#For calculating predicted values
install.packages("e1071")
install.packages("MLmetrics") 
install.packages("caret")

install.packages("tidyverse")
devtools::install_github("ujjwalkarn/xda")

library("tidyverse")
library("ggplot2")
library("xda")
library("ROCR")
library("MLmetrics") 
library("caret")

setwd("/Users/rsing198/Documents/testcode/R_Exercise/Surge/Pre")

trainDS <- read.csv("trainingData.csv",header=TRUE)
str(trainDS)
summary(trainDS)

numSummary(trainDS)



trainDS <- trainDS %>% select(-c("claims_daysaway", "testindex"))

summary(trainDS)

missing <- is.na(trainDS)
which(rowSums(missing) == 1)
trainDS[is.na(trainDS)] <- 1
numSummary(trainDS)
trainDSNoFactor <-trainDS

#################################################################################
#                     Plots
#####################################################################################
factorCols <- c('outcome', 'fqhc','tier','pcp_lookback','family_assignment','kid',
                'is_ped','same_gender','same_language','same_address')
trainDS[factorCols] <- lapply(trainDS[factorCols], factor)
summary(trainDS)

table(trainDS$outcome,trainDS$fqhc)

ggplot(trainDS,aes(x=tier, fill=outcome)) +geom_bar()
ggplot(trainDS,aes(x=fqhc, fill=outcome)) +geom_bar(position="dodge")
ggplot(trainDS,aes(x=pcp_lookback, fill=outcome)) +geom_bar(position="dodge")
ggplot(trainDS,aes(x=family_assignment, fill=outcome)) +geom_bar(position="dodge")
ggplot(trainDS,aes(x=kid, fill=outcome)) +geom_bar(position="dodge")
ggplot(trainDS,aes(x=is_ped, fill=outcome)) +geom_bar(position="dodge")
ggplot(trainDS,aes(x=same_gender, fill=outcome)) +geom_bar(position="dodge")
ggplot(trainDS,aes(x=same_language, fill=outcome)) +geom_bar(position="dodge")
ggplot(trainDS,aes(x=same_address, fill=outcome)) +geom_bar(position="dodge")


colNames<- colnames(trainDS)
colNames <- colNames[colNames != 'outcome']



  bivariate(trainDS, "outcome", "distance")


ggplot(trainDS, aes(x=distance,color=outcome)) + 
  geom_histogram() +labs(title = "Distance outcome density")

###########################################################################################
##OVERSAMPLING
trainData <- trainDSNoFactor %>% drop_na %>% group_by(outcome) %>% sample_n(6800)
summary(trainData)
str(trainData)
str(trainDSNoFactor)


tab_sameadd <- table(trainData$outcome, trainData$same_address)
tab_sameadd
prop.table(tab_sameadd)
summary(prop.table(tab_sameadd))
chisq.test(tab_sameadd,p=0.05)
cramersV(tab_sameadd)


tab_samegend <- table(trainData$outcome, trainData$same_gender)
tab_samegend
prop.table(tab_samegend)
summary(prop.table(tab_samegend))
chisq <- chisq.test(tab_samegend,p=0.05)
chisq$observed
chisq$expected
cramersV(tab_samegend)


##############################################################
# Create the training and test datasets
##############################################################
set.seed(100)
# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(trainData$outcome, p=0.7, list=FALSE)
# Step 2: Create the training dataset
trainData <- trainData[trainRowNumbers,]
# Step 3: Create the test dataset
testData <- trainData[-trainRowNumbers,]

#Randomize
train.data.random <- trainData[sample(nrow(trainData)),]
test.data.random <- testData[sample(nrow(testData)),]

# Create the training and test datasets for RFE prediction

set.seed(100)
trainDSNoFactorNoNA <- trainDSNoFactor %>% drop_na 
# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(trainDSNoFactorNoNA$outcome, p=0.7, list=FALSE)
# Step 2: Create the training dataset
#trainData <- appeal_subset_df[trainRowNumbers,]
# Step 3: Create the test dataset
testDataOrig <- trainDSNoFactorNoNA[-trainRowNumbers,]
test.dataOrig.random <- trainDSNoFactorNoNA[sample(nrow(testDataOrig)),]


str(train.data.random)
str(test.data.random)
str(test.dataOrig.random)
########################################################
#- borrowed from OSDS example by Yezhou
#########################################################

EvaluateModel <- function(predicted, test.label, method.name, threshold) {	
  if (is.factor(predicted))
  {
    predicted <- as.numeric(predicted) -1
  }
  
  prediction.obj <- prediction(predicted, test.label)
  perf <- performance(prediction.obj, "tpr", "fpr")
  perf
  auc <- performance(prediction.obj, "auc") 
  
  # Generate ROC curve.
  plot(perf, colorize=F, col="blue", lwd=3, cex.main=1.5, main=method.name)
  lines(c(0,1), c(0,1), col="gray", lwd=2)
  legend("topleft", legend=paste("AUC=", round(auc@y.values[[1]], digits=4), sep=""), bty="n", cex=2)
  
  # Generate confusion matrix and other model statistics.
  pred <- factor(ifelse(predicted >= threshold, 1, 0))
  
  confusionMatrix(pred, test.label, positive="1")	
  
}

#################################################################
#Evaluate_F1_Score
##################################################

Evaluate_F1_Score<- function(predicted, test.label, method.name, threshold) {	
  if (is.factor(predicted))
  {
    predicted <- as.numeric(predicted) -1
  }
  pred <- factor(ifelse(predicted >= threshold, 1, 0))
  F1_Score(y_pred = pred, test.label, positive = "1")
}



############
#Logistic Regression
###############
train.data <- train.data.random
model.logit <- glm(outcome ~ . , data= train.data, 
                  family=binomial)
model.logit
#Predict on training data
fitted <- predict(model.logit,train.data)
Evaluate_F1_Score(fitted, as.factor(train.data$outcome), "Logistic Regression Train", 0.5)
EvaluateModel(fitted,  as.factor(train.data$outcome), "Logistic Regression Train", 0.5)

#Predict on test data
test.data <- test.data.random
fitted <- predict(model.logit,test.data)
Evaluate_F1_Score(fitted, as.factor(test.data$outcome), "Logistic Regression Test", 0.5)
EvaluateModel(fitted,  as.factor(test.data$outcome), "Logistic Regression Test", 0.5)


################################################################################
# RFE
################################################################################

set.seed(100)

fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10,
  savePredictions = TRUE
#  classProbs = TRUE
)


# Train the model using random forest
model.rf = train(outcome ~ ., data=train.data, method='rf', tuneLength=5, trControl = fitControl,importance=T,preProcess = c("center", "scale"))
model.rf
#train.data$success = as.factor(train.data$success)
#test.data$success = factor(test.data$success)
varimp_rf<- varImp(model.rf)
plot(varimp_rf, main="Variable Importance with Random Forest")

#Predict on training data
fitted <- predict(model.rf,train.data)
#fitted

Evaluate_F1_Score(fitted, as.factor(train.data$outcome), "RFE Regression Train", 0.5)
EvaluateModel(fitted,  as.factor(train.data$outcome), "RFE Regression Train", 0.5)

#Predict on test data
test.data <- test.data.random
fitted <- predict(model.rf,test.data)
Evaluate_F1_Score(fitted, as.factor(test.data$outcome), "RFE Regression Test", 0.5)
EvaluateModel(fitted,  as.factor(test.data$outcome), "RFE Regression Test", 0.5)

test.data <-test.dataOrig.random
fitted <- predict(model.rf,test.data)
Evaluate_F1_Score(fitted, as.factor(test.data$outcome), "RFE Regression Test", 0.5)
EvaluateModel(fitted,  as.factor(test.data$outcome), "RFE Regression Test", 0.5)

str(test.data)
summary(fitted)
table(fitted)
pred <- factor(ifelse(fitted >= 0.5, 1, 0))
pred
table(pred)

####################################################################################
#  Scoring data
########################################################################################


scoreDS <- read.csv("scoringData.csv",header=TRUE)
str(scoreDS)
summary(scoreDS)

scoreDS <- scoreDS %>% select(-c("claims_daysaway"))
str(scoreDS)
scoreDS[is.na(scoreDS)] <- 1

scoreDSNoNA <- scoreDS
str(scoreDSNoNA)
columns <- c( 'fqhc','tier','pcp_lookback','family_assignment','kid',
             'is_ped','same_gender','same_language','same_address','distance','visit_count')
scoreDSNoIndex <- scoreDSNoNA %>% select(columns)
str(scoreDSNoIndex)

scores <-  predict(model.rf,scoreDSNoIndex)
pred <- ifelse(scores >= 0.5, 1, 0)
pred
table(pred)
class(pred)

finalDS <- as.data.frame(scoreDSNoNA[,1])
predDS <- as.data.frame(pred)

submitDS <- data.frame(scoreDSNoNA[,1], pred)
str(submitDS)

write.csv(submitDS,file="Submission_withR.csv",row.names=FALSE, na="")

