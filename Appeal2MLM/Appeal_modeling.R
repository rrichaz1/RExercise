install.packages("tidyverse")#For data manipulation
install.packages("caret")#For running ML models
install.packages("earth")  # For running MARS
install.packages("ROCR")#For calculating predicted values
#install.packages("e1071")
install.packages("MLmetrics")   # f1_score for logistic
install.packages("rpart")       # classification trees
install.packages("rpart.plot") # plot classification trees



library("tidyverse")
library("caret")
library("ROCR")
library("earth")
#library("e1071")
library("MLmetrics")

options(
  scipen = 999, digits = 3
) # Turns scientific notation off


setwd("/Users/rsing198/Documents/testcode/R_Exercise/Appeal2MLM")

#Read the dataset


appeal_subset_df <- read.csv("dataset/Appeal_final_data.csv",header=TRUE)
summary(appeal_subset_df)
str(appeal_subset_df)

###########################################################
# Remove features that showed high correlation


appeal_subset_df <- appeal_subset_df %>%
  select(-c("covchg","totadj","totpay", "lpadj"))
str(appeal_subset_df)


##############################################################
# Create the training and test datasets

set.seed(100)
# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(appeal_subset_df$success, p=0.7, list=FALSE)
# Step 2: Create the training dataset
trainData <- appeal_subset_df[trainRowNumbers,]
# Step 3: Create the test dataset
testData <- appeal_subset_df[-trainRowNumbers,]

#Randomize
train.data.random <- sample_frac(trainData, 1L)
test.data.random <- sample_frac(testData, 1L)



str(train.data.random)
str(test.data.random)

#################################################
# Normalize the data
################################################


preProcess_range_model <- preProcess(train.data.random, method='range')
train.data.scale <- predict(preProcess_range_model, newdata = train.data.random)

str(train.data.scale)
# Store X and Y for later use.
independent.train.var = train.data.scale[, 2:10]
dependent.train.var = train.data.scale$success

#Repeat for test
preProcess_range_model <- preProcess(test.data.random, method='range')
test.data.scale <-  predict(preProcess_range_model, newdata = test.data.random)
str(test.data.scale)
independent.test.var = test.data.scale[, 2:10]
dependent.test.var = test.data.scale$success
#####################################################
# do one hot encoding 
#####################################################

dummies_model <- dummyVars(success ~ ., data=train.data.scale)
train.data.encoded<- data.frame(predict(dummies_model, newdata = train.data.scale))

str(train.data.encoded)  # does not include the result variable "success"

#########For test data

dummies_model_test <- dummyVars(success ~ ., data=test.data.scale)
test.data.encoded<- data.frame(predict(dummies_model_test, newdata = test.data.scale))

str(test.data.encoded)  # does not include the result variable "success"





#summary(train.data.scale)

str(train.data.scale)
str(test.data.scale)

###########################################################
###Modeling
################################################################
####### classification models ################


########################################################
#Function to evaluate the models - borrowed from OSDS example by Yezhou
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



# ********************MODELING*******************************

############################################################
# GLM 
############################################################

#Set the train and test data 
train.data <- train.data.random
test.data <- test.data.random
train.data$success = as.factor(train.data$success)
test.data$success = factor(test.data$success)


# Train the model using glm and predict on the training data itself.
model.logit <- glm(success ~ . , data= train.data, 
                  family=binomial)
model.logit
#Predict on training data
fitted <- predict(model.logit,train.data)
Evaluate_F1_Score(fitted, train.data$success, "Logistic Regression Train", 0.5)
EvaluateModel(fitted, train.data$success, "Logistic Regression Train", 0.5)
#Predict test data
pred <- predict(model.logit,test.data)
Evaluate_F1_Score(pred, test.data$success, "Logistic Regression Test", 0.6)
EvaluateModel(pred, test.data$success, "Logistic Regression Test", 0.6)




################################################################################
# LDA
################################################################################
#Set the train and test data 
train.data <- train.data.random
test.data <- test.data.random
train.data$success = as.factor(train.data$success)
test.data$success = factor(test.data$success)


library("MASS")
model.Lda = lda(formula = success ~ .,
                data = train.data )
summary(model.Lda)
model.Lda
#Predict on training data
fitted <- predict(model.Lda,train.data)

Evaluate_F1_Score(fitted$class, train.data$success, "Linear Discriminant Analysis Train", 0.5)
EvaluateModel(fitted$class, train.data$success, "Linear Discriminant Analysis Train", 0.5)
#Predict test data
pred <- predict(model.Lda,test.data)
Evaluate_F1_Score(pred$class, test.data$success, "Linear Discriminant Analysis Test", 0.5)
EvaluateModel(pred$class, test.data$success, "Linear Discriminant Analysis Test", 0.5)


################################################################################
# Mutlivariate Regression -MARS using caret
################################################################################
# Set the seed for reproducibility
set.seed(100)


fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10,
  savePredictions = TRUE,
  classProbs = TRUE
)

#Set the train and test data 
train.data <- train.data.scale
test.data <- test.data.scale


model.mars = train(success ~ ., data=train.data, method='earth',tuneLength = 5,trControl=fitControl)
fitted <- predict(model.mars)
summary(model.mars)
train.data$success = as.factor(train.data$success)
test.data$success = factor(test.data$success)

plot(model.mars, main="Model Accuracies with MARS")
varimp_mars <- varImp(model.mars)
plot(varimp_mars, main="Variable Importance with MARS")
Evaluate_F1_Score(fitted, train.data$success, "MARS Train", 0.5)
EvaluateModel(fitted, train.data$success, "MARS Train", 0.5)

pred <- predict(model.mars,test.data)
Evaluate_F1_Score(pred, test.data$success, "MARS Test", 0.5)
EvaluateModel(pred, test.data$success, "MARS Test", 0.5)






################################################################################
# SVM
################################################################################

# Set the seed for reproducibility
set.seed(3233)

library("e1071")
library("caret")
library("MLmetrics")

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  savePredictions = TRUE,
  classProbs = TRUE,
  repeats = 3
)

#Set the train and test data 
train.data <- train.data.random
test.data <- test.data.random


model.svm <- train(success ~., data = train.data, method = "svmLinear",
                    trControl=fitControl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
model.svm


train.data$success = as.factor(train.data$success)
test.data$success = factor(test.data$success)
#Predict on training data
fitted <- predict(model.svm,train.data)
fitted
varimp_svm<- varImp(model.svm)
plot(varimp_svm, main="Variable Importance with SVM")

Evaluate_F1_Score(fitted, train.data$success, "SVM Train", 0.5)
EvaluateModel(fitted, train.data$success, "SVM Train", 0.5)
#Predict test data
pred <- predict(model.svm,test.data)

Evaluate_F1_Score(pred, test.data$success, "SVM Test", 0.5)
EvaluateModel(pred, test.data$success, "SVM Test", 0.5)

################################################################################
# Decision tree
################################################################################
#Set the train and test data 
train.data <- train.data.random
test.data <- test.data.random
train.data$success = as.factor(train.data$success)
test.data$success = factor(test.data$success)

library("rpart")
library("rpart.plot")
model.tree <- rpart(formula = success ~ .,
                    data = train.data, control=rpart.control(cp=.01) )

rpart.plot(model.tree)
model.tree

#Predict on training data
fitted <- predict(model.tree,train.data)
fitted_df <- data.frame(fitted)
Evaluate_F1_Score(fitted_df$X1, train.data$success, "Decision Tree Train", 0.5)
EvaluateModel(fitted_df$X1, train.data$success, "Decision Tree Train", 0.5)
#Predict test data
pred <- predict(model.tree,test.data)
pred_df <- data.frame(pred)
Evaluate_F1_Score(pred_df$X1, test.data$success, "Decision Trees Test", 0.5)
EvaluateModel(pred_df$X1, test.data$success, "Decision Tree Test", 0.5)
################################################################################
# RFE
################################################################################

set.seed(100)

fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10,
  savePredictions = TRUE,
  classProbs = TRUE
)
#Set the train and test data 
train.data <- train.data.random
test.data <- test.data.random

# Train the model using random forest
model.rf = train(success ~ ., data=train.data, method='rf', tuneLength=5, trControl = fitControl,importance=T)
model.rf
train.data$success = as.factor(train.data$success)
test.data$success = factor(test.data$success)
varimp_rf<- varImp(model.rf)
plot(varimp_rf, main="Variable Importance with Random Forest")

#Predict on training data
fitted <- predict(model.rf,train.data)
fitted

Evaluate_F1_Score(fitted, train.data$success, "Random Forest Train", 0.5)
EvaluateModel(fitted, train.data$success, "Random Forest Train", 0.5)
#Predict test data
pred <- predict(model.rf,test.data)
Evaluate_F1_Score(pred, test.data$success, "Random Forest Test", 0.5)
EvaluateModel(pred, test.data$success, "Random Forest Test", 0.5)

################################################################################
# ANN
################################################################################

set.seed(100)

fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10,
  savePredictions = TRUE
)
#Set the train and test data 
train.data <- train.data.encoded
test.data <- test.data.encoded
train.data$success <- dependent.train.var
test.data$success <- dependent.test.var
# Train the model using random forest
model.nnet = train(success ~ ., data=train.data, method='nnet', tuneLength=5, trControl = fitControl,linout = TRUE)
model.nnet
train.data$success = as.factor(train.data$success)
test.data$success = as.factor(test.data$success)
varimp_nnet<- varImp(model.nnet)
plot(varimp_nnet, main="Variable Importance with NNET")

#Predict on training data
fitted <- predict(model.nnet,train.data)
fitted

Evaluate_F1_Score(fitted, train.data$success, "NNet Train", 0.5)
EvaluateModel(fitted, train.data$success, "NNet Train", 0.5)
#Predict test data
pred <- predict(model.nnet,test.data)
Evaluate_F1_Score(pred, test.data$success, "NNet Test", 0.5)
EvaluateModel(pred, test.data$success, "Nnet Test", 0.5)



