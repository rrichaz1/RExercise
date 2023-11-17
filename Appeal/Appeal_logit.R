install.packages("tidyverse")  #tidyerse has ggplot2 and dplyr
install.packages("readr")
install.packages("ROCR")


# library for Random Forest algorithm
install.packages("randomForest")
library("randomForest")
library("data.table")  # has fread
library("ggplot2")
require("tidyverse")
library(caret)
library("ROCR")

appeal_df <- fread("/Users/singhr/Documents/testcode/R_Exercise/Appeal/Appeal_xdapost1.csv")
appeal_df <- select (appeal_df,
                            days_appeal_opened_after_discharge,IP_OP_IND,
                            is_payment_received_after,
                            ip_op_num)
summary(appeal_df)

missing <- is.na(appeal_df)
which(rowSums(missing) == 1)
appeal_df <- appeal_df[rowSums(is.na(appeal_df))==0, ]
appeal_df %>% filter( !is.na(days_appeal_opened_after_discharge))
#is.na(appeal_df$is_payment_received_after)
#appeal_i_df <- appeal_df %>% filter(IP_OP_IND == 'I' & !is.na(days_appeal_opened_after_discharge))
#appeal_o_df <- appeal_df %>% filter(IP_OP_IND == 'O' & !is.na(days_appeal_opened_after_discharge))
#dim(appeal_i_df)
#dim(appeal_o_df)
#table(appeal_i_df$is_payment_received_after)
#table(appeal_o_df$is_payment_received_after)
#appeal_i_sample <- appeal_i_df %>% group_by(is_payment_received_after) %>% sample_n(1800)
#appeal_o_sample <- appeal_o_df %>% group_by(is_payment_received_after) %>% sample_n(9000)
#table(appeal_i_sample$is_payment_received_after)
#table(appeal_o_sample$is_payment_received_after)

#appeal_df <- rbind(appeal_i_sample,appeal_o_sample)
#dim(appeal_df)
appeal_df$IP_OP_IND <- as.factor(appeal_df$IP_OP_IND)
#appeal_df$cust_ipop <- as.factor(appeal_df$cust_ipop)

#use  variable cust_ipop to help partition data
#appeal_df = transform(appeal_df, pay_ipop = factor(paste(is_payment_received_after,IP_OP_IND)))
#summary(appeal_df)
#appeal_df$pay_ipop <- as.factor(appeal_df$pay_ipop)
#y1 <- appeal_df[, pay_ipop]
appeal_df$is_payment_received_after <- as.factor(appeal_df$is_payment_received_after)
in.train <- createDataPartition(appeal_df$is_payment_received_after, p=0.8, list=F)
#table(y1)
#table(in.train)
dim(in.train)
#dim(y1)
train.d <- appeal_df[in.train, ]
table(train.d$pay_ipop)
test.d <- appeal_df[-in.train, ]
dim(train.d)
dim(test.d)


EvaluateModel <- function(predicted, test.label, method.name) {	
  
  prediction.obj <- prediction(predicted, test.label)
  perf <- performance(prediction.obj, "tpr", "fpr")
  auc <- performance(prediction.obj, "auc") 
  
  # Generate ROC curve.
 plot(perf)
  legend("topleft", legend=paste("AUC=", round(auc@y.values[[1]], digits=4), sep=""), bty="n", cex=2)
  
  
 
  # Generate confusion matrix and other model statistics.
  pred <- factor(ifelse(predicted > 0.5, 1, 0))
  confusionMatrix(pred, test.label, positive="1")	
}

# Logistic regression model
# Train model with training data.
m <- glm(is_payment_received_after ~ ip_op_num+days_appeal_opened_after_discharge, family=binomial(link="logit"), data=train.d)
m2 <- glm(is_payment_received_after ~ days_appeal_opened_after_discharge, family=binomial(link="logit"), data=train.d)

# Get model details and summary.
m
m2
summary(m)

#Compare m and m2 to see which is better fit
anova(m, m2, test ="Chisq")

varImp(m)
#See coefficients as exponents
mod_fit <- train(is_payment_received_after ~ ip_op_num+days_appeal_opened_after_discharge, data=train.d,method="glm", family="binomial")
exp(coef(mod_fit$finalModel))
predict(mod_fit, newdata=test.d, type="prob")

# Evaluate model performance with test data.
predicted <- predict(m, newdata=test.d, type="response")
EvaluateModel(predicted, test.d$is_payment_received_after, "Logistic Regression")

#K-Fold Cross Validation
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
mod_fit <- train(is_payment_received_after ~ ip_op_num+days_appeal_opened_after_discharge, data=train.d,method="glm", 
                 family="binomial",
                 trControl = ctrl, tuneLength = 5)
predicted1 =predict(mod_fit, newdata=test.d)
#pred <- factor(ifelse(predicted1 > 0.5, 1, 0))
confusionMatrix(predicted1, test.d$is_payment_received_after, positive="1")
prEvaluateModel(pred, test.d$is_payment_received_after, "Logistic K Fold Regression")


# Random Forest model
m <- randomForest(is_payment_received_after ~ ip_op_num+days_appeal_opened_after_discharge, data=train.d)
m
summary(m)
predicted <- predict(m, newdata=test.d, type="prob")
predicted <- predicted[, 2]
EvaluateModel(predicted, test.d$is_payment_received_after, "Random Forest")
