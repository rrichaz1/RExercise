install.packages("tidyverse")  #tidyerse has ggplot2 and dplyr
install.packages("readr")
install.packages("ROCR")


library("data.table")  # has fread
library("ggplot2")
require("tidyverse")
library(caret)
library("ROCR")

appeal_df <- fread("/Users/singhr/Documents/testcode/R_Exercise/Appeal/Appeal_xdapost1.csv")
data_space <- ggplot(data = appeal_df, aes(x = days_appeal_opened_after_discharge, y = is_payment_received_after)) + 
  geom_jitter(width = 0, height = 0.05, alpha = 0.5)
#makes a linear line
data_space +
  geom_smooth(method = "lm", se = FALSE) +
geom_smooth(method = "glm", se = 0, color = "red", 
            method.args = list(family = "binomial"))

appeal_pay_df <- appeal_df %>% filter(Days_Payment_after_appeal > 0 & !is.na(days_appeal_opened_after_discharge))
appeal_nopay_df <- appeal_df %>% filter(is_payment_received_after == 0)

#linear_mod <- lm(Days_Payment_after_appeal ~ days_appeal_opened_after_discharge +ip_op_num, data = appeal_pay_df)
linear_mod <- lm(Days_Payment_after_appeal ~ ip_op_num, data = appeal_pay_df)
summary(linear_mod)
#create new variable to help partition data
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
#table(train.d$pay_ipop)
test.d <- appeal_df[-in.train, ]
dim(train.d)
dim(test.d)


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
m <- glm(is_payment_received_after ~ ip_op_num+days_appeal_opened_after_discharge, family=binomial(link="logit"), data=train.d)

# Get model details and summary.
m
summary(m)

# Evaluate model performance with test data.
predicted <- predict(m, newdata=test.d, type="response")
EvaluateModel(predicted, test.d$is_payment_received_after, "Logistic Regression")

pred <- factor(ifelse(predicted > 0.6, 1, 0))
confusionMatrix(pred, as.factor(test.d$is_payment_received_after), positive="1")	
