attach(heart_disease_dataset)
#Create a subset of data with 3 features removed - age,fbs,chol- 1,5,6
heart_disease_subset.df<-subset(heart_disease_dataset,select=c(2,3,4,7,8,9,10,11,12,13,14))
View(heart_disease_subset.df)
str(heart_disease_subset.df)
library(DescTools)
describe(heart_disease_subset.df)
train.df <- heart_disease_subset.df[1:228,]
View(train.df)
test.df <- heart_disease_subset.df[229:303,]
logit.model<-glm(num ~ .,family=binomial(link='logit'),data=train.df)
summary(logit.model)
#anova(logit.model, test="Chisq")
plot(logit.model)
#prediction
fitted.results <- predict(logit.model,newdata=subset(test.df,select=c(1,2,3,4,5,6,7,8,9,10)),type='response')
View(fitted.results)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
View(fitted.results)
misClasificError <- mean(fitted.results != test.df$num)
print(paste('Accuracy',1-misClasificError))


#confusion matrix
library(caret)
fitted.results<-as.factor(fitted.results) # confusionMatrix command needs factor inputs 
confusionMatrix(data=fitted.results,reference=test.df$num)

# ROC and AUC
library(ROCR)
p <- predict(logit.model,newdata=subset(test.df,select=c(1,2,3,4,5,6,7,8,9,10)),type='response')
pr <- prediction(p, test.df$num)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#Normalize and rerun the logistic regression. didn't find any change in the model summary
heart_disease_test.df<-heart_disease_subset.df
heart_disease_norm.df<-mutate_each_(heart_disease_test.df,funs(scale),vars=c("trestbps","thalach","oldpeak"))
train.df <- heart_disease_norm.df[1:228,]
View(train.df)
test.df <- heart_disease_norm.df[229:303,]
logit.model<-glm(num ~ .,family=binomial(link='logit'),data=train.df)
summary(logit.model)



rm(
  list = ls()
)

