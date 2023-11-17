library("devtools")
install.packages("caret")
install.packages("data.table")
devtools::install_github("ujjwalkarn/xda")
install.packages("tidyverse")
install.packages("PCAmixdata")
install.packages("skimr")
install.packages("randomForest")
install.packages("ROCR")
install.packages("e1071")

library("data.table")  # has fread
library("ggplot2")
library("tidyverse")
library("dplyr")
library("caret")
library("xda")
library("PCAmixdata")
library("skimr")
library("randomForest")
library("ROCR")
library("e1071")


options(
  scipen = 999, digits = 3
) # Turns scientific notation off


setwd("/Users/rsing198/Documents/testcode/R_Exercise/Appeal2MLM")

#Read the dataset

#appeal_no_na_df <- fread("dataset/Appeal_with_result_data.csv")
appeal_no_na_df <- read.csv("dataset/Appeal_with_result_data.csv",header=TRUE)
summary(appeal_no_na_df)
str(appeal_no_na_df)

###Create lists of categorical and numerical variables
categorical_var <- c('code', 'payer', 'io')


counts <- appeal_no_na_df %>% group_by(success,io) %>% tally()
counts




###########################################################
##########Some exploratory data analysis###################
num.summ <- numSummary(appeal_no_na_df)
num.summ
char.summ <- charSummary(appeal_no_na_df)
char.summ 

#Run PCA on dataset numerical data only

appeal.pca = prcomp(appeal_no_na_df[,5:15], scale = TRUE)

x<-summary(appeal.pca)
x
names(appeal.pca)
appeal.pca$sdev
appeal.pca$x
a <- appeal.pca$rotation
a
biplot(appeal.pca,scale = 0,expand=20)#, xlim=c(-0.10, 0.10), ylim=c(-0.10, 0.1))
pr.var =appeal.pca$sdev ^2
pve=pr.var/sum(pr.var )
pve
plot(pve , xlab=" Principal Component ", ylab=" Proportion of Variance Explained ", ylim=c(0,1) ,type='b')
plot(cumsum (pve ), xlab=" Principal Component ", ylab ="Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,
     type='b')

#Drop finden variable as it was used to compute success column
appeal_no_na_df <- appeal_no_na_df %>%
  select(-"finden")

#Scale the numerical data
# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(appeal_no_na_df[,4:14], method=c("center", "scale"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
appeal_scaled_df <- predict(preprocessParams, appeal_no_na_df)
appeal_scaled_df
str(appeal_scaled_df)

#pca with mix data
split.appeal <- splitmix(appeal_scaled_df[,1:14])
X1 <- split.appeal$X.quanti 
X2 <- split.appeal$X.quali 
res.pcamix <- PCAmix(X.quanti=X1, X.quali=X2,rename.level=TRUE,
                     graph=FALSE)
res.pcamix
#variance of the principal components
res.pcamix$eig

dim(res.pcamix$Z)

# use kmeans on 1st few dimensions after PCA
appeal_pca_num <- appeal.pca$x[, 1:5] 
km.appeal.num <- kmeans (appeal.pca.num,2, nstart =25)
km.appeal.num$cluster
str( km.appeal.num)
km.appeal.num
plot(appeal.pca.num, col =(km.appeal.num$cluster +1) , main="K-Means Clustering Results with K=2", 
     xlab ="", ylab="", pch =20, cex =2)

appeal.pca.mix <- res.pcamix$Z[, 1:5] 
km.appeal.mix <- kmeans (appeal.pca.mix,2, nstart =25)

km.appeal.mix
plot(appeal.pca.mix, col =(km.appeal.mix$cluster +1) , main="K-Means Clustering Results with K=2", 
                  xlab ="", ylab="", pch =20, cex =2)


#descriptive statistics

skimmed <- skim_to_wide(appeal_no_na_df)
skimmed
skimmed[, c(1:5, 9:11, 13, 15:16)]


# Do one hot encoding 




dummies_model <- dummyVars(success ~ ., data=appeal_no_na_df)
appeal.encoded<- predict(dummies_model, newdata = appeal_no_na_df)

result <- appeal_no_na_df$success
# # Convert to dataframe
appeal.encoded.df <- data.frame(appeal.encoded)
str(appeal.encoded.df)


preProcess_range_model <- preProcess(appeal.encoded.df, method='range')
appeal.encoded.range.df <- predict(preProcess_range_model, newdata = appeal.encoded.df)
str(appeal.encoded.range.df)
# Append the Y variable
appeal.encoded.range.df$result <- result

summ <- numSummary(appeal.encoded.range.df)

#Plot the variables  -- not working
featurePlot(x = appeal.encoded.range.df[, 1:26],
            y = appeal.encoded.range.df$result,
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"),
                          y = list(relation="free")))

boxplot(appeal.encoded.range.df, expand = 50)


featurePlot(x = appeal.encoded.range.df[, 1:26],
            y = appeal.encoded.range.df$result,
            plot = "density",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"),
                          y = list(relation="free")))


table(appeal.encoded.range.df$X.i.o. , appeal.encoded.range.df$result)




#Plot the dependent variable against all dependent variables
Plot(appeal_no_na_df, 'success')

boxplot(appeal_no_na_df[,4:16])

Ux<-bivariate(appeal.encoded.range.df, 'success','code')
class(x)




















# 1 Logistic regresion
model.Logit = glm(formula = result ~ .,
              family = binomial (link = "logit"),
              data = train.data )
summary(model.Logit)
model.Logit

anova(model.Logit, test="Chisq")


# Evaluate model performance with test data.
predicted <- predict(model.Logit, newdata=test.data, type="response")
EvaluateModel(predicted, test.data$result, "Logistic Regression")
predicted

# 2 Linear Discriminant Analysis
library("MASS")
model.Lda = lda(formula = result ~ .,
                  data = train.data )
summary(model.Lda)
model.Lda




# Evaluate model performance with test data.
predicted <- predict(model.Lda, newdata=test.data, type="response")
names(predicted)
mtab_test <- table(predicted$class, test.data$result)
confusionMatrix(mtab_test, positive = "1")
EvaluateModel(predicted$class, test.data$result, "Linear Discriminant Analysis")


prediction.obj <- prediction(predicted$posterior, test.data$result)
?prediction
str(predicted)
str(result)


rm(
  list = ls()
) # Clears global environment of all visible objects (some hidden objects
