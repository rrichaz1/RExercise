# For feature selection
install.packages("Boruta")

# For data manipulation
install.packages("tidyverse")
#For feature selection
install.packages("caret")
#For Correlation plots
install.packages("corrplot")


library("Boruta")
library("tidyverse")
library("caret")
library("corrplot")
library(MASS)

options(
  scipen = 999, digits = 3
) # Turns scientific notation off


setwd("/Users/rsing198/Documents/testcode/R_Exercise/Appeal2MLM")

#Read the dataset


appeal_df <- read.csv("dataset/Appeal_final_data.csv",header=TRUE)
summary(appeal_df)
str(appeal_df)
is.factor(appeal_df$code)
is.factor(appeal_df$payer)


###################################################
#Plot the correlations between numerical variables
pairs(appeal_df[,5:14])

##Pearson's correlation
corr_vals <- cor(appeal_df[,5:14])
corr_vals
corrplot(corr_vals, method = "shade", title="Corr plot Pearson")
#Spearman Correaltion
corr_vals_spearman <- cor(appeal_df[,5:14],method ="spearman")
corr_vals_spearman
corrplot(corr_vals_spearman, method = "shade", title="Corr plot Spearman")

#Kendall Correlation
corr_vals_kendal <- cor(appeal_df[,5:14],method ="kendall")
corr_vals_kendal
corrplot(corr_vals_kendal, method = "shade", title="Corr plot Kendall")

#######################################################################

#scale the data as the values are varied 
preProcess_range_model <- preProcess(appeal_df, method='range')
appeal_scaled_df <- predict(preProcess_range_model, newdata = appeal_df)
#########################################################################
## Dimenionsionality reduction with PCA  
#
#########################################################################
appeal.pca = prcomp(appeal_scaled_df[,4:14], scale = TRUE)

x<-summary(appeal.pca)
x
names(appeal.pca)
appeal.pca$sdev
appeal.pca$x

biplot(appeal.pca,scale = 0,expand=20)#, xlim=c(-0.10, 0.10), ylim=c(-0.10, 0.1))
pr.var =appeal.pca$sdev ^2
pve=pr.var/sum(pr.var )
pve
plot(pve , xlab=" Principal Component ", ylab=" Proportion of Variance Explained ", ylim=c(0,1) ,type='b')
plot(cumsum (pve ), xlab=" Principal Component ", ylab ="Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,
     type='b')



## Use PCA Mix for mixed categorical and quantitative data  

library(PCAmixdata)

appeal_df2 <- appeal_scaled_df
appeal_df2$io <- as.factor(appeal_df2$io)
str(appeal_df2)
#pca with mix data
split.appeal <- splitmix(appeal_df2[,2:14])
X1 <- split.appeal$X.quanti 
X2 <- split.appeal$X.quali 
res.pcamix <- PCAmix(X.quanti=X1, X.quali=X2,rename.level=TRUE,
                     graph=FALSE)
res.pcamix
#variance of the principal components
a<-res.pcamix$eig
class(a)
b <- data.frame(a)
b
plot(b$Proportion, xlab=" Principal Component ", ylab=" Proportion of Variance Explained ", ylim=c(0,100) ,type='b')
plot(b$Cumulative, xlab=" Principal Component ", ylab ="Cumulative Proportion of Variance Explained ", ylim=c(0,100) ,
     type='b')


#LDA
library("MASS")
model.Lda = lda(formula = success ~ .,
                data = appeal_scaled_df )
summary(model.Lda)
model.Lda


##############################################################

########Use Boruta to find most important features#############

#### https://www.datacamp.com/community/tutorials/feature-selection-R-boruta
#################################################################

library(Boruta)
set.seed(111)
boruta.appeal_data <- Boruta(success~., data = appeal_scaled_df, doTrace = 2)
print(boruta.appeal_data)

##########No attributes deemed unimportant.############
plot(boruta.appeal_data, xlab = "", xaxt = "n", expand = 20)
lz<-lapply(1:ncol(boruta.appeal_data$ImpHistory),function(i)
  boruta.appeal_data$ImpHistory[is.finite(boruta.appeal_data$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.appeal_data$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.appeal_data$ImpHistory), cex.axis = 0.7)
#########################This did not drop any features#######################

###########################################################################
###Using caret package for feature selection  --- pretty resource intensive  *** takes long time to run***
###########################################################################

## Use automated feature selection 

# Caret package - A complete guide to machine learning in r
##################################
set.seed(100)
options(warn=-1)
subsets <- c(1:5, 10, 15, 18)
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)
lmProfile <- rfe(x=appeal_scaled_df[, 2:14], y=appeal_scaled_df$success,
                 sizes = subsets,
                 rfeControl = ctrl, metric="Accuracy")
 lmProfile
 
 
 
 ##################################
 #The top 5 variables (out of 13):
 #  code, payer, postca, totpay, lpadj
 #####################################
 print(lmProfile)
 predictors(lmProfile)

 

# plot the results

ggplot(lmProfile, type=c("g","o")) + 
  scale_x_continuous(breaks=2:14, labels=names(appeal_scaled_df)[2:14])




####################################################
#Step wise  feature selection with AIC
###########################################
## Stepwise selection

#http://r-statistics.co/Variable-Selection-and-Importance-With-R.html

# base intercept only model
model.null <- glm(success ~ 1 , data= appeal_scaled_df, 
                  family=binomial(link="logit")) 
summary(model.null)

# full model with all predictors
model.full <- glm(success ~ . , data= appeal_scaled_df, 
                  family=binomial(link="logit"))
summary(model.full)

# Step-wise selection
model.step <- step(model.null, 
                   scope = list(lower = model.null, 
                                upper = model.full), 
                   direction = "both", 
                   test="Chisq", trace = 1, steps = 1000)  
summary(model.step) 

model.step
# get the shortlisted variable
shortlistedVars <- names(unlist(model.step[[1]])) 
#shortlistedVars
# remove intercept 
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  
print(shortlistedVars)

# aplamt, openday and aplapayadj are dropped by stepwise selection
# code,payer,totadj,lpadj,fpadj,io,totpay,covchg,postca,los selected

model.selected = glm(formula=success ~ code + payer + totadj + lpadj + fpadj + 
                       io+postca +los,
                     family = binomial(link = "logit"), 
                     data = appeal_scaled_df)
summary(model.selected)

##############################################################################################################################
##################Use MASS package##########################################################################################
####http://www.sthda.com/english/articles/36-classification-methods-essentials/150-stepwise-logistic-regression-essentials-in-r/
##############################################################################################################################
model.full <- glm(success ~ . , data= appeal_scaled_df, 
                  family=binomial)
coef(model.full)
model.full


step.model <- model.full %>% stepAIC(trace = FALSE)
coef(step.model)
step.model




#################################################################################

###########################################################################
##Run glm on features selected by rfe

model.selected.rfe = glm(formula=success ~ code + payer + postca + lpadj + fpadj +io+los + aplamt +totadj + openday ,
                     family = binomial(link = "logit"), 
                     data = appeal_scaled_df)
summary(model.selected.rfe)


####################################################################
# Find cut off value

##https://stats.stackexchange.com/questions/199978/optimizing-probability-thresholds-in-a-glm-model-in-caret
#####################################################################
cutoffs <- seq(0.1,0.9,0.1)
accuracy <- NULL
for (i in seq(along = cutoffs)){
  prediction <- ifelse(model.selected.rfe$fitted.values >= cutoffs[i], 1, 0) #Predicting for cut-off
  accuracy <- c(accuracy,length(which(appeal_scaled_df$success ==prediction))/length(prediction)*100)
}

plot(cutoffs, accuracy, pch =19,type='b',col= "steelblue",
     main ="Logistic Regression", xlab="Cutoff Level", ylab = "Accuracy %")
#############################################################################
# value of 0.5 is most optimal
##############################################################################

rm(
  list = ls()
) # Clears global environment of all visible objects (some hidden objects

