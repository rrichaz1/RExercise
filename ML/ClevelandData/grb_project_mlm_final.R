
# install packages for basic data manipulation/visualization
install.packages("data.table")  # data manipulation
install.packages("dplyr")       # data wrangling
install.packages("ggplot2")     # plots/visualization 
install.packages("ggpubr")      # plots/multigrid visualization
install.packages("psych")       # basic statistics
install.packages("reshape2")    # data melting
install.packages("sm")          # density plot comparison
install.packages("PerformanceAnalytics") # analytics
install.packages("car")         # statistical analysis
install.packages("gvlma")       # global test of model assumptions
install.packages("lmtest")      # linear model test
install.packages("glmnet")      # lasso/ridge etc.
install.packages("relaimpo")    # relative importance of lm var
install.packages("pROC")        # auc for logistic
install.packages("foreach")     # loop through search grid
install.packages("DAAG")        # k-fold CV lm
install.packages("MLmetrics")   # f1_score for logistic
install.packages("e1071")       # for CART and others
install.packages("caret")       # for CART and others
install.packages("nnet")        # for neural network
install.packages("kernlab")     # for SVM
install.packages("randomForest") # random forest
install.packages("rpart")       # classification trees
install.packages("repart.plot") # plot classification trees
install.packages("fastDummies") # creating dummy variables
install.packages("mlr")         # ml
install.packages("plotmo")      # plot glmnet
install.packages("ROCR")

library("data.table")
library("dplyr") 
library("ggplot2")
library("ggpubr")
library("psych")
library("reshape2")
library("sm")
library("PerformanceAnalytics")
library("car")
library("gvlma")
library("lmtest")
library("glmnet")
library("relaimpo") 
library("pROC")
library("foreach")
library("DAAG")
library("MLmetrics")
library("e1071")
library("caret")
library("nnet")
library("kernlab")
library("randomForest")
library("rpart")
library("rpart.plot")
library("fastDummies")
library("plotmo")

### set working directory
setwd("/Users/rsing198/Documents/testcode/R_Exercise/ML/ClevelandData")


###########  HEART DISEASE DATASET #################

## READ AND PROFILE DATA 

# data source: 
# https://archive.ics.uci.edu/ml/datasets/heart+Disease
# data name: processed.cleveland.data (converted to csv)
rm(list=ls())
# read data
fulldat <- read.csv("cleveland_chd_data.csv", 
                    header=TRUE, 
                    stringsAsFactors=FALSE)


## CHECK TYPE, DIMENSION & STRUCTURE OF DATA

ls()
head(fulldat) # print out looks good
class(fulldat) # it's a data.frame - good
dim(fulldat) # 303 rows and 14 columns - good
str(fulldat) # variables are mostly character


## CHECK DATA QUALITY AND CLEAN DATA ####

## check column types and convert as necessary
sapply(fulldat, class)
library("dplyr")
colid = 1:14
fulldat[colid] = data.matrix(fulldat[colid])
sapply(fulldat, class)


####### CLEAN NA VALUES - DATA QUALITY ##########

table(is.na(fulldat)) #4539 FALSE, 6 TRUE CELLS (NA)
all_na = sapply(fulldat, function(x) all(is.na(x)))
summary(all_na) 
fulldat = fulldat[!all_na] #drop if all na in a column
col_na = sapply(fulldat, function(x) sum(is.na(x)))
col_na # thal(2), ca(4)
row_na = rowSums(is.na(fulldat))
row_na # six rows have 1 NA
cleandat <- fulldat[complete.cases(fulldat),] # keep complete cases
dim(cleandat) # 297 rows and 15 columns
sapply(cleandat, class)
str(cleandat)
## count total clean samples by location
count_by_num <- cleandat %>%
  group_by(num) %>%
  summarise(counts=n())
count_by_num 


############ READY FOR FURTHER PROCESSING ############

## convert the num column to binary only
cleandat$num[cleandat$num==0] <- 0
cleandat$num[cleandat$num!=0] <- 1

count_by_num <- cleandat %>%
  group_by(num) %>%
  summarise(counts=n())
count_by_num 


###### EXPLORATORY DATA ANALYSIS (EDA) ############

catvar <- c("sex","cp","fbs","restecg","exang","slope","ca","thal")
cntvar <- c("age","trestbps","chol","thalach","oldpeak")

## Create a list of cross tables

xtab_list <- lapply(cleandat[,catvar],
                    function(x) xtabs(~ x + cleandat$num))
xtab_list 

## readjust categories due to small size

# restecg 0 (normal) vs 1/2 (other)
cleandat$restecg[cleandat$restecg==0] <- 0
cleandat$restecg[cleandat$restecg!=0] <- 1

### keep only the variables that go into model consideration
allvar = c("age","sex","cp","chol","trestbps","fbs","restecg",
             "thalach","exang","oldpeak","slope","ca","thal","num")
anadat <- cleandat[allvar]
sapply(cleandat, class)

## convert categorical variables as factor
#faccols<-c("sex","cp","fbs","restecg","exang","slope","ca","thal","num")
#cleandat[faccols] <- lapply(cleandat[faccols], as.factor)


### SIMPLE HYPOTHESIS - STRONG CORRELATION WITH HD ##

corrx <- apply(cleandat, 2, 
               function(col)cor(col, cleandat$num))
corrx


## STACKED 100% BAR PLOT FOR CATEGORICAL VARIABLES ##

theme_set(theme_pubr())
layout(matrix(c(1),1))
ggplot(anadat,aes(factor(num))) + 
  geom_bar(fill="#0073C2FF")

## stacked % bar plot grouped by heart disease

plots <- lapply(cleandat[, catvar], function(x) 
    ggplot(anadat, aes(factor(x))) + 
      geom_bar(aes(fill=factor(num)), position="fill",
               show.legend = FALSE))
# multiple graphics in one page
library(ggpubr)
ggarrange( plots[[1]], plots[[2]], plots[[3]],
           plots[[4]], plots[[5]], plots[[6]],
           plots[[7]], plots[[8]], 
           labels = c("sex", "cp", "fbs", "restecg",
                  "exang", "slope", "ca", "thal"),
          ncol = 3, nrow = 3)

## index/scatter plot of all variables

layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), 
              4, 4, byrow=TRUE))
for(i in names(anadat)) {
  plot((anadat[,i]), main=i, ylab="") 
  } 

## density plot of all variables

layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), 
              4, 4, byrow=TRUE))
for(i in names(anadat)) {
  plot(density(anadat[,i]), main=i) 
  } 

## density plot compare by heart disease

library(sm)
dencomp = c("age","sex","cp","chol","trestbps","fbs","restecg",
            "thalach","exang","oldpeak","slope","ca","thal")

layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), 
              4, 4, byrow=TRUE))
for (i in dencomp) {
  sm.density.compare(anadat[, i], anadat$num,
                     xlab=names(anadat[i]))
  }

## histogram of continuous variables

layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow=TRUE))
for (i in cntvar) {
  hist(anadat[, i], main = names(anadat[i]), 
       xlab = names(anadat[i]))
  }

## qq plot of continuous variables

layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow=TRUE))
for (i in cntvar) {
  qqnorm(anadat[, i], pch =1, main = names(anadat[i]))
  qqline(anadat[, i], col = "steelblue", lwd = 2)
}

## box plot of continuous variables

layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow=TRUE))
for (i in cntvar) { 
  boxplot(anadat[, i] ~ anadat$num, 
          main = names(anadat[i]), 
          xlab = "Heart Disease")
}

## censor upper values for chol and oldpeak (outliers)

anadat$oldpeak <- pmin(anadat$oldpeak, 4.5)
anadat$chol <- pmin(anadat$chol, 450)



########### DISTRIBUTIONAL HYPOTHESIS TESTING ###############

## Develop CLT code to test particularly continuous variables
# Check distribution: normal? skewed? kurtotic? hypergeometric etc?
# Generate confidence intervals and p-values

layout(matrix(c(1),1))

library("psych")

## normal dist accepted range - kurtosis 3, skewness +/-2

## mean/median/std/skewness/kurtosis

ldesc <- lapply(anadat, describe)
rdesc <- sapply(ldesc, '[', c("n","mean","sd","se","median",
                              "min","max","skew","kurtosis"))
tdesc <- t(rdesc)
tdesc


## shapiro-wilk test

lshap <- lapply(anadat, shapiro.test)
rshap <- sapply(lshap, '[', c("statistic","p.value"))
tshap <- t(rshap)
tshap

### t-test for mean difference by heart disease

lttest <- lapply(anadat[,1:13], 
                 function(i) t.test(i~anadat$num))
test1 <- data.frame(t(sapply(lttest, '[', c("statistic","p.value"))))
test2 <- data.frame(t(sapply(lttest, getElement, name = "conf.int")))
colnames(test2) <- c("m_95ci_low", "m_95ci_hi")
test3 <- data.frame(t(sapply(lttest, getElement, name = "estimate")))
colnames(test3) <- c("mean_hd0", "mean_hd1")
test3$mean_diff <- test3$mean_hd0 - test3$mean_hd1
rttest <- cbind(test3, test1, test2)
rttest


### CORRELATION ANALYSIS ###

library(PerformanceAnalytics)
chart.Correlation(anadat, method="spearman", histogram=FALSE, 
                  pch="+", cex.cor.scale=2)
#chart.Correlation(anadatcln, method="spearman", histogram=TRUE, pch=16)

# alternate to the above correlation chart
library(psych)
pairs.panels(anadat) 

### alternate correlation matrix

corrdat <- anadat[, 1:14]
cormat <- round(cor(corrdat),2)

# Use correlation between variables as distance and order them

reorder_cormat <- function(cormat){
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
  }
cormat

## create correlation heatmap

library(reshape2)
library(ggplot2)

melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# get upper triangle of correlation matrix
get_upper_tri<-function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
  }
upper_tri <- get_upper_tri(cormat)
upper_tri

melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


######## ASSESS THE DEGREE OF ENDOGENIETY AND COLLINEARITY #########

# code source: https://www.statmethods.net/stats/rdiagnostics.html

library("car")

#### Multiple linear model without feature selection ####

mfull <- lm (num ~ age + sex + cp + chol + trestbps + fbs + restecg + 
      thalach + exang + oldpeak + slope + ca + thal, data = anadat)
#mfull <- lm( num ~ ., data = anadat) # easy way
summary(mfull)$r.squared
summary(mfull)
### age, chol, fbs and restecg are not significant
### thalach and oldpeak are weekly significant

# Residuals plot
plot(mfull$fitted.values, mfull$residuals, xlab = 'Fitted values', 
     ylab = 'Residuals', main = 'Residuals ~ Fitted Values')
abline(h = 0, col = 'red')
plot(mfull$residuals, ylab = 'Residuals', main = 'Residual Plot')
abline(h = 0, col = 'red')
hist(mfull$residuals)
shapiro.test(mfull$residuals)

# Q-Q plot
qqnorm(mfull$residuals, ylab='Residuals', main='Residuals Q-Q Plot')
qqline(mfull$residuals, col = 'red')

# Assessing Outliers
outlierTest(mfull) # Bonferonni p-value for most extreme obs
qqPlot(mfull, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(mfull) # leverage plots 

# Influential Observations
# added variable plots 
# avPlots(mfull) # same as leveragePlots(fit)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(anadat)-length(mfull$coefficients)-2))
cutoff
plot(mfull, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(mfull)


# Normality of Residuals
# qq plot for studentized resid
qqPlot(mfull, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(mfull) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xmfull<-seq(min(sresid),max(sresid),length=40) 
ymfull<-dnorm(xmfull) 
lines(xmfull, ymfull) 

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(mfull)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(mfull)

# Evaluate Collinearity
vif(mfull) # variance inflation factors 
sqrt(vif(mfull)) > 2 # problem?

# Evaluate Nonlinearity
# component + residual plot 
crPlots(mfull)

# Test for Autocorrelated Errors
durbinWatsonTest(mfull)

# Global test of model assumptions

library(gvlma)
gvmodel <- gvlma(mfull) 
summary(gvmodel) 

# No serious endogeniety or heteroskedasticity in data ####



########### LOGISTIc REGRESSION VARIABLE SELECTION #################


# A. Variable selection using stepwise regression

# R uses AIC value for step() selection, not p-value

## Stepwise selection

# base intercept only model
model.null <- glm(num ~ 1 , data= anadat, 
                  family=binomial(link="logit")) 
summary(model.null)

# full model with all predictors
model.full <- glm(num ~ . , data= anadat, 
                  family=binomial(link="logit"))
summary(model.full)

# Step-wise selection
model.step <- step(model.null, 
                   scope = list(lower = model.null, 
                                upper = model.full), 
                   direction = "both", 
                   test="Chisq", trace = 1, steps = 1000)  
summary(model.step) 

# get the shortlisted variable
shortlistedVars <- names(unlist(model.step[[1]])) 
# remove intercept 
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  
print(shortlistedVars)

# age, oldpeak and restecg are dropped by stepwise selection
# thal, ca, exang, cp, sex, thalach, trestbps, fbs, slope, chol selected

model.selected = glm(formula=num ~ thal + ca + exang + cp + sex + 
                    thalach + trestbps + fbs + slope + chol,
                  family = binomial(link = "logit"), 
                  data = anadat)
summary(model.selected)

# Analysis of variance for individual terms
library(car)
Anova(model.selected, type="II", test="Wald")
anova(model.selected, model.null, test="Chisq")

# log likelihood ratio test
library(lmtest)
lrtest(model.selected)

# overdispersion check (should be <1.5)
summary(model.selected)$deviance / summary(model.selected)$df.residual


# B. Variable selection using LASSO regression

# R uses cross validation for selecting lamba

# info source: 
# https://www.r-bloggers.com/variable-selection-with-elastic-net/
  
## LASSO regression
library("glmnet")
library("pROC")
library("foreach")
library("plotmo") 

# prepare data
mdlx<- as.matrix.data.frame(anadat[c(catvar, cntvar)])
mdly<- as.factor(anadat$num)

# LASSO (alpha=1)

# simple way to plot and check
glmlasso <- glmnet(mdlx, mdly, alpha=1, family="binomial")
plot(glmlasso, xvar="lambda", label=TRUE)
plot(glmlasso, xvar="norm", label=TRUE)
plot_glmnet(glmlasso, label=TRUE)

# run cross-validation with k=10 fold
cvlasso <- cv.glmnet(mdlx, mdly, family="binomial", 
          nfold=10, type.measure="deviance", alpha=1)
plot(cvlasso)
cvlasso$lambda.min
cvlasso$lambda.1se
#coef(cvlasso, cvlasso$lambda.min)
#coef(cvlasso, cvlasso$lambda.1se)

# run model with minLamda.min
mdlasso <- glmnet(mdlx, mdly, family="binomial",
                  lambda=cvlasso$lambda.min, alpha=1)
coef(mdlasso) 
roc(mdly, as.numeric(predict(mdlasso, mdlx, type = "response")))

## only age is dropped by minmum lamda
# stepwise and lasso drop different set of attributes
# restecg and oldpeak may be theoretically important
# going with LASSO (min lamba)

###Setting lambda = lambda.1se produces a simpler model 
### compared to lambda.min, but the model might be a little 
### bit less accurate than the one obtained with lambda.min.
### lamba.1se great if 1000s of variables to select from


########### ML MODELING WORKS BEGIN HERE #####################


# only age dropped from lasso

allvar = c("age","sex","cp","chol","trestbps","fbs","restecg",
           "thalach","exang","oldpeak","slope","ca","thal","num")

modeldat <- anadat[, !names(anadat) %in% c("age")]
dim(modeldat) 

# split data into data.train and data.test sets

library(dplyr)
set.seed(275) #to get repeatable data

data.train <- sample_frac(modeldat, 0.8)
train_index <- as.numeric(rownames(data.train))
data.test <- modeldat[-train_index, ]
dim(data.train) # 238 rows
head(data.train)
dim(data.test) # 62 rows
head(data.test)



### for each of the models below, make adjustment to data
### as train and test based of data.train and data.test

################################################################
####### this project is classification modeling ################
################################################################

#Build models and test accuracy in both training and test data
###############################################################

##### 1. LINEAR CLASSIFICATION 

# 1.1. Logistic regresion

library("e1071")
library("caret")
library("MLmetrics")

# prepare data
train <- data.train
test <- data.test
train[["num"]] = factor(train[["num"]])
test[["num"]] = factor(test[["num"]])

# run model
model11 = glm(formula = num ~ .,
              family = binomial (link = "logit"),
              data = train )
summary(model11)
model11

# accuracy of training model
pred_train <- predict(model11, train, type = "response")
pred_train <- ifelse(pred_train < 0.5, 0, 1)
mtab_train <- table(pred_train, train$num)
confusionMatrix(mtab_train)
F1_Score(y_pred = pred_train, y_true = train$num, positive = "1")
AUC(y_pred = pred_train, y_true = train$num)

# accuracy in test data
pred_test <- predict(model11, test, type = "response")
pred_test <- ifelse(pred_test < 0.5, 0, 1)
mtab_test <- table(pred_test, test$num)
confusionMatrix(mtab_test, positive = "1")
F1_Score(y_pred = pred_test, y_true = test$num, positive = "1")
AUC(y_pred = pred_test, y_true = test$num)


# 1.2. Linear Discriminant Analysis (LDA)

library("MASS")
library("MLmetrics")

# prepare data
train <- data.train
test <- data.test
train[["num"]] = factor(train[["num"]])
test[["num"]] = factor(test[["num"]])

# run model
model12 <- lda( num ~ ., data = train)
summary(model12)
model12

# accuracy of training model
pred_train <- predict(model12, train)
names(pred_train)
mtab_train <- table(pred_train$class, train$num)
confusionMatrix(mtab_train, positive = "1")
F1_Score(y_pred = pred_train$class, y_true = train$num, positive = "1")
AUC(y_pred = pred_train$class, y_true = train$num)

# accuracy in test data
pred_test <- predict(model12, test)
names(pred_test)
mtab_test <- table(pred_test$class, test$num)
confusionMatrix(mtab_test, positive = "1")
F1_Score(y_pred = pred_test$class, y_true = test$num, positive = "1")
AUC(y_pred = pred_test$class, y_true = test$num)



# 1.3 Support Vector Machine (svm_linear)

# http://dataaspirant.com/2017/01/19/
# support-vector-machine-classifier-implementation-r-caret-package/
library("e1071")
library("caret")
library("MLmetrics")

# prepare data
train <- data.train
test <- data.test
train[["num"]] = factor(train[["num"]])
test[["num"]] = factor(test[["num"]])

# train model
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model13 <- train( num ~ ., data = train, 
                  method = "svmLinear",
                  trControl = trctrl, 
                  preProcess = c("center", "scale"),
                  tuneLength = 10 )
summary(model13)
model13

# accuracy of training model
pred_train <- predict(model13, train)
mtab_train <- table(pred_train, train$num)
confusionMatrix(mtab_train, positive = "1")
F1_Score(y_pred = pred_train, y_true = train$num, positive = "1")
AUC(y_pred = pred_train, y_true = train$num)

# accuracy in test data
pred_test <- predict(model13, test)
mtab_test <- table(pred_test, test$num)
confusionMatrix(mtab_test, positive = "1")
F1_Score(y_pred = pred_test, y_true = test$num, positive = "1")
AUC(y_pred = pred_test, y_true = test$num)


##### 2. NON-LINEAR CLASSIFICATION 

# 2.1. Neural Network

library("e1071")
library("caret")
library("MLmetrics")
library("dplyr")
library("data.table")
library("fastDummies")
library("nnet")

# prepare data
# normalize data
modeldatnorm <- data.table(modeldat %>%
               mutate_at(vars("trestbps","thalach","oldpeak", "chol"), 
                         scale))
apply(modeldatnorm, 2, mean)
apply(modeldatnorm, 2, sd)
head(modeldatnorm)
# create dummy variables
modeldatdum <- fastDummies::dummy_cols(modeldatnorm,
               select_columns = c("cp","slope","ca","thal"),
               remove_first_dummy = FALSE,
               remove_most_frequent_dummy = FALSE)
# drop original after creating dummy
modeldatdum <- subset(modeldatdum, 
              select = -c(cp, slope, ca, thal)) 
# num as factor
modeldatdum$num = as.factor(modeldatdum$num)
head(modeldatdum)

# data train/test split
set.seed(275)
n = nrow(modeldatdum)
train <- sample(1:n, round(0.8*n))
test <- setdiff(1:n, train)
# train model
ideal <- class.ind(modeldatdum$num)
model21 <- nnet(modeldatdum[train, -c("num")],
                ideal[train, ], size = 10, softmax = TRUE)
summary(model21)
model21

# accuracy of training model
pred_train <- predict(model21, 
                      modeldatdum[train, -c("num")], 
                      type = "class")
mtab_train <- table(pred_train, modeldatdum[train, ]$num)
confusionMatrix(mtab_train, positive = "1")
F1_Score(y_pred = pred_train, y_true = modeldatdum[train, ]$num, 
         positive = "1")
AUC(y_pred = pred_train, y_true = modeldatdum[train, ]$num)

# accuracy in test data
pred_test <- predict(model21, modeldatdum[test, -c("num")], 
                     type = "class")
mtab_test <- table(pred_test, modeldatdum[test, ]$num)
confusionMatrix(mtab_test, positive = "1")
F1_Score(y_pred = pred_test, y_true = modeldatdum[test, ]$num, 
         positive = "1")
AUC(y_pred = pred_test, y_true = modeldatdum[test, ]$num)


# 2.2. k-Nearest Neighbors
#https://www.r-bloggers.com/k-nearest-neighbor-step-by-step-tutorial/
  
library("e1071")
library("caret")
library("MLmetrics")
library("dplyr")
library("data.table")
library("fastDummies")
library("nnet")
library("ROCR")

# prepare data
# normalize continuous variables
modeldatnorm <- data.table(modeldat %>%
               mutate_at(vars("trestbps","thalach","oldpeak"), scale))
apply(modeldatnorm, 2, mean)
apply(modeldatnorm, 2, sd)
head(modeldatnorm)

# create dummy for categorical variables
modeldatdum <- fastDummies::dummy_cols(modeldatnorm,
               select_columns = c("cp","slope","ca","thal"),
               remove_first_dummy = FALSE,
               remove_most_frequent_dummy = FALSE)
# drop original after creating dummy
modeldatdum <- subset(modeldatdum, 
                      select = -c(cp, slope, ca, thal)) 
# num as factor
modeldatdum$num = as.factor(modeldatdum$num)

# partition data little differently this time (use caret package)
set.seed(275)
index = createDataPartition(modeldatdum$num, p = 0.8, list = F)
train = modeldatdum[index, ]
test = modeldatdum[-index, ]
levels(train$num) <- make.names(levels(factor(train$num)))
levels(test$num) <- make.names(levels(factor(test$num)))

# set parameters
set.seed(1234)
trctrl <- trainControl(method = "repeatedcv", number = 10, 
                       repeats = 3, classProbs = TRUE, 
                       summaryFunction = twoClassSummary)
# train model
model22 <- train(num ~ ., data = train, method = "knn", 
                 preProcess = c("center", "scale"), 
                 trControl = trctrl,
                 metric = "ROC", tuneLength = 10)
summary(model22)
model22

# accuracy of training model
pred_train <- predict(model22, newdata = train)
mtab_train <- table(pred_train, train$num)
confusionMatrix(mtab_train, positive = "X1")
F1_Score(y_pred = pred_train, y_true = train$num, positive = "X1")

# accuracy in test data
pred_test <- predict(model22, newdata = test)
mtab_test <- table(pred_test, test$num)
confusionMatrix(mtab_test, positive = "X1")
F1_Score(y_pred = pred_test, y_true = test$num, positive = "X1")

#auc is calculated differently here
library("ROCR")
pred_train <- predict(model22, train, type = "prob")
pred_val_train <- prediction(pred_train[, 2], train$num)
perf_val_train <- performance(pred_val_train, "auc")
perf_val_train
perf_val_train <- performance(pred_val_train, "tpr", "fpr")
plot(perf_val_train, col = "green", lwd = 1.5)
ks_train <- max(attr(perf_val_train, "y.values")[[1]] -
                  (attr(perf_val_train, "x.values")[[1]]))
ks_train

pred_test <- predict(model22, test, type = "prob")
pred_val_test <- prediction(pred_test[, 2], test$num)
perf_val_test <- performance(pred_val_test, "auc")
perf_val_test
perf_val_test <- performance(pred_val_test, "tpr", "fpr")
plot(perf_val_test, col = "green", lwd = 1.5)
ks_test <- max(attr(perf_val_test, "y.values")[[1]] -
                 (attr(perf_val_test, "x.values")[[1]]))
ks_test

# 2.3. Naive Bayes

#https://www.r-bloggers.com/understanding-naive-bayes-classifier-using-r/
  
library("e1071")
library("caret")
library("MLmetrics")
library("mlr")

# prepare data
train <- data.train
test <- data.test
train$num <- as.factor(train$num)
test$num <- as.factor(test$num)

# train model
task <- makeClassifTask(data = train, target = "num")
model23 <- makeLearner("classif.naiveBayes")
model23 <- train(model23, task)
model23$learner.model
summary(model23)
model23

# accuracy of training model
pred_train <- as.data.frame(predict(model23, 
                                    newdata = train[, 1:12]))
mtab_train <- table(pred_train[, 1], train$num)
confusionMatrix(mtab_train, positive = "1")
F1_Score(y_pred = pred_train[, 1], train$num, positive = "1")
AUC(y_pred = pred_train[, 1], y_true = train$num)

# accuracy in test data
pred_test <- as.data.frame(predict(model23, 
                                   newdata = test[, 1:12]))
mtab_test <- table(pred_test[, 1], test$num)
confusionMatrix(mtab_test, positive = "1")
F1_Score(y_pred = pred_test[, 1], test$num, positive = "1")
AUC(y_pred = pred_test[, 1], y_true = test$num)


##### 3. NON-LINEAR CLASSIFICATION WITH DECISION TREES

# 3.1. Classification and Regression Trees(CART)

library("rpart")

# prepare data
train <- data.train
test <- data.test
train[["num"]] = factor(train[["num"]])
test[["num"]] = factor(test[["num"]])

# train model
model31 = rpart( num ~ ., data = train)
summary(model31)
model31

# accuracy of training model
pred_train <- predict(model31, train, type = "class")
mtab_train <- table(pred_train, train$num)
confusionMatrix(mtab_train, positive = "1")
F1_Score(y_pred = pred_train, train$num, positive = "1")
AUC(y_pred = pred_train, y_true = train$num)

# accuracy in test data
pred_test <- predict(model31, test, type = "class")
mtab_test <- table(pred_test, test$num)
confusionMatrix(mtab_test, positive = "1")
F1_Score(y_pred = pred_test, test$num, positive = "1")
AUC(y_pred = pred_test, y_true = test$num)


# 3.2 Bagging CART

library("ipred")

# prepare data
train <- data.train
test <- data.test
train[["num"]] = factor(train[["num"]])
test[["num"]] = factor(test[["num"]])

# train model
model32 = bagging( num ~ ., data = train)
summary(model32)
model32

# accuracy of training model
pred_train <- predict(model32, train)
mtab_train <- table(pred_train, train$num)
confusionMatrix(mtab_train, positive = "1")
F1_Score(y_pred = pred_train, train$num, positive = "1")
AUC(y_pred = pred_train, y_true = train$num)

# accuracy in test data
pred_test <- predict(model32, test)
mtab_test <- table(pred_test, test$num)
confusionMatrix(mtab_test, positive = "1")
F1_Score(y_pred = pred_test, test$num, positive = "1")
AUC(y_pred = pred_test, y_true = test$num)


# 3.3 Random Forest

library("randomForest")

# prepare data
train <- data.train
test <- data.test
train[["num"]] = factor(train[["num"]])
test[["num"]] = factor(test[["num"]])

# train model
model33 = randomForest( num ~ ., data = train)
summary(model33)
model33

# accuracy of training model
pred_train <- predict(model33, train)
mtab_train <- table(pred_train, train$num)
confusionMatrix(mtab_train, positive = "1")
F1_Score(y_pred = pred_train, train$num, positive = "1")
AUC(y_pred = pred_train, y_true = train$num)

# accuracy in test data
pred_test <- predict(model33, test)
mtab_test <- table(pred_test, test$num)
confusionMatrix(mtab_test, positive = "1")
F1_Score(y_pred = pred_test, test$num, positive = "1")
AUC(y_pred = pred_test, y_true = test$num)




