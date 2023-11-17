# Read the heart disease datset in to dataframe and display summary

heart_disease_dataset <- read.csv(file="cleveland_chd_data.csv",header = TRUE)
str(heart_disease_dataset)
library(psych)
describe(heart_disease_dataset)
summary(heart_disease_dataset)

heart_disease_dataset

#Check if there are missing values in the dataset. no missing values found
sapply(heart_disease_dataset,function(x) sum(is.na(x))) 
heart_disease_dataset$ca <- gsub("?","0",heart_disease_dataset$ca, fixed = TRUE)
heart_disease_dataset$thal<-gsub("?",3,heart_disease_dataset$thal, fixed = TRUE)
heart_disease_dataset$ca<-as.factor(heart_disease_dataset$ca)
heart_disease_dataset$thal<-as.factor(heart_disease_dataset$thal)

#Change categorical variables in to factors
heart_disease_dataset$sex<-factor(heart_disease_dataset$sex, labels=c("F", "M"))
heart_disease_dataset$fbs<-factor(heart_disease_dataset$fbs, labels=c("Normal", "High"))
heart_disease_dataset$cp<-factor(heart_disease_dataset$cp, labels=c("typical", "atypical", "non-anginal","asymptomatic"))
heart_disease_dataset$num<-factor(heart_disease_dataset$num, labels=c("0", "1", "1","1","1"))
heart_disease_dataset$restecg<-factor(heart_disease_dataset$restecg, labels=c("Normal","Abnormal","Abnormal"))
heart_disease_dataset$slope<-factor(heart_disease_dataset$slope, labels=c("Normal","Abnormal","Abnormal"))

attach(heart_disease_dataset)
# Histogram shows that majority of patients are in between 50-60 years
hist(age, breaks=seq(0, 80, by=10), col="darkgray", border="black",main="Age Distribution")

install.packages("sm")  
require("sm")
# create value labels 
sex.f<-factor(sex,levels=levels(sex), labels = c("Female", "Male"))
# plot densities 
sm.density.compare(age, sex, xlab="Age in years")
title(main="Age distribution by Sex")
#add legend 
colfill<-c(2:(2+length(levels(sex.f))))
legend(x =75, y=0.04,levels(sex.f), fill=colfill)

heart_disease_dataset$num <-as.factor(heart_disease_dataset$num)
sm.density.compare(age,num, xlab="age")
title(main="age distribution by num")
colfill<-c(2:(2+length(levels(num))))
legend(x = 70, y=0.04,levels(num), fill=colfill)

sm.density.compare(age,num, xlab="age")
title(main="age distribution by heart disease")
colfill<-c(2:(2+length(levels(num))))
legend(x = 70, y=0.04,c("Heart Disease","No Heart Disease"), fill=c("Green","Red"))


#Computing correlation between independent variables and response variable
#i). Testing if there is a statistically significant correlation between two variables
#ii). Quantifying the association or 'goodness of fit' between the two variables. 

# Barplot and chi-square test to determine corelation between fasting blood sugar and heart disease
#no correlation found . p-value = 0.7813, cramersV =  0.01595115
library(lsr)# for CramersV
counts <- table(num, fbs)
barplot(counts, main=" Distribution by fbs and num",
        xlab="fbs", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
chisq.test(counts,p=0.05)
cramersV(counts)

# Barplot and chi-square test to determine corelation between sex and heart disease
#Significant correlation found. p-value = 2.667e-06, CramersV = 0.2697179

sex_num.tbl<-table(num, sex)
barplot(sex_num.tbl, main=" Distribution by sex and num",
        xlab="sex", col=c("darkblue","red"),beside=TRUE)
legend ("top",c("No Heart Disease", "Heart Disease"), fill = c("darkblue","red"))
chisq.test(sex_num.tbl,p=0.05)
cramersV(sex_num.tbl)

# Barplot and chi-square test to determine corelation - chest pian type and heart disease
#  Shows significance and strong association, but doesn't make sense
# p-value < 2.2e-16, CramersV =  0.5196336
cp_num.tbl<-table(num, cp)
cp_num.tbl
barplot(cp_num.tbl, main=" Distribution of heart disease by chest pain type",
        xlab="cp", col=c("darkblue","red"), beside=TRUE)
legend ("top", c("No Heart Disease", "Heart Disease"),  fill = c("darkblue","red"))
chisq.test(cp_num.tbl,p=0.05)
cramersV(cp_num.tbl)
library("DescTools", lib.loc="C:/Program Files/R/R-3.5.1/library")
CramerV(cp_num.tbl)


# Barplot and chi-square test to determine corelation - exercise induced angina and heart disease
#  Shows significance and strong association
# p-value = 1.414e-13,  CramersV = 0.4248332
exang_num.tbl<-table(num, exang)
exang_num.tbl
barplot(exang_num.tbl, main=" Distribution of heart disease by exercise induced angina",
        xlab="exang", col=c("darkblue","red"), beside=TRUE)
legend ("top", c("No Heart Disease", "Heart Disease"),  fill = c("darkblue","red"))
chisq.test(exang_num.tbl,p=0.05)
cramersV(exang_num.tbl)

#Barplot and chi-square test to determine association between resting ECG and Heart Disease
# p-value = 0.003233, CramersV = 0.1691668
library(ggplot2)
ggplot( data = heart_disease_dataset) + geom_bar(aes(x=restecg, fill=as.factor(num))) +ggtitle("Rest ECG Vs Heart Disease")
restecg_num.tbl<-table(num,restecg)
chisq.test(restecg_num.tbl,p=0.05)
cramersV(restecg_num.tbl)

# Barplot and chi-square test to determine corelation - slope of the peak exercise ST segment and heart disease
# ST segment depression (horizontal or downsloping) is the most reliable indicator of exercise-induced ischaemia
slope_num.tbl<-table(num, slope)
slope_num.tbl
barplot(slope_num.tbl, main=" Distribution of heart disease by ST Segment depression",
        xlab="slope", col=c("darkblue","red"), beside=TRUE)
legend ("top", c("No Heart Disease", "Heart Disease"),  fill = c("darkblue","red"))
chisq.test(slope_num.tbl,p=0.05)
cramersV(slope_num.tbl)


# Barplot and chi-square test to determine corelation between no: of blood vessels colored and heart disease
# p-value = 1.174e-15, CramersV =0.4895505
ca_num.tbl<-table(num, ca)
ca_num.tbl
barplot(ca_num.tbl, main=" Distribution of heart disease by no:of blood vessels colored",
        xlab="No: of blood vessels colored", col=c("darkblue","red"), beside=TRUE)
legend ("top", c("No Heart Disease", "Heart Disease"),  fill = c("darkblue","red"))
chisq.test(ca_num.tbl,p=0.05)
cramersV(ca_num.tbl)

#**************************************************************
#continuous variable versus binary response
#**************************************************************
#Boxplot of ST depression variances between  patients with heart disease and no heart disease
temp.df <- subset(heart_disease_dataset,select=c(10,14))
temp.df$num<-factor(num, labels=c("Heart Disease", "No Heart Disease"))
boxplot(temp.df$oldpeak ~ temp.df$num, notch = TRUE, col = "grey", ylab = "oldpeak", main = "Boxplot of ST Depression by Heart Disease",  boxwex = 0.5, varwidth = TRUE)
dev.off()
library(gplots)
# Plot the mean ST Depression values between patient groups 
# having heart disease and no heart disease
plotmeans(temp.df$oldpeak ~ temp.df$num, data = temp.df, frame = TRUE, mean.labels = TRUE, connect = TRUE)
library(polycor)
polyserial(oldpeak,num) # 0.5330833
# Plot the mean of maximum heart rate achieved between patient groups 
# having heart disease and no heart disease
temp_thalach.df<-subset(heart_disease_dataset,select=c(8,14))
plotmeans(temp_thalach.df$thalach ~ temp_thalach.df$num, data = temp_thalach.df, frame = TRUE, mean.labels = TRUE, connect = TRUE)
library(polycor)
polyserial(thalach,num)
#[1] -0.5238619

#
temp_age.df<-subset(heart_disease_dataset,select=c(1,14))
plotmeans(temp_age.df$age ~ temp_age.df$num, data = temp_age.df, frame = TRUE, mean.labels = TRUE, connect = TRUE)
library(polycor)
polyserial(age,num) #.2801858
polyserial(chol,num) #.1069451
boxplot(chol ~ num, notch = TRUE, col = "grey", ylab = "chol", main = "Boxplot of chol and Heart Disease",  boxwex = 0.5, varwidth = TRUE)
#****************************************************************************
#  Linear regression -  Max Heart Rate Vs Age
#***************************************************************************

attach(heart_disease_dataset)
lm_hd_mod<- lm(thalach ~ age, data = heart_disease_dataset)
summary(lm_hd_mod)
#lm_hd_mod2<- lm(thalach ~ trestbps , data = heart_disease_dataset)
#summary(lm_hd_mod2)

library(broom)
model.diag.metrics <- augment(lm_hd_mod)
head(model.diag.metrics)

#Plot the residuals
ggplot(model.diag.metrics, aes(age, thalach)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = age, yend = .fitted), color = "red", size = 0.3)

ggplot(model.diag.metrics, aes(age, thalach)) +geom_point() +stat_smooth(method = lm, se = FALSE) +
geom_segment(aes(xend = age, yend = .fitted), color = "red", size = 0.3)

# Regression diagnostics plots 

par(mfrow = c(2, 2))
plot(lm_hd_mod)

# Cook's distance
plot(lm_hd_mod, 4)
#plot(lm_hd_mod, 4, id.n = 5)

#library(tidyverse)
#library(broom)
#model.diag.metrics %>% 
#top_n(3, wt = .cooksd)
