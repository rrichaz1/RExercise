#for normality tests
library("devtools")
install.packages("car")  #for correlation testing
install.packages("gvlma")  #for correlation testing


install.packages("tidyverse")   #for data wrangling
install.packages("ggpubr")  #for plots
install.packages("lsr")  #for chi sq test
devtools::install_github("ujjwalkarn/xda")  #for data exploration



library("car")
library("tidyverse")
library("ggplot2")
library("ggpubr")
library("lsr")
library("xda")


setwd("/Users/rsing198/Documents/testcode/R_Exercise/Appeal2MLM")


options(
  scipen = 999, digits = 3
) # Turns scientific notation off

#Read the dataset


appeal_df <- read.csv("dataset/Appeal_final_data.csv",header=TRUE)
str(appeal_df)
summary(appeal_df)

cat.vars <- c("success",    "code",       "payer",      "io" ) 
numeric.vars <- c("aplamt" ,    "covchg" ,    "totpay",     "los" ,      
                  "totadj" ,    "openday",    "postca" ,    "aplapayadj", "fpadj" ,     "lpadj"  )



###############################################
#Data Summary  
###############################################


#Change to factor and other manipulations for labels
appeal_df2 <- appeal_df
appeal_df2$success <- as.factor(appeal_df2$success)
appeal_df2$io <- as.factor(appeal_df2$io)

numSummary(appeal_df)
charSummary(appeal_df)

bivariates <- lapply(numeric.vars, function(indep.var) 
  bivariate(appeal_df2, "success", indep.var))
bivariates



#################################################
#Plots
#################################################

theme_set(theme_pubr())
layout(matrix(c(1),1))
ggplot(appeal_df2,aes(success)) + 
  geom_bar(fill="#0073C2FF")




levels(appeal_df2$success )[levels(appeal_df2$success )=="0"] <- "Fail"
levels(appeal_df2$success)[levels(appeal_df2$success )=="1"]   <- "Success"
levels(appeal_df2$io )[levels(appeal_df2$io )=="0"] <- "Inpatient"
levels(appeal_df2$io)[levels(appeal_df2$io )=="1"]   <- "Outpatient"
appeal_df2$code <- substring(appeal_df2$code,4,6)
head(appeal_df2)
str(appeal_df2)

labels <- c(Fail = "Fail", Success = "Success")

ggplot(appeal_df2, aes(x=factor(code)))+
  geom_bar(fill="#0073C2FF") + xlab("Denial Code") +facet_wrap(.~success, labeller=labeller(success = labels))


ggplot(appeal_df2, aes(x=factor(payer)))+
  geom_bar(fill="#0073C2FF") + xlab("Payers") +facet_wrap(.~success, labeller=labeller(success = labels))

ggplot(appeal_df2, aes(x=io))+
  geom_bar(fill="#0073C2FF") + xlab("Inpatient/Outpatient") +facet_wrap(.~success, labeller=labeller(success = labels))




## Density plots for continuous variables

plots <- lapply(appeal_df2[, numeric.vars[2:10]], function(indep.var) 
  ggplot(appeal_df2, aes(x=indep.var,color=success)) + 
    geom_density() )

# multiple graphics in one page

ggarrange( plots[[1]], plots[[2]], plots[[3]],
           plots[[4]], plots[[5]], plots[[6]],
           plots[[7]], plots[[8]], plots[[9]],
           labels = c( "cov chg","tot pay", "len of stay", "tot adj",
                      "appeal open after(days)", "post appeal ca", "after appeal pay/adj", "first pay/adj","last pay/adj"),
           ncol = 3, nrow = 4)
ggplot(appeal_df2, aes(x=aplamt,color=success)) + 
  geom_density() +labs(title = "Appeal Amount density")

##Box plots

## stacked % bar plot grouped by heart disease
appeal_df2 <- appeal_df %>% filter(aplamt < 4000)

str((appeal_df2))
summary(appeal_df2)
ggplot(appeal_df2, aes(x=factor(success), y=totadj)) + 
  geom_boxplot() +xlab("Success")



plots <- lapply(appeal_df2[, numeric.vars], function(indep.var) 
  ggplot(appeal_df2, aes(y=indep.var,x=success)) + 
    geom_boxplot() +xlab("Success"))

# multiple graphics in one page

ggarrange( plots[[1]], plots[[2]], plots[[3]],
           plots[[4]], plots[[5]], plots[[6]],
           plots[[7]], plots[[8]], plots[[9]], plots[[10]],
           labels = c( "appeal amt","cov chg","tot pay", "len of stay", "tot adj",
                       "appeal open after(days)", "post appeal ca", "after appeal pay/adj", "first pay/adj","last pay/adj"),
           ncol = 3, nrow = 4)
######################################################
###Hypothesis tests###########
######################################################

## shapiro-wilk test

lshap <- lapply(appeal_df[, numeric.vars], shapiro.test)
rshap <- sapply(lshap, '[', c("statistic","p.value"))
tshap <- t(rshap)
tshap

### t-test for mean difference by success

lttest <- lapply(appeal_df2[, numeric.vars], 
                 function(i) t.test(i~appeal_df2$success))
test1 <- data.frame(t(sapply(lttest, '[', c("statistic","p.value"))))
test2 <- data.frame(t(sapply(lttest, getElement, name = "conf.int")))
colnames(test2) <- c("95ci_low", "95ci_hi")
test3 <- data.frame(t(sapply(lttest, getElement, name = "estimate")))
colnames(test3) <- c("mean_hd0", "mean_hd1")
test3$mean_diff <- test3$mean_hd0 - test3$mean_hd1
rttest <- cbind(test3, test1, test2)
rttest


##############
# Chisq tests for categorical variables
tab_io <- table(appeal_df$success, appeal_df$io)
tab_io
prop.table(tab_io)
summary(prop.table(tab_io))
chisq.test(tab_io,p=0.05)
cramersV(tab_io)


tab_code <- table(appeal_df$success, appeal_df$code)
tab_code
prop.table(tab_code)
summary(prop.table(tab_code))
chisq.test(tab_code,p=0.05)
cramersV(tab_code)


tab_payer <- table(appeal_df$success, appeal_df$payer)
tab_payer
prop.table(tab_payer)
summary(prop.table(tab_payer))
chisq.test(tab_payer,p=0.05)
cramersV(tab_payer)


#Code and Payer are strongly related with success

######################################################
#  Assessing normality collinearity
#https://www.statmethods.net/stats/rdiagnostics.html
######################################################
model.full <- lm(success~., data=appeal_df)
summary(model.full)


#Plot the residuals
residualPlots(model.full)


# Assessing Outliers
outlierTest(model.full) # Bonferonni p-value for most extreme obs
qqPlot(model.full, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(model.full) # leverage plots

# Evaluate Collinearity
vif(model.full) # variance inflation factors 
sqrt(vif(model.full)) > 2 # problem?

# Test for Autocorrelated Errors
durbinWatsonTest(model.full)

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(model.full) 
summary(gvmodel)

####After removing collinear variables
model.noncollinear <- lm(formula=success ~ code + payer + postca  +io+los + aplamt +aplapayadj + openday + fpadj, data=appeal_df) 
# Evaluate Collinearity
vif(model.noncollinear) # variance inflation factors 
sqrt(vif(model.noncollinear)) > 2 # problem?


# Test for Autocorrelated Errors
durbinWatsonTest(model.noncollinear)

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(model.noncollinear) 
summary(gvmodel)

#########################################################################
#Value         p-value                   Decision
#Global Stat        674.8 0.0000000000000 Assumptions NOT satisfied!
#  Skewness           228.9 0.0000000000000 Assumptions NOT satisfied!
#  Kurtosis            45.6 0.0000000000148 Assumptions NOT satisfied!
#  Link Function       15.9 0.0000664190992 Assumptions NOT satisfied!
#  Heteroscedasticity 384.5 0.0000000000000 Assumptions NOT satisfied!
#########################################################################
#Linear modeling may not be the most appropriate
########################################################################
#Drop the columns having high collinearity and try LDA to see if there are collinearity warnings

appeal_subset_df <- appeal_df %>%
  select(-c("covchg","totadj","totpay","lpadj"))
str(appeal_subset_df)

library("MASS")
model.Lda = lda(formula = success ~ .,
                data = appeal_subset_df )
summary(model.Lda)
model.Lda
model.Lda$scaling

rmAll(FALSE)
