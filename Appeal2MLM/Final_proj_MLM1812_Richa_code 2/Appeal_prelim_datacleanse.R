#Install packages
install.packages("devtools")
#For xda
devtools::install_github("ujjwalkarn/xda")
#Use Fread to read thedata
install.packages("data.table")
##Plotting and Data Wrangling
install.packages("tidyverse") 



library("data.table")  # has fread
library("ggplot2")
library("tidyverse")
library("dplyr")


library(xda)


options(
  scipen = 999, digits = 3
) # Turns scientific notation off


setwd("/Users/rsing198/Documents/testcode/R_Exercise/Appeal2MLM")

#Read the dataset

appeal_all_df <- fread("dataset/Appeal_All_xdapre.csv")
#####This is a larger dataset and is not included 
str(appeal_all_df)
summary(appeal_all_df)
#table(appeal_all_df$APL_DISPOSITION_CODE_DESC)
dim(appeal_all_df)
##Split the code and description into code and description columns
appeal_all_df <- appeal_all_df %>% separate(APL_DISPOSITION_CODE_DESC, " - ",
                into = c("APL_DISPOSITION_CODE", "APL_DISPOSITION_DESC"), 
                remove = TRUE)

appeal_all_df <- appeal_all_df %>% separate(PAYER_ORG, " - ",
                           into = c("PAYER", "PAYER_DESC"), 
                           remove = TRUE)

summary(appeal_all_df)

### Get the summary of all numerical columns

numSummary(appeal_all_df)


### Get the summary of all character columns

charSummary(appeal_all_df)

##Feature Engineering
#As payments and adjustments are equivalent transactions to close account sum them up
appeal_all_df <- mutate(appeal_all_df, TOTAL_PAY_ADJ_AMT = ifelse((is.na(TOTAL_PAYMENTS) & is.na(TOTAL_ADJUSTMENTS)), NA, (coalesce(TOTAL_PAYMENTS, 0.0) + coalesce(TOTAL_ADJUSTMENTS,0.0))))

appeal_all_df <- mutate(appeal_all_df, PAY_ADJ_AMT_AFTER_APPEAL = ifelse((is.na(PAYMENTS_AFTER_APPEAL) & is.na(ADJUSTMENTS_AFTER_APPEAL)), NA, (coalesce(ADJUSTMENTS_AFTER_APPEAL, 0.0) + coalesce(ADJUSTMENTS_AFTER_APPEAL,0.0))))

appeal_all_df <- mutate(appeal_all_df, FINAL_DENIAL_AMOUNT = ifelse((is.na(PRE_APPEAL_DENIAL_AMOUNT) & is.na(POST_APPEAL_DENIAL_AMOUNT)), NA, (coalesce(PRE_APPEAL_DENIAL_AMOUNT, 0.0) + coalesce(POST_APPEAL_DENIAL_AMOUNT,0.0))))


appeal_all_df <- mutate(appeal_all_df, DAYS_TO_FIRST_PAY_ADJ_AFT_APPEAL = pmin(DAYS_TO_FIRST_PAYMENT_AFT_APPEAL,DAYS_TO_FIRST_ADJUSTMENT_AFT_APPEAL, na.rm=TRUE))
appeal_all_df <- mutate(appeal_all_df, DAYS_TO_LAST_PAY_ADJ_AFT_APPEAL = pmax(DAYS_TO_LAST_PAYMENT_AFT_APPEAL,DAYS_TO_LAST_ADJUSTMENT_AFT_APPEAL, na.rm=TRUE))

str(appeal_all_df)




#appeal_all_df$POST_APPEAL_CA_AMOUNT <- coalesce(appeal_all_df$POST_APPEAL_CA_AMOUNT,0)
#appeal_all_df$POST_APPEAL_DENIAL_AMOUNT <- coalesce(appeal_all_df$POST_APPEAL_DENIAL_AMOUNT,0)
#appeal_all_df$DAYS_TO_CLOSE_ACCOUNT <- coalesce(as.numeric(appeal_all_df$DAYS_TO_CLOSE_ACCOUNT),-1)

x <- numSummary(appeal_all_df)
x


#Filter the data  

#Appeal should be opened after discharge  - remove the outliers
appeal_all_df <- appeal_all_df %>% filter(DAYS_APPEAL_AFTER_DISCHARGE > 0)
appeal_all_df <- appeal_all_df %>% filter(   between (FINAL_DENIAL_AMOUNT, -10000, 10000))

appeal_all_df <- appeal_all_df %>% filter(COVERED_CHARGES <80000)
str(appeal_all_df)
summary(appeal_all_df)

#check categorical data
table(appeal_all_df$IP_OP_IND)

table(appeal_all_df$APL_DISPOSITION_CODE)  
table(appeal_all_df$PAYER)






#filter certain denial codes
dropDenial <- c("DNL02", "DNL03", "DNL04","DNL05","DNL08","DNL16","DNL17","DNL18")
dropDenial
dropPayer <- c("TRICARE","Humana","Unicare","Workers Comp","Coventry", "Self Pay","Medicaid", "Medicare")
appeal_all_df <- appeal_all_df %>% filter(!APL_DISPOSITION_CODE %in% dropDenial)
appeal_all_df <- appeal_all_df %>% filter(!PAYER %in% dropPayer)


# Convert i/p op ind to numeric
appeal_all_df$IP_OP_IND <- as.factor(appeal_all_df$IP_OP_IND)
str(appeal_all_df$IP_OP_IND)
appeal_all_df$IP_OP_IND <-as.numeric(appeal_all_df$IP_OP_IND ) -1



#Drop the columns
colsToBeDropped <- c('DAYS_TO_FIRST_PAYMENT_AFT_APPEAL','DAYS_TO_LAST_PAYMENT_AFT_APPEAL',
                     'DAYS_TO_FIRST_ADJUSTMENT_AFT_APPEAL','TOTAL_PAY_ADJ_AMT',
                     'DAYS_TO_LAST_ADJUSTMENT_AFT_APPEAL','APL_DISPOSITION_DESC','PAYER_DESC',
                    'PAYMENTS_AFTER_APPEAL','ADJUSTMENTS_AFTER_APPEAL'
                    ,'POST_APPEAL_DENIAL_AMOUNT','DAYS_TO_CLOSE_ACCOUNT','PRE_APPEAL_DENIAL_AMOUNT' )

appeal_all_df <- appeal_all_df %>%
  select(-one_of(colsToBeDropped))
str(appeal_all_df)

#Drop the NA columns - there can be other ways of imputing 
appeal_no_na_df <- appeal_all_df %>% drop_na
dim(appeal_no_na_df)

x <- numSummary(appeal_no_na_df)
x

boxplot(appeal_no_na_df$FINAL_DENIAL_AMOUNT)

y <- appeal_no_na_df$FINAL_DENIAL_AMOUNT[appeal_no_na_df$FINAL_DENIAL_AMOUNT <= 0]
str(y)

z <- appeal_no_na_df$FINAL_DENIAL_AMOUNT[appeal_no_na_df$FINAL_DENIAL_AMOUNT >50000]
str(z)


ggplot(appeal_no_na_df, aes(x=FINAL_DENIAL_AMOUNT)) +geom_histogram(bins=10)# + scale_x_log10()
ggplot(appeal_no_na_df, aes(x=FINAL_DENIAL_AMOUNT)) + geom_density()  + scale_x_log10()
qqnorm(
  y = appeal_all_df$FINAL_DENIAL_AMOUNT
)

#Add result column - make it the first column 

appeal_no_na_df <- appeal_no_na_df %>% mutate( SUCCESS = ifelse(FINAL_DENIAL_AMOUNT <= 0, 1, 0) )




appeal_renamed_df <- setnames(appeal_no_na_df, old=c("APL_DISPOSITION_CODE","IP_OP_IND","APPEALED_AMT", 'COVERED_CHARGES', "TOTAL_PAYMENTS","LENGTH_OF_STAY",
                     "TOTAL_ADJUSTMENTS","DAYS_APPEAL_AFTER_DISCHARGE",
                     "POST_APPEAL_CA_AMOUNT","PAY_ADJ_AMT_AFTER_APPEAL",
                     "FINAL_DENIAL_AMOUNT","DAYS_TO_FIRST_PAY_ADJ_AFT_APPEAL","DAYS_TO_LAST_PAY_ADJ_AFT_APPEAL", "SUCCESS", "PAYER"),
         new=c("code","io", "aplamt","covchg","totpay","los", "totadj","openday","postca",
"aplapayadj","finden","fpadj","lpadj","success","payer"),skip_absent=TRUE)
#appeal_no_na_df %>% rename(IP_OP_IND = )
str(appeal_renamed_df)


counts <- appeal_renamed_df %>% group_by(success,io) %>% tally()
counts

pairs(appeal_no_na_df[,4:14])

#Subset the data and get ~2000 rows for performance
counts <- appeal_renamed_df %>% group_by(success,io,code,payer) %>% tally()
counts
appeal_subset_df <- appeal_renamed_df %>% group_by(success,io) %>% sample_frac(size = 0.25) 
dim(appeal_subset_df)

summary(appeal_subset_df)

counts <- appeal_subset_df %>% group_by(success,io) %>% tally()
counts

appeal_ordered_df<- appeal_subset_df %>% select("success", "code", "payer","io", "aplamt","covchg","totpay","los", "totadj","openday","postca",
                              "aplapayadj","fpadj","lpadj") 

str(appeal_ordered_df)
numSummary(appeal_ordered_df)
table(appeal_ordered_df$success)

write.csv(appeal_ordered_df,file="dataset/Appeal_final_data.csv",row.names=FALSE, na="")



