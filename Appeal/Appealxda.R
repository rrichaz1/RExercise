install.packages("devtools")
devtools::install_github("ujjwalkarn/xda")
install.packages("data.table")
install.packages('bit64')


install.packages("tidyverse")  #tidyerse has ggplot2 and dplyr
install.packages("readr")


library("data.table")  # has fread
library("ggplot2")
require("tidyverse")

require("readr")

library(xda)
library(lubridate)

appeal_all_df <- fread("/Users/singhr/Documents/testcode/R_Exercise/Appeal/Appeal_xdapre.csv")
str(appeal_all_df)
summary(appeal_all_df)




appeal_payGT0_df <-  appeal_all_df %>% filter(PAYMENTS_AFTER_APPEAL >= 0)
summary(appeal_payGT0_df)


appeal_denial_df <- appeal_all_df %>% filter (!(APL_APPEAL_CODE_DESC %like% 'Audit')) %>%
  filter(!(APL_APPEAL_CODE_DESC %like% 'payment')) %>%
  filter(!(APL_APPEAL_CODE_DESC %like% 'Error')) %>%
  filter (!(APL_APPEAL_CODE_DESC == '-'))
table(appeal_denial_df$APL_APPEAL_CODE_DESC)
str(appeal_df)  #30978
str(appeal_denial_df) #28736
appeal_df <- appeal_denial_df

ip_op_factor <- factor(c("I","O"))
appeal_df$IP_OP_IND <- factor(appeal_df$IP_OP_IND, levels=ip_op_factor, ordered=TRUE)
levels(appeal_df$IP_OP_IND)
as.numeric(appeal_df$IP_OP_IND)

appeal_pay_df <- appeal_df %>% filter(PAYMENTS_AFTER_APPEAL > 0)
str(appeal_pay_df) #17269
summary(appeal_pay_df)

numSummary(appeal_df)
charSummary(appeal_df)
#table(appeal_df$PAYER_NAME)

ggplot(appeal_pay_df, aes(x=Days_Payment_after_appeal)) +geom_histogram(bins=40) + scale_x_log10()
ggplot(appeal_pay_df, aes(x=IP_OP_IND, y=Days_Payment_after_appeal)) +geom_boxplot()

ggplot(appeal_df, aes(x=days_appeal_opened_after_discharge)) +
  geom_histogram(bins=40) + scale_x_log10() + facet_wrap(~is_payment_received_after)

# Contingency plot -- Inpatient/Outpatient, Payment received
tab_io <- table(appeal_df$IP_OP_IND, appeal_df$is_payment_received_after)
tab_io
prop.table(tab_io)
summary(prop.table(tab_io))
summary(tab_io)
sum(prop.table(tab_io))

ggplot(appeal_df, aes(x=is_payment_received_after, fill = IP_OP_IND)) +
  geom_bar(position="dodge") + xlab("Payment received")
  #geom_bar(position="fill") + xlab("Payment received")
ggplot(appeal_df, aes(x=IP_OP_IND, fill = as.factor(is_payment_received_after)) )+
  geom_bar(position="dodge") + xlab("Payment Inpatient Outpatient") +ylab("Payment received")
table(appeal_df$IP_OP_IND)
prop.table(table(appeal_df$IP_OP_IND))
prop.table(table(appeal_df$is_payment_received_after))

ggplot(appeal_df, aes(x = as.factor(is_payment_received_after), y = days_appeal_opened_after_discharge)) + geom_boxplot()
ggplot(appeal_df, aes(x =log(days_appeal_opened_after_discharge),fill =  as.factor(is_payment_received_after))) + 
  geom_density(alpha = .3) + xlab("Appeal opened after days") + facet_wrap(~IP_OP_IND)

ggplot(appeal_df, aes(x =log(Days_Payment_after_appeal))) + 
  geom_density(alpha = .3) + xlab("Payment received after days") + facet_grid(~IP_OP_IND, labeller = label_both)

ggplot(appeal_df, aes(x =log(Days_Payment_after_appeal))) + 
  geom_histogram() + facet_wrap(~IP_OP_IND)+xlab("Payment received after days") 

ggplot(appeal_df, aes(x =log(days_appeal_opened_after_discharge))) + 
  geom_density(alpha = .3) + xlab("Appeal opened after days") + facet_grid(~IP_OP_IND~is_payment_received_after,, labeller = label_both)

#Grouping
payer_group <- appeal_df %>% group_by(PAYER_NAME) %>% 
      summarise(total = n(),
      avg_days =mean(Days_Payment_after_appeal))
str(payer_group)
summary(payer_group)
ggplot(payer_group, aes(x=PAYER_NAME, y=total)) +geom_point() +theme(axis.text.x = element_text(angle = 90))#+scale_y_log10()

#Create a variable by customer,i/p
#appeal_df = transform(appeal_df, cust_ipop = factor(paste(CUSTOMER_NAME,IP_OP_IND,is_payment_received_after)))
appeal_df = transform(appeal_df, cust_ipop = factor(paste(CUSTOMER_NAME,IP_OP_IND)))

appeal_df = transform(appeal_df, ip_op_num =as.numeric(IP_OP_IND))

charSummary(appeal_df)
numSummary(appeal_df)
ggplot(appeal_df, aes(x =log(Days_Payment_after_appeal))) + 
  geom_density(alpha = .3) + xlab("Payment received after days") + facet_grid(~cust_ipop, labeller = label_both)

ggplot(appeal_df, aes(x =log(Days_Payment_after_appeal))) + 
  geom_histogram() + facet_wrap(~cust_ipop)+xlab("Payment received after days") 

ggplot(appeal_df, aes(x = is_payment_received_after, fill = cust_ipop )) +
         geom_bar(position="fill") 

summary(appeal_df)

appeal_close_df <- appeal_df %>% filter(Is_appeal_closed == 1)
tab_cnt_close_pay <- table(appeal_close_df$Is_appeal_closed, appeal_close_df$is_payment_received_after)
prop.table(tab_cnt_close_pay)
table(appeal_df$Is_appeal_closed)
table(appeal_df$APL_APPEAL_CODE_DESC)
appeal_subset_df <- select (appeal_df,CUSTOMER_NAME,Is_appeal_closed,
                            days_appeal_opened_after_discharge,IP_OP_IND,
                            Days_Payment_after_appeal,is_payment_received_after,
                            cust_ipop,ip_op_num,APPEALED_AMT)
str(appeal_subset_df)
write.csv(appeal_subset_df,file="/Users/singhr/Documents/testcode/R_Exercise/Appeal/Appeal_xdapost1.csv",row.names=FALSE, na="")
  