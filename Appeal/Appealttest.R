require("tidyverse")

appeal_df <- read.csv("/Users/singhr/Documents/testcode/R_Exercise/Appeal/Appeal_xdapost.csv")
summary(appeal_df)

#t tests
#Exclude ouliers
#appeal_grouped <- appeal_df %>% filter(days_appeal_opened_after_discharge < 1000) %>% group_by(is_payment_received_after) 
#sample_df <- sample_frac(appeal_grouped, 0.10)
 # appeal_subset_df <- select (sample_df,  days_appeal_opened_after_discharge,is_payment_received_after)
  #write.csv(appeal_subset_df,file="/Users/singhr/Documents/testcode/R_Exercise/Appeal/Appeal_samplettest.csv",row.names=FALSE, na="")  
#str(appeal_subset_df)  
#sample_1000 <- appeal_subset_df %>% group_by(is_payment_received_after)  %>% sample_n(1000) 


  appeal_grouped <- appeal_df %>% filter(days_appeal_opened_after_discharge < 250) %>% group_by(is_payment_received_after, cust_ipop) 
  sample_df <- sample_frac(appeal_grouped, 0.10)
  str(sample_df)
  table(sample_df$is_payment_received_after, sample_df$IP_OP_IND)
  table(sample_df$cust_ipop, sample_df$is_payment_received_after)
  sample_1000 <- select (sample_df,  days_appeal_opened_after_discharge,is_payment_received_after)
  
  str(sample_1000)
  
 
  sample_1000 %>% group_by(is_payment_received_after) %>%   summarise(count = n(), mean = mean(days_appeal_opened_after_discharge, na.rm=TRUE),
              sd = sd(days_appeal_opened_after_discharge, na.rm=TRUE))
install.packages("ggpubr")
library("ggpubr")

ggboxplot(sample_1000_eq, x = "is_payment_received_after", y = "days_appeal_opened_after_discharge", 
          color = "is_payment_received_after", palette = c("#00AFBB", "#E7B800"),
          ylab = "Appeal Opened after days", xlab = "Payment Received or Not")
#Shapiro-Wilk normality test  for payment not received
with(sample_1000, shapiro.test(days_appeal_opened_after_discharge[is_payment_received_after == 0]))
#Shapiro-Wilk normality test  for payment  received
with(sample_1000, shapiro.test(days_appeal_opened_after_discharge[is_payment_received_after == 1]))

#Compute two-samples Wilcoxon test
res <- wilcox.test(days_appeal_opened_after_discharge ~ is_payment_received_after, data = sample_1000, exact=FALSE)
res#var.test for homogeneity

res.ftest <- var.test(days_appeal_opened_after_discharge ~ is_payment_received_after, data = sample_1000)
res.ftest


res.ttest <- t.test(days_appeal_opened_after_discharge ~ is_payment_received_after, data = sample_1000, var.equal = TRUE)
res.ttest

appeal_grouped <- appeal_df %>% filter(days_appeal_opened_after_discharge < 250) %>% group_by(is_payment_received_after, ip_op_num) 
sample_1000_eq <- sample_n(appeal_grouped, 500) %>% select ( days_appeal_opened_after_discharge,is_payment_received_after)
table(sample_1000_eq$is_payment_received_after)
res.ttest <- t.test(days_appeal_opened_after_discharge ~ is_payment_received_after, data = sample_1000_eq, var.equal = TRUE)
res.ttest
sample_1000_eq %>% group_by(is_payment_received_after) %>%   summarise(count = n(), mean = mean(days_appeal_opened_after_discharge, na.rm=TRUE),
                                                                    sd = sd(days_appeal_opened_after_discharge, na.rm=TRUE))

# Compute the analysis of variance
res.aov <- aov(days_appeal_opened_after_discharge ~ as.factor(is_payment_received_after), data = sample_1000)
# Summary of the analysis
summary(res.aov)
TukeyHSD(res.aov)

