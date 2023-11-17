appeal_df <- read.csv("/Users/singhr/Documents/testcode/R_Exercise/Appeal/Appeal_xdapost.csv")
summary(appeal_df)

#Chisquare tests
table(appeal_df$IP_OP_IND, appeal_df$is_payment_received_after)
chisq.test(as.factor(appeal_df$IP_OP_IND), as.factor(appeal_df$is_payment_received_after))
chisq$observed
chisq$expected


#Test for variation by customer and ip-op
chisq1 <- chisq.test(as.factor(appeal_df$cust_ipop), as.factor(appeal_df$is_payment_received_after))
chisq1$observed
chisq1$expected
chisq1

#two Category test for customer 1
appeal_sf_cust1 <- appeal_df %>% filter(CUSTOMER_NAME == "customer1")
chisq2 <- chisq.test(as.factor(appeal_sf_cust1$IP_OP_IND), as.factor(appeal_sf_cust1$is_payment_received_after))
chisq2$observed
chisq2$expected
chisq2


#two Category test for customer 2
appeal_sf_cust2 <- appeal_df %>% filter(CUSTOMER_NAME == "customer2")
chisq3 <- chisq.test(as.factor(appeal_sf_cust2$IP_OP_IND), as.factor(appeal_sf_cust2$is_payment_received_after))
chisq3$observed
chisq3$expected
chisq3
