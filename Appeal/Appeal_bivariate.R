# library for exploratory data analysis
install.packages("devtools")
devtools::install_github("ujjwalkarn/xda")
install.packages("tidyverse")
install.packages("readr")
require("tidyverse")
require("readr")
require(xda)

appeal_df <- read_csv("/Users/singhr/Documents/testcode/R_Exercise/Appeal/Appeal_xdapost.csv")
summary(appeal_df)
str(appeal_df)

day_appeal_lt200 <- appeal_df %>% filter(days_appeal_opened_after_discharge < 300)
ggplot(day_appeal_lt200, aes(x = as.factor(is_payment_received_after), y = days_appeal_opened_after_discharge)) + geom_boxplot()



appeal_pay_df <- appeal_df %>% filter(Days_Payment_after_appeal > 0 & !is.na(days_appeal_opened_after_discharge))
appeal_nopay_df <- appeal_df %>% filter(is_payment_received_after == 0)



ggplot(data=appeal_pay_df, aes(x=days_appeal_opened_after_discharge, y = Days_Payment_after_appeal)) +
  geom_point() + scale_x_continuous("Appeal open after discharge in days - x") + scale_y_continuous("Payment after appeal in days - y")
ggplot(data=appeal_pay_df, aes(x=(days_appeal_opened_after_discharge), y = (Days_Payment_after_appeal))) +
  geom_point(alpha=0.5, position="jitter") + # + scale_x_continuous("Appeal open after discharge in days - x") + scale_y_continuous("Payment after appeal in days - y")
  scale_x_log10() + scale_y_log10() # coord_trans(x = "log10", y = "log10")

#Discretize
ggplot(data=appeal_pay_df, aes(x=cut(days_appeal_opened_after_discharge, breaks=10), y = Days_Payment_after_appeal)) +
  geom_boxplot() 
day_appeal_lt200 <- appeal_pay_df %>% filter(days_appeal_opened_after_discharge < 200)
ggplot(data=day_appeal_lt200, aes(x=days_appeal_opened_after_discharge, y = Days_Payment_after_appeal)) +
  geom_point()
ggplot(data=day_appeal_lt200, aes(x=cut(days_appeal_opened_after_discharge, breaks=5), y = Days_Payment_after_appeal)) +
  geom_boxplot() 

day_appeal_lt200
day_opn_cut <- (cut(appeal_pay_df$days_appeal_opened_after_discharge, breaks=10))
pay_cut <- (cut(appeal_pay_df$Days_Payment_after_appeal, breaks=10))
table(day_opn_cut,pay_cut)
table(day_opn_cut)
str(day_opn_cut)
summary(day_opn_cut)
ggplot(appeal_pay_df, aes(x=cut(days_appeal_opened_after_discharge, breaks=10))) +geom_bar()

ggplot(data=appeal_pay_df, aes(x=cut(days_appeal_opened_after_discharge, breaks=10), 
                               y = cut(Days_Payment_after_appeal, breaks = 10))) +
  geom_histogram()


day_opn_pay_cut <- (cut(appeal_pay_df$days_appeal_opened_after_discharge, breaks=10))
day_opn_nopaycut <- (cut(appeal_nopay_df$days_appeal_opened_after_discharge, breaks=10))

table(day_opn_pay_cut)
table(day_opn_nopaycut)

#Pearson coefficient for payment day vs appeal open days

appeal_pay_df %>%
  summarize(N = n(), r = cor(Days_Payment_after_appeal, days_appeal_opened_after_discharge, use = "pairwise.complete.obs"))


res <- cor.test(appeal_pay_df$days_appeal_opened_after_discharge, appeal_pay_df$Days_Payment_after_appeal, 
                method = "pearson")
res

library(ggpubr)
ggqqplot(appeal_df$days_appeal_opened_after_discharge)



