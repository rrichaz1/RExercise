install.packages("data.table")
library("data.table")

install.packages("tidyverse")
install.packages("readr")
require("tidyverse")

require("readr")

perf_df <- fread("/Users/singhr/Documents/testcode/R_Exercise/Performance/export_upmc_spark_pas.csv")
#perf_df <- fread("/Users/singhr/Documents/testcode/R_Exercise/Performance/upmcpre.csv")
perf_df$QUEUE_DATE <- as.Date(perf_df$QUEUE_DATE, format="%d-%b-%y")
str(perf_df)
summary(perf_df)
perf_df_rm_outlier <- perf_df %>% filter(MAX_STEP_TIME > 0 & MAX_STEP_TIME < 150)

ggplot(perf_df_rm_outlier, aes(QUEUE_DATE, MAX_STEP_TIME,color=MAX_STEP_TIME)) +geom_line() +
   xlab("Job Date") + ylab("Max Pre Time in min")

perf_df_rm_outlier <- perf_df %>% filter(MAX_STEP_TIME > 0 & MAX_STEP_TIME < 400)
summary (perf_df_rm_outlier)
ggplot(perf_df_rm_outlier, aes( MAX_STEP_TIME,color=MAX_STEP_TIME)) +
  geom_histogram(binwidth = 25) 

df <- data.frame(c("22-Aug-17", "23-Aug-17"))
head(df)
str(df)
#calculate standard deviation
sd(perf_df_rm_outlier$MAX_STEP_TIME)


# plotting 2 on same 
ggplot(perf_df,aes(VOL))+
 # geom_line(aes(y=TOTAL_SPARK_TIME,color="TOTAL_SPARK_TIME"))+
geom_line(aes(y=TOTAL_SPARK_MERGE_TIME,color="TOTAL_SPARK_MERGE_TIME")) +
  ylab("Total job time") + xlab("Volume")



perf_df_plsql <- fread("/Users/singhr/Documents/testcode/R_Exercise/Performance/export_upmc_plsql_pas.csv")
str(perf_df_plsql)

ggplot(perf_df_plsql,aes(VOL))+
  # geom_line(aes(y=TOTAL_SPARK_TIME,color="TOTAL_SPARK_TIME"))+
  geom_line(aes(y=JOB_TIME,color="JOB_TIME")) +
  ylab("Total job time") + xlab("Volume")

