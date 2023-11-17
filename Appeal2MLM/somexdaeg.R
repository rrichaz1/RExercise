library(lsr)# for CramersV
counts <- table(num, fbs)
barplot(counts, main=" Distribution by fbs and num",
        xlab="fbs", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
chisq.test(counts,p=0.05)
cramersV(counts)


### for some random errors
dev.off()

library(gapminder)
library(dplyr)
library(ggplot2)
