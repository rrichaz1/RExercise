# Up and Running with R
# Ex06_05
# Comparing means with ANOVA

# Load data file about Google searches by state
google <- read.csv("google_correlate.csv", header = T)
names(google)

# One Way Anova
anova1 <- aov(data_viz ~ region, data = google)
summary(anova1)

# Two Way Factorial Design 
# First way to specify
anova2a <- aov(data_viz ~ 
               region + stats_ed + region:stats_ed, 
               data = google)
summary(anova2a)

# Second way to specify
anova2b <- aov(data_viz ~ 
               region*stats_ed, 
               data = google)
summary(anova2b)