####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

#Marc d. Paradis
#R 3.5.1 for Windows
#RStudio 1.1.456


# hypothesis_testing.R
# Created by Yezhou Sun

# Test mean of large sample

# One sample two sided test
# Sample mean = 18500, sample standard deviation = 15000, sample size = 1000.
# Test if sample mean consistent with a population mean of 20000.
# Null Hypothesis: mean of population from which sample is drawn = 20000
z01 <- 
  (18500 - 20000) / (15000 / sqrt(1000))

# z-score = (x.bar - mu) / (s / sqrt(n))

p01 <- 
  2 * pnorm(-abs(z01))

# p < 0.05, reject null hypothesis. The mean of population from which sample 
# was drawn is not likely to be equal to 20000.

# Unpaired Two sample two sided test
# Sample 1 mean = 12780, sample 1 variance = 8740, sample 1 size = 137000
# Sample 2 mean = 14270, sample 2 variance = 6580, sample 2 size = 78000
# Test if two sample means are equal.
# Null Hypothesis: Sample 1 and Sample 2 are drawn from the same population
z02 <- 
  (12780 - 14270) / sqrt((8740 / 137000) + (6580 / 78000))

p02 <- 
  2 * pnorm(-abs(z02)) # Why do you think the p-value = 0?

# p < 0.05, reject null hypothesis. The two samples are drawn from different 
# populations with different means.

# Test mean of small sample: Student's t test

# One sample two sided test
# Test if sample mean equals a specific value
# Null Hypothesis: the sample s01 was drawn from a population with a mean of 75

s01 <- 
  c(65, 78, 88, 55, 48, 95, 66, 57, 79, 81) # The observed sample

mean(s01) # mean = 71.2

t.test(
  x = s01, # non-empty, numeric vector
  alternative = 'two.sided', # the Alternative Hypothesis, Ha
  mu = 75 # Because one sample test, this is proposed "true" population mean 
  )

# p > 0.05, fail to reject null hypothesis. The mean of population from which 
# sample was drawn could be equal to 75.

# Two sample two sided test
# Test if two sample means are equal.

s02 <- 
  c(175, 168, 168, 190, 156, 181, 182, 175, 174, 179)

s03 <- 
  c(185, 169, 173, 173, 188, 186, 175, 174, 179)

# First test if two variances are equal using F test.

var.test(
  x = s02, 
  y = s03
  )

# p > 0.05, can't reject null hypothesis. The two observed samples could have
# been drawn from either the same population or from two populations with the
# same variance.

# t test using equal variance

t.test(
  x = s02, 
  y = s03, 
  alternative = 'two.sided', 
  var.equal = T, # b/c var.test p-value > 0.05, var.equal = T
  paired = F, 
  conf.level = 0.95
  )

# p > 0.05, fail to reject null hypothesis. Populations from which two samples 
# are drawn could have same mean.

# Test porportion of large sample

# One sample two sided test

# A sample of 37,000 patients where 8.7 percent of patients have a 
# hospitalization.

# Test if true proportion equals 10% (0.1)
# Null Hypothesis: Population proportion of patients hospitalized = 10%

prop.test(
  x = 37000 * 0.087, # Estimated number of observed successes 
  n = 37000, # Number of units
  p = 0.1, # Proposed true proportion or rate of success
  alternative = 'two.sided', 
  conf.level = 0.95, 
  correct = F
  )

# p << 0.05, reject null hypothesis. Proportion of patients hospitalized is not
# equal to 0.1.

# Two sample two sided test
# Test if proportions of smokers in males and females are same.

smoker <- 
  matrix(
    c(70, 120, 65, 140), 
    ncol = 2, 
    byrow = T
    )

rownames(smoker) <- 
  c('male', 'female')

colnames(smoker) <- 
  c('smoke', 'nosmoke')

smoker

# Null Hypothesis: Proportion of smokers identical in males and females

prop.test(
  x = smoker, # rows are two samples, columns are two proportions
  alternative = 'two.sided', 
  conf.level = 0.95,
  correct = T
  )

# p > 0.05, can't reject null hypothesis. Proportions of smokers in males and 
# females could be the same.

# Chi-squared test

# Test if a treatment improves patient's condition.

treatment <- 
  matrix(
    c(26, 29, 35, 15), 
    ncol = 2, 
    byrow = T
    )

colnames(treatment) <- 
  c('improved', 'not_improved')

rownames(treatment) <- 
  c('not_treated', 'treated')

treatment

# Null Hypothesis: Proportion of Improved patients is identical in treated and
# non-treated patients

chisq.test(
  x = treatment, 
  correct = F
  )

# p < 0.05, reject null hypothesis. Treatment could improve patient's 
# condition.

# Our smoker dataset can be viewed either as continuous proportions (and
# therefore tested via proportions based on the normal distribution) or as 
# counts (and therefore tested via the chi-squared distribution).

chisq.test(
  x = smoker, 
  correct = F
  )

# p > 0.05 therefore fail to reject null hypothesis. Number of smokers and
# non-smokers does not differ between counts of males and females.

# Fisher's exact test

# Test if job satisfaction depends on income level.

job <- 
  matrix(
    c(1, 2, 1, 0, 3, 3, 6, 1, 10, 10, 14, 9, 6, 7, 12, 11),
    4,
    4,
    dimnames = list(
      income = c(
        '< 15k', 
        '15-25k', 
        '25-40k', 
        '> 40k'
        ),
      satisfaction = c(
        'VeryD', 
        'LittleD', 
        'ModerateS', 
        'VeryS'
        )
      )
    )

job

fisher.test(
  x = job,
  conf.level = 0.95
  )

# P > 0.05, can't reject null hypothesis. Job satisfaction doesn't depend on 
# income level.

# Correlation test for ordinal variables

# Test the correlation between the level of productivity of a machine and the 
# level of satisfaction of operator using Kendall's tau (a non-parametic test).

productivity <- 
  c(5, 7, 9, 9, 8, 6, 4, 8, 7, 7)

satisfaction <- 
  c(6, 7, 4, 4, 8, 7, 3, 9, 5, 8)

cor.test(
  x = productivity, 
  y = satisfaction, 
  method = 'kendall', # 'spearman' also non-parametric, 'pearson' is parametric
  alternative = 'two.sided'
  )

# Kendall's tau is low and not significant indicated by p-value > 0.05. There
# is no correlation between productivity and satisfaction.

rm(
  list = ls()
  ) # Clears global environment of all visible objects (some hidde objects may
    # remain)

# END
