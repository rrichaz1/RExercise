####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

#Marc d. Paradis
#R 3.5.1 for Windows
#RStudio 1.1.456

# discriminant_analysis.R
# Yezhou Sun

# MASS R packages has functions for linear and quadratic 
# discriminant analysis (LDA and QDA)

library(MASS)

# klaR has functions for regularized LDA (RDA) and visualization.

library(klaR)

set.seed(12345)

# Use iris data set 
data('iris')
str(iris)

# Change species name to avoid confusion.
levels(
  iris$Species)[
    levels(iris$Species) == 'setosa'
    ] <- 
  'A'

levels(
  iris$Species)[
    levels(iris$Species) == 'versicolor'
    ] <- 
  'B'

levels(
  iris$Species)[
    levels(iris$Species) == 'virginica'
    ] <- 
  'C'

# Exploratory plots for diferent classification methods. All pairwise
# correlaions are shown. Misclassified samples are labeled in red.
partimat( # klaR
  formula = Species ~., 
  data = iris, 
  method = 'lda' # Linear Discriminant Analysis
)

partimat(
  formula = Species ~., 
  data = iris, 
  method = 'qda' # Quadratic Discriminant Analysis
)

partimat(
  formula = Species ~., 
  data = iris, 
  method = 'rda' # Regularized Discriminant Analysis
)
# Note use of default parameters

partimat(
  formula = Species ~., 
  data = iris, 
  method = 'rpart' # Recursive Partitioning and Regression Trees
)
# Note use of default parameters

partimat(
  formula = Species ~., 
  data = iris, 
  method = 'naiveBayes' # Naive Bayes
)

partimat(
  formula = Species ~., 
  data = iris, 
  method = 'sknn' # Simple k-Nearest Neighbors
) # Slow to plot

# Note use of default parameters 

# LDA
lda.fit1 <- # MASS
  lda(
    formula = Species ~., 
    data = iris
  )

lda.fit1
# There are 2 Linear Discriminants (LD1 and LD2) because this is a three class
# problem. In lecture we only looked at the binary case so there was only a 
# single linear discriminant (LD1)

# Scatterplot using first two linear discriminants.
plot(
  lda.fit1, 
  dimen = 2
)

# Remember that to previously plot in 2D we had to do a pairwise lattice plot
# where for some pairs the separation was ok and for others, not so much.
plot(iris[, -5])

# Histogram and density plots along with first linear discriminant.
# plot(
#  lda.fit1, 
#  dimen = 1, 
#  type = 'both'
#  )


# Make prediction
predict(
  object = lda.fit1, 
  data = iris
)

# Assess model accuracy by cross validation.
lda.fit2 <- 
  lda(
    formula = Species ~ ., 
    data = iris, 
    CV = TRUE # Leave-One-Out Cross Validation
  )

lda.fit2

lda.ct <- 
  table(
    iris$Species, # Observed
    lda.fit2$class # Expected / Predicted
  )

lda.ct

diag( # Extracts the diagonal of a table
  x = prop.table( # Converts counts into frequencies / proportions
    x = lda.ct, 
    margin = 1 # proportion relative to class
  )
)

# Total percent accuracy
sum(
  diag(
    x = prop.table(
      x = lda.ct,
      margin = NULL # proportion relative to total
    )
  )
)

# QDA
qda.fit1 <- 
  qda(
    formula = Species ~., 
    data = iris
  )

qda.fit1

predict(
  object = qda.fit1, 
  data = iris
)

# Assess model accuracy by cross validation.
qda.fit2 <- 
  qda(
    formula = Species ~ ., 
    data = iris, 
    CV = TRUE
  )

qda.fit2

qda.ct <- 
  table(
    iris$Species, # Observed
    qda.fit2$class # Expected / Predicted
  )

qda.ct # Is it better? Is it worse?

diag(
  prop.table(
    x = qda.ct, 
    margin = 1
  )
)

sum(
  diag(
    prop.table(
      x = qda.ct,
      margin = NULL
    )
  )
)

# Regularized Discriminant Analaysis
# In this context RDA is more like elasticNet in that it is a weighted mixture
# of LDA and QDA.

# RDA has two tuning parameters: gamma and lambda. Both take value between 0 and
# 1. Gamma controls the balance between LDA and QDA. Lambda represents the 
# correlation of variables in the dataset.
rda.fit1 <- 
  rda(
    formula = Species ~., 
    data = iris
  )

# rda() as part of its output will find optimal values for gamma and lambda. 
# Please refer to manual for details.
rda.fit1

rda.fit1$regularization[1]

rda.fit1$regularization[2]

predict(
  object = rda.fit1, 
  data = iris
)

# Assess model accuracy. Note that rda() does not have LOOCV like lda() and 
# qda(), and therefore there is no $class object. Instead have to use predict()
# with rda.fit1 object because predict() does have $class object as output.
rda.fit2 <- 
  predict(
    object = rda.fit1, 
    data = iris
  )

rda.fit2

rda.ct <- 
  table(
    iris$Species, # Observed
    rda.fit2$class # Expected / Predicted
  )

rda.ct # Is it better? Is it worse?

diag(
  prop.table(
    x = rda.ct, 
    margin = 1
  )
)

sum(
  diag(
    prop.table(
      x = rda.ct,
      margin = NULL
    )
  )
)

# END