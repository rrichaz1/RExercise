####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

#Marc d. Paradis
#R 3.5.1 for Windows
#RStudio 1.1.456

# one time setup
#install.packages('bnlearn')
require(bnlearn)

wrkng.path <- 
  'C:/Users/mparadis/Desktop/DSU/Immersion College/02 MLM/07. Probabilistic Networks/' 
#note you have to change backslashes (\) to forward slashes (/)

##### cancer example

cnet <- 
  read.bif(
    paste(
      wrkng.path,
      'cancer.bif.gz',
      sep = ''
    )
    )

cnet

cpquery( # Conditional Probability Query
  fitted = cnet, 
  event = (
    Cancer == 'True' # What is the conditional probability that cancer is true
    ),
  evidence = (
    Dyspnoea == 'True' # Given that dyspnoea is true
    )
  ) # p(Cancer | Dyspnoea)

cpquery(
  cnet, 
  event = (
    Cancer == 'True'
    ),
  evidence = (
    (Dyspnoea == 'True') & (Smoker == 'True')
    )
  ) # p(Cancer | Dyspnoea & Smoker)

cpquery(
  cnet, 
  event = (
    Dyspnoea == 'True'
    ),
  evidence = (
    (Cancer == 'True') & (Smoker == 'True')
    )
  ) # p(Dyspnoea | Cancer & Smoker)

##########################
data('learning.test')

str(learning.test)

head(learning.test)

pdag <- 
  iamb(
    learning.test
    )

#partial dag
pdag

pdag$arcs

# Note single undirected arc (A <-> B). This is a violation of Bayesian 
# Networks! How can this be? Because iamb doesn't have enough information / 
# signal in the dataset to determine the direction of the arc
score(
  set.arc(
    pdag, 
    from = 'A', 
    to = 'B'
    ), 
  learning.test
  )

score(
  set.arc(
    pdag, 
    from = 'B', 
    to = 'A'
  ), 
  learning.test
)

(
  dag <- 
    set.arc(
      pdag, 
      from = 'B', 
      to = 'A'
      )
  )

fit <- 
  bn.fit(
    dag, 
    learning.test
    )

options(
  digits = 3
  )

fit

evidence.AaFb <- 
  data.frame(
    A = factor( # encode a vector or scalar as a factor
      x = 'a', # a scalar
      levels = levels(learning.test$A) # this is an optimization step not
#                                      # necessary to create this df. If levels
#                                      # is not specified, unique() is called
#                                      # which can be slow for large x
      ),
    F = factor(
      x = 'b', 
      levels = levels(learning.test$F)
      )
    )

predicted.AaFb.blw <- 
  predict( # predict.bn.fit
    object = fit, 
    node = 'C', 
    data = evidence.AaFb, 
    method = 'bayes-lw', 
    prob = TRUE
    )

attr( # get or set specific attributes of an object, x
  x = predicted.AaFb.blw, # an object whose attributes are to be accessed
  which = 'prob' # the attribute to be accessed
  )

predicted.AaFb.par <- 
  predict(
    object = fit, 
    node = 'C', 
    data = evidence.AaFb, 
    method = 'parents', 
    prob = TRUE
    )

attr( # get or set specific attributes of an object, x
  x = predicted.AaFb.par, 
  which = 'prob'
  )

evidence.AcFb <- 
  data.frame(
    A = factor(
      'c', 
      levels = levels(learning.test$A)
      ),
    F = factor(
      'b', 
      levels = levels(learning.test$F)
      )
    )

predicted.AcFb.blw <- 
  predict(
    object = fit, 
    node = 'C', 
    data = evidence.AcFb, 
    method = 'bayes-lw', 
    prob = TRUE
    )

attr(
  x = predicted.AcFb.blw, 
  which = 'prob'
  )

predicted.AcFb.par <- 
  predict(
    object = fit, 
    node = 'C', 
    data = evidence.AcFb, 
    method = 'parents', 
    prob = TRUE
    )

attr(
  x = predicted.AcFb.par, 
  which = 'prob'
  )

# Ok, so none of those probabilities changed very much, which is not surprising
# given the Bayesian network and what we chose as evidence. Let's see what
# happens to conditional probabilities in node E when we take into account new
# evidence about both of node E's parents (node B and node F)

evidence.BaFa <- 
  data.frame(
    B = factor(
      'a', 
      levels = levels(learning.test$B)
    ),
    F = factor(
      'a', 
      levels = levels(learning.test$F)
    )
  )

predicted.BaFa.blw <- 
  predict(
    object = fit, 
    node = 'E', 
    data = evidence.BaFa, 
    method = 'bayes-lw', 
    prob = TRUE
  )

attr(
  x = predicted.BaFa.blw, 
  which = 'prob'
)

nodeE <- fit[5]

nodeE[[1]]$prob

# Use more sophiticated methods to find strength of each arc, this approach is
# taking the observed data and trying to infer the most likely Bayesian Network
# estimating both likelihood of arc existence (strength) as well as likelkihood
# of arc direction interpreted as 0.5 = bidirectional, close to 1 being from ->
# to, close to zero being from <- to.

arcs = 
  boot.strength( # Measures the strength of probabilistic relationships (arcs)
#                # in a Bayesian network
    learning.test, 
    algorithm = 'hc' # 'gs', 'iamb', 'fast.iamb', 'inter.iamb', 'mmpc', 'tabu',
#                    # 'mmhc', 'rsmax2' are all estimation methods
    )

arcs

arcs[(arcs$strength > 0.85) & (arcs$direction >= 0.5), ]

averaged.network(arcs) # Returns a completely directed graph

#END