####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

#Marc d. Paradis
#R 3.5.1 for Windows
#RStudio 1.1,456

#install.packages('depmixS4')
require(depmixS4)

require(ggplot2)

data('speed')

str(speed)

head(speed)

tail(speed)

# Note that this dataframe has NO information about network structure, so it is
# a Hidden Markov Model (HMM).
# rt = response time (continuous)
# corr = accuarcy score (binary)
# Pacc = relative pay-off for accurate responding, from 0 to 1, varied during
# the experiment ('treatment'). In theory the subject should pick up on this
# change over time and the switch their state
# prev = accuracy score on previous trial (binary covariate)

set.seed(1)

plot(speed)

plot(
  x = speed$Pacc[1:168],
  ylim = 0:1
  )

plot(
  x = speed$Pacc[169:302],
  ylim = 0:1
  )

plot(
  x = speed$Pacc[303:439],
  ylim = 0:1
  )

plot(
  x = speed$Pacc[1:168],
  y = speed$rt[1:168],
  xlim = 0:1
)

plot(
  x = speed$Pacc[169:302],
  y = speed$rt[169:302],
  xlim = 0:1
)

plot(
  x = speed$Pacc[303:439],
  y = speed$rt[303:439],
  xlim = 0:1
)

plot(
  x = speed$Pacc[1:168],
  y = speed$corr[1:168],
  xlim = 0:1
)

plot(
  x = jitter(
    x = speed$Pacc[1:168],
    factor = 1.5,
    amount = NULL
    ),
  y = speed$corr[1:168],
  xlim = 0:1
)


plot(
  x = jitter(
    x = speed$Pacc[169:302],
    factor = 1.5,
    amount = NULL
    ),
  y = speed$corr[169:302],
  xlim = 0:1
)

plot(
  x = jitter(
    x = speed$Pacc[303:439],
    factor = 1.5,
    amount = NULL
    ),
  y = speed$corr[303:439],
  xlim = 0:1
)

# The outcome is 2 variables, response time and accuracy. Response is Gaussian, 
# accuracy is multinomial. 
hmm <- 
  depmix(
    response = list( # Responses to be modeled, the observed variables
      rt ~ 1,
      corr ~ 1
      ),
    data = speed,
    transition = ~ 1, # Transitions are modeled as a multinomial logistic model
#                     # for each state. ~1 means the Transitions are stable.
    nstates = 2, # Assume 2 states for the test taker (fast or accurate)
    family = list( # Must be in same order as response parameter
      gaussian(), # Asserted distribution for first state S1 (rt)
      multinomial('identity') # Asserted distribution for second state S2 (corr)
      ),
    ntimes = c( # Sets the number of samples and trials for each
      168, # Sample 1 for Subject 1. First 168 rows. 168 Trials
      134, # Sample 2 for subject 1. Rows 169 - 302. 134 Trials
      137 # Sample 3 for subject 1. Rows 303 - 439. 137 Trials
      )
    )

# THIS IS NOT A FITTED MODEL YET. Have to run fit() on the HMM object in order
# to create the fitted model.

fhmm <- 
  fit(
    object = hmm
    )

fhmm

summary(fhmm)

## add covariate that impacts transition matrix
hmm2 <- 
  depmix(
    response = list(
      rt ~ 1,
      corr ~ 1
      ), 
    data = speed, 
    nstates = 2,
    family = list(
      gaussian(), 
      multinomial('identity')
      ),
    transition = ~ scale(Pacc), # this says changing Pacc may be changing
#                               # transition matrix, so control for this in 
#                               # estimating the hidden transition matrix
    ntimes = c(
      168,
      134,
      137
      )
    )

fhmm2 <- 
  fit(
    object = hmm2
    )

fhmm2

summary(fhmm2)

hmm3 <- 
  depmix(
    response = list(
      rt ~ 1,
      corr ~ 1
    ), 
    data = speed, 
    nstates = 2,
    family = list(
      gaussian(), 
      multinomial('identity')
    ),
    transition = ~ prev,
    ntimes = c(
      168,
      134,
      137
    )
  )

fhmm3 <- 
  fit(
    object = hmm3
  )

fhmm3

summary(fhmm3)

hmm4 <- 
  depmix(
    response = list(
      rt ~ 1,
      corr ~ 1
    ), 
    data = speed, 
    nstates = 2,
    family = list(
      gaussian(), 
      multinomial('identity')
    ),
    transition = ~ scale(Pacc) + prev,
    ntimes = c(
      168,
      134,
      137
    )
  )

fhmm4 <- 
  fit(
    object = hmm4
  )

fhmm4

summary(fhmm4)

fp1 <- 
  posterior(fhmm)

fp2 <- 
  posterior(fhmm2)

fp3 <- 
  posterior(fhmm3)

fp4 <- 
  posterior(fhmm4)

idx.12 <- # this is a logical vector, TRUE where fp1 and fp2 differ, else FALSE
  fp1[ ,1] != fp2[ ,1]

which( # which index values are TRUE
  x = idx.12
  )  

fp2[which(idx.12), ]

fp1[which(idx.12), ]

idx.14 <- # this is a logical vector, TRUE where fp1 and fp4 differ, else FALSE
  fp1[,1] != fp4[ ,1]

which( # which index values are TRUE
  x = idx.14
)  

head(
  x = fp4[which(idx.14), ],
  n = 15L
  )

head(
  x = fp1[which(idx.14), ],
  n = 15L
  )

# So which model is Correct? which model is True?
# Bring in the expert to help. It is not an easy question to answer.
# But at the end of the day, it is which model is most "useful" - whether that
# is use in making monetizable predictions or use in understanding a system or
# use in some other sense.

#END