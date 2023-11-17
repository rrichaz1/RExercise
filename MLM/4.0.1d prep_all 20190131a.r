####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

#Based on scripts by Chris Hane

require(class)
require(ggplot2)
require(data.table)

wrkng.path <- 'C:/Users/mparadis/Desktop/DSU/Immersion College/02 MLM/04. Supervised Classification/4.1 Distance-Based Classification/'

source(
  paste(
    wrkng.path,
    '4.0.1a prep_HSA_rates 20180926a.r',
    sep = ''
  ),
  echo = FALSE
)

source(
  paste(
    wrkng.path,
    '4.0.1b prep_Cost_rates 20180926a.r',
    sep = ''
  ),
  echo = FALSE
)


source(
  paste(
    wrkng.path,
    '4.0.1c prep_mort 20180926a.R',
    sep = ''
  ),
  echo = FALSE
)


nrow(cost)
nrow(mort)
nrow(pc)

dt <- 
  merge(
    cost, 
    pc, 
    by.x = 'HSA', 
    by.y = 'HSA', 
    all.x = TRUE
    )

dt <- 
  merge(
    dt, 
    mort, 
    by.x = 'HSA', 
    by.y = 'HSA', 
    all.x = TRUE
    )

# Count complete cases  
sum(complete.cases(dt)) / nrow(dt)

# More details
dt[
  ,
  lapply(
    .SD,
    FUN = 
      function(x){
        sum(is.na(x))
        }
    )
  ]

# Decide how to handle missing, drop amputation or is it really good feature 
# when present?

dt[
  ,
  DM_All_Rate := DM_All_count / Members.x
  ]

cols2Keep <- 
  c(
    'HSA', 
    'Name.x',
    'Members.x', 
    'Hospital_asr', 
    'Outpatient_asr',
    'primVisit_All_Rate',
    'DM_All_Rate', 
    'mammogram_All_Rate',
    'sensVisit_All_Rate', 
    'TotalMortality'
    )

stopifnot(
  sum(
    cols2Keep %in% names(dt)) == length(cols2Keep)
  )


dt <- 
  dt[
    , 
    .SD, 
    .SDcols = cols2Keep
    ]

sum(complete.cases(dt)) / nrow(dt)

dt[
  ,
  lapply(
    .SD, 
    FUN = function(x) {
      sum(is.na(x))
      }
    )
  ]

# For simplicity
dt <- 
  dt[complete.cases(dt)]

stopifnot(
  sum(complete.cases(dt)) == nrow(dt)
  )

# Review data
ggplot(
  dt, 
  aes(
    x = Outpatient_asr, 
    y = primVisit_All_Rate, 
    z = TotalMortality
    )
  ) +
 stat_summary_hex(fun = mean) +
 scale_fill_gradientn(
   colors = c(
     'blue', 
     'grey80', 
     'red'
     )
   ) +
 theme_bw()

# Create outcome
ggplot(
  dt, 
  aes(
    x = Hospital_asr
    )
  ) +
 geom_histogram(bins = 100) +
#geom_density()+
 geom_vline(
   xintercept = 5000, 
   color = 'red'
   ) +
 theme_bw()

dt[
  , 
  y := (Hospital_asr > 5000)
  ]  # predict high vs not-high hospital costs

set.seed(40100) # Was 51300 so figures may differ from lecture
test.idx <- 
  sample(
    nrow(dt), 
    ceiling(nrow(dt) * 0.20)
    )

test <- dt[test.idx]
train <- dt[
  setdiff(
    1:nrow(dt), 
    test.idx
    )
  ]

stopifnot(
  nrow(test) + nrow(train) == nrow(dt)
  )

# Keep as features the columns of interest, not the outcome
vars2Use <-
  setdiff(
    cols2Keep, 
    c(
      'HSA',
      'Name.x', 
      'Hospital_asr', 
      'Members.x'
      )
    )

# SCALING and save for production deployment
ccMean  <- 
  train[
    , 
    lapply(
      .SD, 
      mean
      ), 
    .SDcols = vars2Use
    ]

ccSigma <- 
  train[
    , 
    lapply(
      .SD, 
      sd
      ), 
    .SDcols = vars2Use
    ]

# Not many columns lets seq_along; Note use of training scales to adjust test!
for (k in seq_along(vars2Use)){  # data.table can be painful with names vs symbols
 train[[ vars2Use[k] ]] <- 
   (train[[ vars2Use[k] ]] - ccMean[[ vars2Use[k] ]]) / ccSigma[[ vars2Use[k] ]] 
 
 test [[ vars2Use[k] ]] <- 
   (test[[ vars2Use[k]  ]] - ccMean[[ vars2Use[k] ]]) / ccSigma[[ vars2Use[k] ]]
}

stopifnot(
  all.equal(
    colMeans(
      train[
        , 
        .SD, 
        .SDcols = vars2Use
        ]
      ),
    rep(
      0, 
      length(vars2Use)
      ), 
    check.attributes = FALSE
    )
  )

colMeans(
  test[
    , 
    .SD, 
    .SDcols = vars2Use
    ]
  )

# Make the big matrix, just to debug and get stats
train.dist <- 
  dist(
    train[
      , 
      .SD, 
      .SDcols = vars2Use
      ]
    )

max.dist <- 
  max(train.dist)

hist(train.dist)

quantile(
  x = train.dist, 
  probs = seq(
    0, 
    1, 
    0.05
    )
  )

save(
  train, 
  test, 
  vars2Use, 
  file = paste(
    wrkng.path,
    '4.0d train_test.RData',
    sep = ''
  )
)

# END