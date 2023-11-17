####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456


require(RANN)
require(data.table)
require(ggplot2)

wrkng.path <- 'C:/Users/mparadis/Desktop/DSU/Immersion College/02 MLM/04. Supervised Classification/4.1 Distance-Based Classification/'

load(
  paste(
    wrkng.path,
    '4.0d train_test.RData',
    sep = ''
    )
  )

str(train)
str(test)

#using RANN what is neighborhood?
kdtree <- 
  nn2(
    train[, .SD, .SDcols = vars2Use], 
    k = 11, 
    eps = 0
    )  # use 1 more than necessary because this returns the identity

str(kdtree)

kdtree$nn.idx <- 
  kdtree$nn.idx[, -1]

head(kdtree$nn.idx)

kdtree$nn.dists <- 
  kdtree$nn.dists[, -1]

head(kdtree$nn.dists)

#how to vote with kdtree?
voteKD <- 
  function(
    kd, 
    .y,
    event = TRUE
    ){ #vote for outcomes=event
    stopifnot(nrow(kd) == length(.y))
    apply(
      kd, 
      1, 
      function(x) {
        sum(.y[x] == event)
        }
    )
    }

yhat <- 
  voteKD(
    kdtree$nn.idx, 
    train$y, 
    TRUE
    )

yhat5 <- 
  voteKD(
    kdtree$nn.idx[,1:5], 
    train$y, 
    TRUE
    ) #no need to re-run if you want <=k neighbors

totalDist <- 
  rowSums(kdtree$nn.dists)

quantile(
  x = totalDist, 
  probs = seq(0, 1, 0.05)
  )

qplot(
  log10(totalDist), 
  geom = 'histogram', 
  bins = 100
  )

train[
  which(totalDist>16), 
  .SD, 
  .SDcols = vars2Use
  ] #which have row sum distance ~95-tile or more

## apply KNN from package class
require(class)

knn5 <- 
  knn(
    train[
      , 
      .SD, 
      .SDcols = vars2Use
      ], 
    test[
      , 
      .SD, 
      .SDcols = vars2Use
      ], 
    cl = train$y, 
    k = 5, 
    prob = TRUE
    )

test[ 
  , 
  knn5 := knn5
  ]

#generic accuracy
twoByTwo <- 
  table(
    test$knn5, 
    test$y
    )

twoByTwo

sum(diag(twoByTwo)) / sum(twoByTwo)

ggplot(
  test,
  aes_string(
    x = vars2Use[1],
    y = vars2Use[3],
    color="knn5"
    )
  ) +
  geom_point(
    size = 2,
    alpha = 0.5
    ) +
  scale_fill_manual(
    values = c("red","blue")
    ) +
  facet_grid(
    y ~ knn5
    ) +
  theme_bw()

#how many decisions are made with <66% of votes?
votes <- 
  attr(knn5, 'prob')

sum(votes < 0.66)/length(votes)

table(votes)

rm(
  list = ls()
  ) # Clears global environment of all visible objects (some hidden objects
#   # may remain)

# End