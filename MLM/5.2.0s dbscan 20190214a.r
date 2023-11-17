####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

# Based on scripts by Chris Hane

require(data.table)

require(igraph)

require(dbscan)

require(ggplot2)

myTheme <- 
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    )

## better data
set.seed(972)
# Note that I am not following my best practice on set.seed() seed numbering.
# This is because the discussion and flow of the lecture are currently too
# tightly intertwined with this specific seed. It would be hard to follow the
# points made in the lecture if the generated data were different as they 
# would be with a new seed.

dt <- 
  rbindlist(
    list(
      data.table(
        Set = 1, 
        x = rnorm(
          50, 
          -1,
          0.2
          ), 
        y = rnorm(
          50, 
          -1, 
          0.2
          )
        ),
      data.table(
        Set = 2, 
        x = rnorm(
          100, 
          -0.5,
          0.1
          ), 
        y = rnorm(
          100, 
          -.3, 
          0.2
          )
        ),
      data.table(
        Set = 3, 
        x = rnorm(
          100, 
          0.2,
          0.2
          ), 
        y = rnorm(
          100, 
          0.4, 
          0.1
          )
        ),
      data.table(
        Set = 4, 
        x = rnorm(
          100, 
          0.3,
          0.2
          ), 
        y = rnorm(
          100, 
          -0.5, 
          0.2
          )
        )
      )
    )

ggplot(
  dt, 
  aes(
    x = x, 
    y = y, 
    color = factor(Set)
    )
  ) +
  geom_point(
    size = 2,
    alpha = 0.6
    ) +
  myTheme


set.seed(333)
# Note that I am not following my best practice on set.seed() seed numbering.
# This is because the discussion and flow of the lecture are currently too
# tightly intertwined with this specific seed. It would be hard to follow the
# points made in the lecture if the generated data were different as they 
# would be with a new seed.

hypSet <- 
  expand.grid(
    eps = c( # eps = epsilon neighborhood
      0.05, 
      0.10, 
      0.15, 
      0.20
      ), 
    minPts = 3:6
    )

solution <- 
  lapply(
    1:nrow(hypSet),
    function(k) {
      dbscan(
        x = dt[, .(x,y)],
        , # Double-check on purpose of this...
        eps = hypSet$eps[k], 
        minPts = hypSet$minPts[k]
        )
      }
    )

# how many clusters did each run find?
sapply(
  solution, 
  function(j){
    uniqueN(
      j$cluster
    )
  }
  )

eps <- 0.2
minPts <- 4

nnFixed <- 
  frNN( # Fixed Radius Nearest Neighbors 
    x = dt[, .(x,y)], 
    eps = eps # Neighbors Radius
    )

plot( # BE PATIENT: THIS PLOT TAKEs A FEW MOMENTS TO RUN
  nnFixed, 
  dt[, .(x,y)]
  )

plot(
  dt[,.(x,y)], 
  col = solution[[12]]$cluster # eps = 0.2, minPts = 5
  )

points(
  dt[,.(x,y)][solution[[12]]$cluster == 0,],  # "Garbate Cluster"
  pch = 3, 
  col = 'grey'
  )


plot(
  dt[,.(x,y)], 
  col = solution[[10]]$cluster # eps = 0.1, minPts = 5
  )

points(
  dt[,.(x,y)][solution[[10]]$cluster == 0,],  # "Garbage Cluster"
  pch = 3, 
  col = 'grey'
  )

# Note how many more points are now in the Garbage Cluster when eps = 0.1
# as opposed to eps = 0.2.  Note also different number of cluseters.

require(clv)

## dropping noise points
dunnIndex <- 
  lapply(
    1:length(solution), 
    function(j) {
      idx <- 
        solution[[j]]$cluster
      dnn <- 
        NULL
  if(uniqueN(idx) > 2){
    dnn <-
      clv.Dunn(
        cls.scatt.data(
          dt[idx!=0 , .(x,y)],
          clust = idx[idx != 0],
          dist = 'euclidean'),  # Should we do this with dbscan?
        intracls = c('centroid'), # INTRA
        intercls = c('centroid') # INTER
      )
    }
      return(
        c(
          hypSet[j,], 
          dunn = dnn
          )
      )
      }
  )

dunnIndex <-
  as.matrix(
    unlist(dunnIndex)
    )

dunnIndex

# A bit annoying, but if you read through manually you will see max Dunn of
# 1.7732840 for epsilon neighborhood = 0.1 and minimum points = 4


rm(
  list = ls()
  )

# END