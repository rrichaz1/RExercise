####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

#Based on scripts by Chris Hane

require(data.table)

require(clv)

require(cluster)

require(ggplot2)

# random data points
set.seed(1066)  # Battle of Hastings
# Note that I am not following my best practice on set.seed() seed numbering.
# This is because the discussion and flow of the lecture are currently too
# tightly intertwined with this specific seed. It would be hard to follow the
# points made in the lecture if the generated data were different as they 
# would be with a new seed.

# Create a random 2D field of points

rand.dt <- 
  data.table(
    x = runif(
      n = 100
      ), # 100 random uniform points
    y = runif(
      n = 100
      ) # Another 100 random uniform points
    )



# This is ggplot2 "grammar of graphics"

myTheme <- 
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    )

ggplot(
  rand.dt, 
  aes(
    x = x, 
    y = y
    )
  ) +
  geom_point(
    color = 'red'
    ) 
  myTheme

kClusters <- 
  seq(
    from = 3, 
    to = 10, 
    by = 1
    )

solution <- 
  lapply(
    kClusters,
    function(k) {
      clst <- 
        kmeans(
          rand.dt, 
          k
          )
      results <- 
        fitted(
          clst, 
          'classes'
          )
      return( # This is a nice way to have a function return multiple values
        list(
          model = clst, 
          fitted = results
          )
        )
      }
    )

solution[[1]] # takes first value in kcluster list and feeds through custom
#             # function we just created

plotMyRandCluster <- 
  function(c) {
    ggplot(
      rand.dt, 
      aes_string(
        x = 'x', 
        y = 'y', 
        color = c
        )
      ) +
      geom_point(
        size = 4, 
        alpha = 0.7
        ) +
      scale_color_brewer(
        type = 'qual'
        ) +
      myTheme
    }

rand.dt[
  , 
  cluster3 := factor(
    unlist(
      solution[[1]]['fitted'] # Remember first value in list was 3 clusters
      )
    )
  ]

plotMyRandCluster('cluster3')
    
rand.dt[ 
  , 
  cluster6 := factor(
    unlist(
      solution[[4]]['fitted'] # fourth value in list was 6 clusters
      )
    )
  ]

plotMyRandCluster('cluster6')

# Directly view the between and within sum of squares
qplot(
  x = 1:length(solution), # Length = 8 bc assesses k = 3, 4, 5, 6, 7, 8, 9, 10
#                         # i.e. x-axis = cluster number: 1 => 3; 2 => 4; etc.
  y = sapply(
    solution, 
    function(j){
      j[['model']]$betweenss / j[['model']]$totss # Between divided by total
      },
    simplify = TRUE
    ), 
  geom = 'line'
  )


qplot(
  x = 1:length(solution), # Length = 8 bc assesses k = 3, 4, 5, 6, 7, 8, 9, 10
#                         # i.e. x-axis = cluster number: 1 => 3; 2 => 4; etc.
  y = sapply(
    solution, 
    function(j){
      max(j[['model']]$withinss / j[['model']]$totss) # Within divided by total
      },
    simplify = TRUE
    ), 
  geom = 'line'
  )

# use clv
dunnCl3 <- 
  clv.Dunn(
    cls.scatt.data(
      rand.dt[
        , 
        .(x,y)
        ], 
      clust = as.integer(rand.dt$cluster3), 
      dist = 'euclidean'
      ),
    intracls = c( # Columns in dunnCl3
      'complete',
      'average',
      'centroid'
      ),
    intercls = c( # Rows in dunnCl3
      'single', 
      'complete', 
      'average',
      'centroid'
      )
    )

dunnCl3

dunnCl6 <- 
  clv.Dunn(
    cls.scatt.data(
      rand.dt[ 
        , 
        .(x,y)
        ], 
      clust = as.integer(rand.dt$cluster6), 
      dist = 'euclidean'
      ),
    intracls = c(
      'complete', # Columns in dunnCl6
      'average',
      'centroid'
      ),
    intercls = c(
      'single',  # Rows in dunnCl6
      'complete', 
      'average',
      'centroid'
      )
    )

  dunnCl6

# Note essentially meaningless differences between dunncl3 and dunncl6
# euclidean distance measures

## better data
set.seed(972)
# Note that I am not following my best practice on set.seed() seed numbering.
# This is because the discussion and flow of the lecture are currently too
# tightly intertwined with this specific seed. It would be hard to follow the
# points made in the lecture if the generated data were different as they 
# would be with a new seed.

clst.dt <- 
  rbindlist(
    list(
      data.table(
        Set = 1, 
        x = rnorm(
          n = 50, 
          mean = -1,
          sd = 0.2
          ), 
        y = rnorm(
          n = 50, 
          mean = -1, 
          sd = 0.2
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
          -0.3, 
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

#Add noise
clst.dt
str(clst.dt)

v <- clst.dt$x
is.vector(v)
str(v)
v
jitter(clst.dt$x,2,NULL)
(clst.dt$x - v)
clst.dt[,2]

jitter(v)
(clst.dt$x - v)

clst.dt <- data.table(clst.dt$Set, v, clst.dt$y)

clst.dt
#End - add noise



ggplot(
  clst.dt, 
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

kClusters <- 
  seq(
    from = 2, 
    to = 6, 
    by = 1
    )

plotMyCluster <- 
  function(c) {
    ggplot(
      clst.dt, 
      aes_string(
        x = 'x', 
        y = 'y', 
        color = c
      )
    ) +
      geom_point(
        size = 4, 
        alpha = 0.7
      ) +
      scale_color_brewer(
        type = 'qual'
      ) +
      myTheme
  }

# careful to select the proper columns of dt to cluster (omitting .(x,y) would 
# use Set!)
clst.solution <- 
  lapply(
    kClusters,
    function(k) {
      clst <- 
        kmeans(
          clst.dt[ 
            ,
            .(x,y)
            ], 
          k
          )
      results <- 
        fitted(
          clst, 
          'classes'
          )
      return(
        list(
          model = clst, 
          fitted = results
          )
        )
      }
    )

clst.dt[ 
  , 
  cluster3 := factor( # cluster3 sets k (nbr of clusters) to 3
    unlist(
      clst.solution[[2]]['fitted'] # Second element in list is 3
      )
    )
  ]

plotMyCluster('cluster3')

clst.dt[ 
  , 
  cluster4 := factor( # cluster3 sets k (nbr of clusters) to 4
    unlist(
      clst.solution[[3]]['fitted'] # Third element in list is 4
      )
    )
  ]

plotMyCluster('cluster4') # Pretty good job, but still mis-clusters outliers

clst.dt[ 
  , 
  cluster5 := factor( # cluster3 sets k (nbr of clusters) to 5
    unlist(
      clst.solution[[4]]['fitted']
      )
    )
  ]

plotMyCluster('cluster5')

# compare dunn index
(
  dunnIndex <- 
    sapply(
      3:5, # Try 3, 4 and 5 clusters
      function(j) {
        clv.Dunn(
          cls.scatt.data(
            clst.dt[ 
              , 
              .(x,y)
              ],
            clust = as.integer(
              clst.dt[[
                paste0(
                  'cluster',
                  j
                  )
                ]]
              ),
            dist = 'euclidean'
            ),
          intracls = c('centroid'),
          intercls = c('centroid')
        )
        }
      )
  )

# 4 Clusters has highest Dunn Index

# compare Davies-Bouldin index
(
  dbIndex <- 
    sapply(
      3:5, # Try 3, 4 and 5 clusters
      function(j) {
        clv.Davies.Bouldin(
          cls.scatt.data(
            clst.dt[ 
              , 
              .(x,y)
              ],
            clust = as.integer(
              clst.dt[[
                paste0(
                  'cluster', 
                  j
                  )
                ]]
              ),
            dist='euclidean'),
            intracls = c('centroid'), 
            intercls = c('centroid')
          )
        }
      )
  )

# 4 clusters has lowest Davies-Bouldin Index

# co-occurence and correlation
dt.dist <- 
  as.vector(
    as.matrix(
      dist(
        clst.dt[ 
          ,
          .(x,y)
          ]
        )
      )
    )

dt.idx1 <- 
  rep(
    1:nrow(clst.dt), 
    times = nrow(clst.dt)
    ) # for rows repeat 1..N N times

dt.idx2 <- 
  rep(
    1:nrow(clst.dt), 
    each = nrow(clst.dt)
    )  # for columns repeat 1 N times, 2 N times, 3 N times...

cD <- 
  data.table(
    i = dt.idx1, 
    j = dt.idx2, 
    d = dt.dist
    ) # i is rows, j is columns

coMatrix <- 
  function(v){ # if v(j) and v(k) are same label then 1 else 0
    tmp <- 
      data.table(
        i = rep(
          v, 
          times = length(v)
          ),
        j = rep(
          v, 
          each = length(v)
          )
        )
    tmp[ 
      ,
      co := (i==j)
      ]
    tmp$co
    }

cD4 <- 
  coMatrix(clst.dt$cluster4)

# A little QA, sum of coMatrix is sum of size of each cluster^2
all.equal(
  sum(
    table(clst.dt$cluster4)^2
    ), 
  sum(cD4)
  )

cD3 <- 
  coMatrix(clst.dt$cluster3)

cor(
  x = cD$d, 
  y = cD3 # k = 3
  )

cor(
  x = cD$d, 
  y = cD4 # k = 4
  )

cD5 <- 
  coMatrix(clst.dt$cluster5)

cor(
  x = cD$d, 
  y = cD5 # k = 5
  )

# Note that k = 4 is largest negative value of the three

rm(
  list = ls()
  )

# END

