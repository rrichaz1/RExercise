####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

#Based on scripts by Yezhou Sun

# DBSCAN_clustering


# In this exercise, we will use DBSCAN algorithm implemented in fpc 
# and dbscan libraries and data set from factoextra library.
# DBSCAN in fpc can work with arbitrary distance matrice but runs slowly 
# while DBSCAN in dbscan is fast but can only use Euclidean distance.

# Install libraries.
#install.packages('fpc', dependencies=T)
#install.packages('dbscan', dependencies=T)
#install.packages('factoextra', dependencies=T)
#install.packages('ggplot2', dependencies=T)

library(fpc)

library(dbscan)

library(factoextra)

library(ggplot2)
 
options(scipen = 999)

set.seed(52100) # Results may differ from lecture as originally set.seed(12345)

# Load data from factoextract.
data(
  'multishapes', 
  package = 'factoextra'
  )

str(multishapes)

d <- 
  multishapes[, 1:2]

# Visualize data set.
plot(
  d$x, 
  d$y
  )

# DBSCAN has two parameters:
# epsilon (eps) is the radius of neighborhoods around a data point.
# minimum points (minPts) is the minimum number of data points needed to define
# a cluster. minPts should be >= 3 but optimal value may need trials.
# Once minPts selected, eps can be determined by k-distance graph.

# Using minPts = 5 which is default for both fpc and dbscan libraries.
# Determine optimal eps.

kNNdistplot(
  d, 
  k = 5
  )

abline(
  h = 0.15, 
  col = 'red'
  )

# Note that pairwise distance begins to increase dramatically at about 
# eps = 0.15. So eps = 0.15 is subjectively the optimal value.

x <- 
  kNNdist( # Can be used to help find a suitable value for epsilon (eps) for
           # DBSCAN
    x = d, 
    k = 5
    )

x[1:10, ]

# Five columns are distances from five nearest neighborhoods of a data point.

# Clustering using dbscan library and count time.
system.time(
  cl1 <- 
    dbscan::dbscan( # Use dbscan() from dbscan package
      x = d, 
      eps = 0.15, 
      minPts = 5
      )
  )

fviz_cluster(
  object = cl1, 
  data = d, 
  stand = FALSE, 
  ellipse = FALSE, 
  show.clust.cent = FALSE,
  geom = 'point', 
  palette = 'npg', 
  ggtheme = theme_classic()
  )

# Clustering using fpc library and count time.
system.time(
  cl2 <- 
    fpc::dbscan( # Use dbscan() rom fpc package
      d, 
      eps = 0.15, 
      MinPts = 5
      )
  )

fviz_cluster(
  object = cl2, 
  data = d, 
  stand = FALSE, 
  ellipse = FALSE, 
  show.clust.cent = FALSE,
  geom = 'point', 
  palette = 'npg', 
  ggtheme = theme_classic()
  )

# Note the difference in time used by two dbscan implementations.

# Summary of clustering result.
cl1
cl2

# Extract cluster membership of original data points.
member <- cl1$cluster

# length(member) = nrow(d), each number in member corresponds  to one row in d.

# Evaluate quality of clusters.
cluster.quality <- 
  cluster.stats(
    d = dist(d), 
    clustering = cl1$cluster
    )

str(cluster.quality)

# Extract two commonly used metrics.
cluster.quality[c('within.cluster.ss', 'avg.silwidth')]

rm(
  list = ls()
  )

# End