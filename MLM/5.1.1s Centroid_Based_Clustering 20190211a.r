####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

#Based on scripts by Yezhou Sun
# centroid_based_clustering.R


# Examples for k-means and k-medoids clustering.

# Packages for clustering, evaluation and visualization.
#install.packages('cluster', dependencies=T)
library(cluster)

#install.packages('clValid', dependencies=T)
library(clValid)

#install.packages('factoextra', dependencies=T)
library(factoextra)

# Set seed for randomness to maintain reproducibility
set.seed(51100) # Note changed from 123 so may differ from lecture

# Use iris data set
data(iris)

head(iris)

# Remove Species column and assign meaningful row names.
d <- 
  iris[
    , 
    -5
    ]

rownames(d) <- # Updates rownames in matrix d to first two letters of species
  paste(
    substr(
      as.character(iris$Species), 
      1, 
      2
      ), 
    rownames(iris), 
    sep = ''
    )

# Normalize data by Z-transformation.
d <- 
  scale(
    d, 
    center = TRUE, # Mean Center
    scale = TRUE # SD Scale
    ) 

# Find optimal number of clusters
method.test <- 
  c(
    'kmeans', 
    'pam' # pam stands for partition around medoids.
    )		

clusters.test <- 2:6 # To test 2 to 6 clusters.

# Calculate connectivity, Silhouette width and Dunn index for clustering 
# with specific number of clusters.
# Optimal clustering should minimize connectivity and maximize Dunn index
# and Silhouette width.
clv.out <- 
  clValid(
    d, 
    nClust = clusters.test, 
    clMethods = method.test, 
    validation = 'internal'
    )

summary(clv.out)

plot(clv.out) # YOU NEED TO HIT <RETURN> THREE TIMES IN THE CONSOLE TO 
              # SUCCESSFULLY COMPLETE THIS SCRIPT. NOTE PLOTS IN PLOT WINDOW.

# Note that all three methods found 2 clusters is optimal for both k-means and 
# pam, although we know there are three species in data set. The method 
# esssentially failed a positive control.

# Clustering for 3 clusters
kmeans.cl <- 
  kmeans(
    x = d, 
    centers = 3
    )

kmeans.cl

pam.cl <- 
  pam(
    x = d, 
    k = 3, 
    stand = TRUE # Standardize before calculating dissimilarities
    )

pam.cl

# Plot using first two principal dimensions since there are four variables in 
# data set.

# fviz_cluster(
# kmeans.cl, 
# data = d, 
# show.clust.cent = T, 
# repel = T, 
# main = 'K-means clustering', 
# ggtheme = theme_classic()
# )

fviz_cluster(
  object = kmeans.cl, 
  data = d, 
  show.clust.cent = TRUE, 
  main = 'K-means clustering', 
  ggtheme = theme_classic()
  )

#fviz_cluster(
# pam.cl, 
# show.clust.cent = T, 
# repel = T, 
# main = 'Partition Around Medoids (PAM)', 
# ggtheme = theme_classic()
# )

fviz_cluster(
  object = pam.cl, 
  show.clust.cent = TRUE, 
  main = 'Partition Around Medoids (PAM)', 
  ggtheme = theme_classic()
  )

# Get cluster membership
kmeans.cl$cluster

pam.cl$cluster

# Get centers and medoids
kmeans.cl$centers

pam.cl$medoids

rm(
  list = ls()
  )

# END