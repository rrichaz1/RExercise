####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

#Marc d. Paradis
#R 3.5.1 for Windows
#RStudio 1.1.456

#Based on scripts by Chris Hane

require(data.table)

require(ggplot2)

myTheme <- 
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    )

## better data
set.seed(972)

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
    size = 2 ,
    alpha = 0.6
    ) +
  myTheme

dt.d <- 
  dist(dt[,.(x,y)])

cls <- 
  hclust(
    dt.d, 
    method = 'ward.D'
    )

plot(cls)

screeplot <- 
  function(tree, k){
    n <-
      length(tree$height)
    tr <- 
      data.table(
        x = k:1, 
        height = tree$height[(n - k + 1):n]
        )
    ggplot(
      tr, 
      aes(
        x = x, 
        y = height
        )
      ) +
      geom_point(size=3) + 
      myTheme
    }

screeplot(
  cls, 
  10
  )

c4 <- 
  cutree(
    cls, 
    k = 4
    )

table(c4)

dt[ , cluster4 := c4]

ggplot(
  dt, 
  aes(
    x = x, 
    y = y, 
    color = factor(cluster4)
    )
  ) +
  geom_point(
    size = 2,
    alpha = 0.6
    ) +
  myTheme

ggplot(
  dt, 
  aes(
    x = x, 
    y = y
    )
  ) +
  geom_point(
    color = cutree(
      tree = cls,
      3 # 3 clusters
      ), 
    size = 2 ,
    alpha = 0.6
    ) +
  myTheme

ggplot(
  dt, 
  aes(
    x = x, 
    y = y
    )
  ) +
  geom_point(
    color = cutree(
      tree = cls,
      5 # 5 Clusters
      ), 
    size = 2,
    alpha = 0.6
    ) +
  myTheme

# centroid clustering can have inversions of the height
cls <- 
  hclust(
    d = dt.d, 
    method = 'centroid'
    )

plot(
  cls, 
  labels = FALSE
  )

ggplot(
  dt, 
  aes(
    x = x, 
    y = y
    )
  ) +
  geom_point(
    color = cutree(
      tree = cls,
      4 # 4 clusters (note the lone blue dot!)
      ), 
    size = 2,
    alpha = 0.6
    ) +
  myTheme

screeplot(
  cls, 
  10
  )

ggplot(
  dt, 
  aes(
    x = x, 
    y = y
    )
  ) +
  geom_point(
    color = cutree(
      cls,
      5
      ), 
    size = 2 ,
    alpha = 0.6
    ) +
  myTheme

ggplot(
  dt, 
  aes(
    x = x, 
    y = y
    )
  ) +
  geom_point(
    color = cutree(
      tree = cls,
      3 # 3 clusters (note lone green dot)
      ), 
    size = 2,
    alpha = 0.6
    ) +
  myTheme

#outliers
ggplot(
  dt, 
  aes(
    x = x, 
    y = y
    )
  ) +
  geom_point(
    color = cutree(
      tree = cls,
      15 # Choose arbitrarily large number of clusters to identify outliers
      ), 
    size = 2,
    alpha = 0.6
    ) +
  myTheme

#dendextend
require(dendextend)

s<-
  as.dendrogram(
    hclust(
      d = dt.d, 
      method = 'single'
      )
    )

set( # set.dendrogram
  dend = s, 
  what = 'branches_k_color', 
  k = 4
  )

wd <- 
  as.dendrogram(
    hclust(
      dt.d, 
      method = 'ward.D'
      )
    )

set(
  wd, 
  'branches_k_color', 
  k = 4
  )

# Tanglegrams take 30 - 60 seconds to run on my laptop

tanglegram(
  dend1 = s, 
  dend2 = wd, 
  sort = TRUE
  )

tanglegram(
  dend1 = ladderize(
    x = s
    ), 
  dend2 = rev( # Reverse elements
    x = ladderize(
      x = wd
      )
    ), 
  sort = FALSE
  )

require(cluster)

# DIvisive  ANAlysis (di-ana)
cls <-
  diana(
    x = dt.d, 
    diss = TRUE
    )

str(cls)

plot( # YOU MUST HIT <ENTER> TWICE IN CONSOLE BEFORE RUNNING REMAINDER OF SCRIPT
  cls, 
  labels = FALSE
  )

# ggplot(
#   dt, 
#   aes(
#     x = x, 
#     y = y
#     )
#   ) +
#   geom_point(
#     color = cutree(
#       cls,
#       5
#       ), 
#     size = 2,
#     alpha = 0.6
#     ) +
#   myTheme

rm(
  list = ls()
  )

# END