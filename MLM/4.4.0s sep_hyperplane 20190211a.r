####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

# Based on scripts by Chris Hane

require(data.table)
require(ggplot2)

set.seed(44000) # Note changed from 111 so figures may differ from lecture
d1 <- 
  data.table(
    x = runif(50), 
    y = runif(50)
    )

d2 <- 
  data.table(
    x = runif(50), 
    y = runif(50)
    )

d1[ 
  ,
  ':='(
    x = x - 0.3,
    y = (y - 0.5) * 0.7, 
    label = 'a'
    )
  ]

d2[ 
  ,
  ':='(
    x = (x + 0.5) * 0.6,
    y = (y + 0.4) * 0.7, 
    label = 'b'
    ) 
  ]

d <- 
  rbindlist(
    list(
      d1,
      d2
      )
    )

centers <- 
  d[
    , 
    .(x = mean(x), 
      y = mean(y)
      ), 
    by = label
    ]

cc <-
  colSums(
    centers[
      ,
      .SD, 
      .SDcols = 2:3
      ]
    ) / 2

midCenter <- 
  data.table(
    x = cc[1], 
    y = cc[2], 
    label = NA
    )

ggplot(
  d, 
  aes(
    x = x, 
    y = y, 
    color = label
    )
  ) + 
  geom_point(size = 4) +
  geom_point(
    data = centers,
    aes(
      x = x, 
      y = y, 
      fill = label
      ), 
    size = 6, 
    shape = 22
    ) +
  theme_bw()

ggplot(
  data = centers, 
  aes(
    x = x, 
    y = y, 
    fill = label
    )
  ) +
  geom_point(
    size = 6, 
    shape = 22
    ) +
 geom_point(
   data = midCenter, 
   shape = 23, 
   size = 4) +
 xlim(
   c(-0.3, 0.9)) + 
 ylim(
   c(-0.5, 1.0)) +
 geom_line(group = 1) +
 theme_bw()

ggplot(
  d, 
  aes(
    x = x, 
    y = y, 
    color = label
    )
  ) +
  geom_point(size = 4) +
  geom_point(
    data = centers, 
    aes(
      x = x, 
      y = y, 
      fill = label
      ), 
    size = 6, 
    shape = 22
    ) +
  geom_point(
    data = midCenter, 
    shape = 23, 
    size = 4
    ) +
  xlim(
    c(-0.3, 0.9)
    ) +
  ylim(
    c(-0.5, 1.0)
    ) +
  geom_line(
    data = centers, 
    group = 1
    ) +
  theme_bw()

rm(
  list = ls()
  ) # Clears global environment of all visible objects (some hidden objects
#   # may remain)

# END