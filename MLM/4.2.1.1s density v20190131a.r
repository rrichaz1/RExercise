####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

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

ggplot(
  train,
  aes( #Aesthetic Mapping
    x = primVisit_All_Rate
    )
  ) +
  geom_density(fill = 'grey80') + #Smoothed Density Estimate
  geom_rug() +
  theme_bw()


ggplot(
  train,
  aes( # Aesthetic Mapping
    x = primVisit_All_Rate, 
    fill = y #Splits by T/F y column
    )
  ) +
  geom_density(alpha = 0.5) +
  geom_rug(alpha = 0.5) +
  theme_bw()

# in 2D
ggplot(
  train,
  aes(
    x = primVisit_All_Rate,
    y = DM_All_Rate
    )
  ) +
  stat_density_2d( #Contours of a 2D Density Estimate
    aes(fill = ..level..), 
    geom = 'polygon') +
  geom_rug(alpha = 0.5) +
  theme_bw()


#extract info and make the classifier
pcpY1 <- 
  density( #Kernel Density Estimation
    train[
      y == 1, 
      primVisit_All_Rate
      ]
    )

pcpY0 <- 
  density(
    train[
      y == 0, 
      primVisit_All_Rate
      ]
    )

prior <- 
  nrow(
    train[
      y == 1
      ]
    ) / nrow(train)

classDensity <- 
  data.table(
    x = pcpY1$x, 
    post = pcpY1$y * prior / (pcpY1$y * prior + pcpY0$y * (1 - prior))
    )

ggplot(
  classDensity, 
  aes(
    x = x, 
    y = post
    )
  ) +
  geom_line() +
  geom_hline(
    yintercept = 0.50, 
    color = 'red'
    ) +
  theme_bw()

ggplot(
  train,
  aes(
    x = TotalMortality, 
    fill = y
    )
  ) + 
  geom_density(alpha = 0.5) +
 geom_rug(alpha = 0.5) +
 theme_bw()

pcpY1 <- 
  density(
    train[
      y == 1,
      TotalMortality
      ]
    )

pcpY0 <- 
  density(
    train[
      y == 0, 
      TotalMortality
      ]
    )

prior <- 
  nrow(train[y == 1]) / nrow(train)

classDensity <- 
  data.table(
    x = pcpY1$x,
    post = pcpY1$y * prior / (pcpY1$y * prior + pcpY0$y * (1 - prior))
    )

ggplot(
  classDensity, 
  aes(
    x = x, 
    y = post
    )
  ) +
  geom_line() +
  geom_hline(
    yintercept = 0.50, 
    color='red'
    ) +
  theme_bw()

rm(
  list = ls()
  ) # Clears global environment of all visible objects (some hidden objects
#   # objects may remain)

# End