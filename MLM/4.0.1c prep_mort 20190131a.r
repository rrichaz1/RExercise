####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

#Based on scripts by Chris Hane

require(data.table)

wrkng.path <- 'C:/Users/mparadis/Desktop/DSU/Immersion College/02 MLM/04. Supervised Classification/4.1 Distance-Based Classification/'

mort <- 
  fread(
    paste(
      wrkng.path,
      'mortality_hsa_2014.csv',
      sep = ''
    ), 
    header = FALSE
  )

setnames(
  mort, 
  c(
    'HSA',
    'Name',
    'Members',
    'TotalMortality', 
    'NonHMOMortality'
    )
  )

mort <- 
  mort[2:nrow(mort)][
    HSA < 60000]

mort <- 
  cbind(
    mort[
      , 
      .(HSA, Name)
      ],
    mort[
      , 
      lapply(
        .SD, 
        as.numeric
        ), 
      .SDcols = 3:ncol(mort)
      ]
    )

# END