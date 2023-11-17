####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

#Based on scripts by Chris Hane

require(data.table)
require(zoo) # for the NA replacements, na.locf
require(stringi)

wrkng.path <- 'C:/Users/mparadis/Desktop/DSU/Immersion College/02 MLM/04. Supervised Classification/4.1 Distance-Based Classification/'

cost <- 
  fread(
    paste(
      wrkng.path,
      'pa_reimb_hsa_2014.csv',
      sep = ''
      ), 
      header = FALSE
    )

cNames <- 
  data.table(
    t(
      cost[1:2]
      )
    )

setnames(
  cNames, 
  c(
    'group',
    'value'
    )
  )

cNames[
  group == '', 
  group := NA
  ]

cNames[
  value == '', 
  value := NA
  ]

# Fill in group name
cNames[
  ,
  ':='(
    group = na.locf(group),
    value = na.locf(
      value, 
      na.rm = FALSE
      )
    )
  ]  # na.rm FALSE to keep leading NA as NA

cNames[
  value == 'Price, age, sex & race-adjusted', 
  value := 'pasr'
  ]

cNames[
  value == 'Age, sex & race-adjusted', 
  value := 'asr'
  ]

cNames[
  1:4, 
  value := ''
  ]

cNames[
  , 
  shortNames := paste(
    unlist(
      lapply(
        stri_split_fixed(
          cNames$group, 
          ' '
          ), 
        function(x){
          x[[1]]
          }
        )
      ),
    cNames$value, 
    sep = '_'
    )
  ]

cNames[
  1:4, 
  shortNames := c(
    'HSA',
    'Name', 
    'State',
    'Members'
    )
  ]

setnames(
  cost, 
  cNames$shortNames
  )

goodCols <- 
  cNames$shortNames[
    names(cost) %like% '_asr'
    ]

cost <- 
  cost [
    , 
    .SD, 
    .SDcols = names(cost) %in% c(
      'HSA',
      'Name', 
      'State',
      'Members', 
      goodCols
      )
    ]

cost <- 
  cost[
    3:nrow(cost)
    ][
      HSA < 60000
      ]

rm(cNames)

cost <- 
  cbind(
    cost[
      , 
      .(
        HSA, 
        Name, 
        State
        )
      ],
    cost[
      , 
      lapply(
        .SD, 
        as.numeric
        ), 
      .SDcols = 4:ncol(cost)
      ]
    )

# END