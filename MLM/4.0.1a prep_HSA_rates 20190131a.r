####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

#Based on scripts by Chris Hane

require(data.table)
require(zoo) # for the NA replacements, na.locf
require(stringi)

wrkng.path <- 'C:/Users/mparadis/Desktop/DSU/Immersion College/02 MLM/04. Supervised Classification/4.1 Distance-Based Classification/'

pc <- 
  fread(
    paste(
      wrkng.path,
      'pc_hsa_rates_2014.csv',
      sep = ''
    ), 
    header = FALSE
  )


pcNames <- 
  data.table(
    t(
      pc[1:3]
      )
    )

setnames(
  pcNames, 
  c(
    'group',
    'race',
    'value'
    )
  )

pcNames[ 
  group == '', 
  group := NA
  ]

pcNames[ 
  race == '',  
  race := NA
  ]

pcNames[ 
  value == '', 
  value := NA
  ]

# Fill in group name
pcNames[ 
  ,
  ':='(
    group = na.locf(group),
    race = na.locf(
      race, 
      na.rm = FALSE
      )
    )
  ]  # na.rm FALSE to keep leading NA as NA

pcNames[
  is.na(value), 
  value := 'count'
  ]

pcNames[ 
  , 
  value := stri_replace_all_fixed(
    value,
    'Lower confidence limit', 
    'LowCI'
    )
  ]

pcNames[ 
  , 
  value := stri_replace_all_fixed(
    value,
    'Upper confidence limit', 
    'UpperCI'
    )
  ]

pcNames[ 
  , 
  race := stri_replace_all_fixed(
    race,
    'Overall', 
    'All'
    )
  ]

# Order here matters
pcNames[
  group %like% 'Part B', 
  group := 'memB'
  ]

pcNames[
  group %like% 'Part A', 
  group := 'memA'
  ]

pcNames[
  group %like% 'primary care', 
  group := 'primVisit'
  ]

pcNames[
  group %like% 'sensitive', 
  group := 'sensVisit'
  ]

pcNames[
  group %like% 'A1c', 
  group := 'a1cVisit'
  ]

pcNames[
  group %like% 'eye exam', 
  group := 'eyeExam'
  ]

pcNames[
  group %like% 'lipids', 
  group := 'ldlTest'
  ]

pcNames[
  group %like% 'amputations', 
  group := 'amputate'
  ]

pcNames[
  group %like% 'diabetic', 
  group := 'DM'
  ]

pcNames[
  group %like% 'mammogram', 
  group := 'mammogram'
  ]

pcNames[
  group %like% 'female', 
  group := 'female'
  ]


pcNames[ 
  , 
  shortNames := stri_replace_all_fixed(
    paste(
      pcNames$group,
      trimws(pcNames$race),
      trimws(pcNames$value), 
      sep = '_'
      ),
    ' ', 
    '')
  ]

pcNames[
  1:2, 
  shortNames := c(
    'HSA',
    'Name'
    )
  ]


setnames(
  pc, 
  pcNames$shortNames
  )

pc <- 
  pc[
    4:nrow(pc)][
      HSA != 999999
      ]  # drop old name rows, drop entire US population

goodCols <- 
  pcNames$shortNames[
    pcNames$shortNames %like% 'All_count' | 
      pcNames$shortNames %like% 'All_Rate'
    ]

pc <- 
  pc[ 
    , 
    .SD, 
    .SDcols = names(pc) %in% c(
      'HSA', 
      'Name', 
      goodCols)
    ]

rm(pcNames)

pc <- 
  cbind(
    pc[
      , 
      .(HSA, Name)
      ], 
    pc[
      , 
      lapply(
        .SD, 
        as.numeric
        ), 
      .SDcols = 3:ncol(pc)
      ]
    )

# END