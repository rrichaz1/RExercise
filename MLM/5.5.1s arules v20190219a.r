####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.0 for Windows
# RStudio 1.1.456


#install.packages(c('arules', 'arulesViz'))

require(arules)

require(arulesViz)

data(Groceries)

#transaction data are not data.frames
Groceries

summary(Groceries)

slotNames(Groceries) # The slots in an object from a formal class

head(Groceries@itemInfo)

itemLabels(Groceries)[1:10]

# Look into transactions
inspect(Groceries[1:10])

itemFrequencyPlot(
  Groceries, 
  topN = 20, 
  type = 'absolute',  
  cex.names = 0.5
  )

rules <- 
  apriori(
    Groceries, 
    parameter = 
      list(
        supp = 0.001, 
        conf = 0.8
        )
    ) # Min Support as 0.001, confidence as 0.8.

quality(rules[1:20]) # show the support, lift and confidence for all rules

inspect (rules[1:5])

plot(rules)

plot(
  rules, 
  'grouped matrix'
  )

options(digits = 3)

rules <- 
  sort(
    rules, 
    by = 'confidence', 
    decreasing = TRUE
    ) # high-confidence rules.

inspect(rules[1:10])

plot(
  rules[1:10],
  method = 'paracoord'
  )

# high-lift rules.
inspect (
  sort(
    rules, 
    by = 'lift', 
    decreasing = TRUE
    )[1:10]
  )

plot(
  sort(
    rules, 
    by = 'lift', 
    decreasing = TRUE
    )[1:10],
  method = 'paracoord'
  )

subrules10 <- 
  subset(
    rules, 
    lift > 10
  )

inspect(subrules10)

## 2D matrix with shading
plot(
  subrules10, 
  method = 'matrix'
  ) # Fails because there is only one rule with lift > 10

subrules08 <- 
  subset(
    rules, 
    lift > 8
  )

inspect(subrules08)

## 2D matrix with shading
plot(
  subrules08, 
  method = 'matrix'
  )

subrules05 <- 
  subset(
    rules, 
    lift > 5
  )

inspect(subrules05)

## 2D matrix with shading
plot(
  subrules05, 
  method = 'matrix'
  )

####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

rulesFromBeer <- 
  apriori(
    data = Groceries, 
    parameter = list(
      supp = 0.001,
      conf = 0.15,
      minlen = 2
      ),
    appearance = list(
      default = 'rhs',
      lhs = 'bottled beer'
      ),
    control = list(verbose = F)
    ) # those who bought lhs also bought rhs.

inspect(rulesFromBeer)

rules2Beer <- 
  apriori(
    data = Groceries, 
    parameter = list(
      supp = 0.001,
      conf = 0.30,
      minlen = 2
      ), 
    appearance = list(
      default = 'lhs',
      rhs = 'bottled beer'
      ),
    control = list(verbose = F)
    ) # those who bought lhs also bought rhs.

inspect(rules2Beer)

####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

itemsets <- 
  lapply(
    2:3, 
    function(x) {
      eclat(
        Groceries, 
        parameter = list(
          support = 0.02, 
          minlen = x
          )
        )
      }
    )

inspect(itemsets[[1]])

inspect(itemsets[[2]])

####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

rm(
  list = ls()
  ) # Clears global environment of all visible objects (some hidden
#   # objects may remain)

# End
