####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

#Marc d. Paradis
#R 3.5.1 for Windows
#RStudio 1.1.456

# Theoretical Distribution Parameters
X01.norm.mu <- 1
X01.norm.sigma <- 0.5
X02.norm.mu <- 1
X02.norm.sigma <- 0.5

# Theoretical Linear Regression Parameters
Theta00.norm <- 1
Theta01.norm <- 1
Theta02.norm <- 1
# Theta.hat will be estimated Linear Regression Parameters

# Theoretical Priors
X00.norm <- as.vector(Theta00.norm)
X01.norm <- as.vector(0)
X02.norm <- as.vector(0)

# Generative/Causal Model
LinearCausalModel <- function(Thetas, Independents){
  Dependent <- 
    Independents %*% Thetas # Linear Algebra equation for a line
  df.var <- 
    as.data.frame(
      cbind( # binds two or more vectors, matrices or arrays by column
        Dependent, 
        Independents
        )
      )
  vn.wts <- 
    Thetas
  col.names <- 
    c('Y01','X00','X01','X02')
  colnames(df.var) <- 
    col.names[c(1:ncol(df.var))] # R specific syntax for assigning column
                                 # names. Yes, it's annoying, just learn it.
  df.var
  # Y01.norm <- Theta00.norm + Theta01.norm*X01.norm + Theta02.norm*X02.norm
}

# Unobservable Population
pop <- 1000000
set.seed(001) # Important for reproducibility, but otherwise optional
X01.norm <- 
  rnorm( # Create SRS...
    n = pop, # of sample size n...
    mean = X01.norm.mu, # from a Normal Gaussian with given mean...
    sd = X01.norm.sigma # and given standard deviation
    )
set.seed(002) # If set to 001 would be exactly the same sample as x01.norm
X02.norm <- 
  rnorm(
    n = pop, 
    mean = X02.norm.mu, 
    sd = X02.norm.sigma
    )

df.pop.2D <- 
  LinearCausalModel(
    Thetas = c(
      Theta00.norm,
      Theta01.norm,
      Theta02.norm
      ), # Thetas must be a vector
  Independents = 
    cbind( # Combine R objects by columns
      X00.norm,
      X01.norm,
      X02.norm
      ) # Independents must be a matrix
  ) 

head(
  x = df.pop.2D, 
  n = 10
  )

# Simple Random Sample
sample.size <- 100
set.seed(003)
df.samp.2D <- 
  df.pop.2D[sample( # Select a random sample from df.pop.2D...
    x = nrow(df.pop.2D), # limited to number of rows in df.pop.2D... 
    size = sample.size # with given sample size
    ), 
    ]

# Descriptive Statistics 
# Univariate Sample Statistics 2D
x01 <- 
  df.samp.2D[ ,3] # Subselects only the third column
x02 <- 
  df.samp.2D[ ,4] # Subselects only the fourth column
y01 <- 
  df.samp.2D[ ,1] # Subselects only the first column

library(psych) # for skew, kurtosi, cor.plot, etc.
library(lsr) # for average (mean) absolute deviation
library(moments) # for moment function, but second moment != var

FindMode <- 
  function(x) { # Calculates modal value
    uniqx <- 
      unique(x) 
    uniqx[
      which.max(
        tabulate(
          match(
            x, 
            uniqx
            )
          )
        )
      ]
    }


rms <- 
  function(x) { # Root Mean Square
    sqrt(
      mean(x^2)
      )
    }

trim.mean <- 
  function(x) { # Min-Max Trimmed Mean
    mean(
      subset(
        x = x, # Object to be subsetted
        subset = # Boolean subset conditions
          x != min(x) & # NOT the minimum value
          x != max(x) # NOT the maximum value
        )
      )
    }

QD <- 
  function(x){ # Quartile Deviation
    IQR(x = x) / 2
    }

quartile.skew <- 
  function(x) { # Quartile Skewness
    quarts <- 
      t(
        as.vector(
          quantile(
            x = x, 
            probs = seq(0.25, 0.75, 0.25)
            )
          )
        )
    (quarts[1] - 2 * quarts[2] + quarts[3]) / (quarts[3] - quarts[1])
    }

uni.desc.stat <- 
  function(x, var.name) {
    x.desc <- 
      cbind(
      # Univariate Descriptive Statistics of Location
        min(x),
        max(x),
        mean(x),
        median(x),
        FindMode(x),
        geometric.mean(x),
        harmonic.mean(x),
        rms(x),
        t(
          as.vector(
            quantile(
              x = x, 
              probs = seq(0.25, 0.75, 0.25)
              )
            )
          ), # Quartiles
        trim.mean(x),
        # Univariate Descriptive Statistics of Dispersion
        var(x),
        sd(x),
        aad(x), # Mean/Average Absolute Deviation
        mad(x), # Median Absolute Deviation
        IQR(x = x),
        QD(x),
        # Univariate Descriptive Statistics of Shape
        moment(
          x = x, 
          order = 1, 
          absolute = F
          ),
        moment(
          x = x, 
          order = 2, 
          absolute = F
          ), 
        moment(
          x = x, 
          order = 3, 
          absolute = F
          ),
        moment(
          x = x, 
          order = 4, 
          absolute = F
          ), 
        # https://en.wikipedia.org/wiki/Moment_(mathematics)
        skew(x), # Check how calculated relative to Wolfram definition
        kurtosi(x), # Check how calculated relative to Wolfram definition
        quartile.skew(x)
        )
        # This list is from: 
        # http://reference.wolfram.com/language/tutorial/DescriptiveStatistics.html
        # He also has Expectation in here but in a sense where Expectation is 
        # very similar to Estimation
  colnames(x.desc) <- 
    c(
      'Min',
      'Max',
      'Mean',
      'Median',
      'Mode',
      'GeoMean',
      'HarMean',
      'RootMeanSqr',
      '25%',
      '50%',
      '75%',
      'TrimMean',
      'Var',
      'StdDev',
      'AvgAbsDev',
      'MedAbsDev', 
      'IQR',
      'QuartileDev',
      'FirstMmnt',
      'SecondMmnt',
      'ThirdMmnt',
      'FourthMmnt',
      'Skew',
      'Kurtosis',
      'QuartSkew'
      )
  rownames(x.desc) <- 
    c(var.name)
  x.desc
  # There should be defensive programming in here for log(x) : NaNs produced
  }

x01.desc <-
  uni.desc.stat(
    x = x01, 
    var.name = 'x01'
    )

# Don't worry about Warning message: In log(x) : NaNs produced

x02.desc <-
  uni.desc.stat(
    x = x02, 
    var.name = 'x02'
    )

y01.desc <-
  uni.desc.stat(
    x = y01, 
    var.name = 'y01'
    )

df.desc.stats <- 
  as.data.frame(
    rbind(
      y01.desc,
      x01.desc,
      x02.desc
      )
    )

# Descriptive Statistics
# Sample Visualization
hist(
  x = x01,
  breaks = ((floor(min(x01)) * 5):(ceiling(max(x01)) * 5)) / 5,
  xlim = range(
    floor(min(x01)):ceiling(max(x01))
    ),
  main = 'Histogram of x01',
  xlab = 'x01'
  )

hist(
  x = x02,
  breaks = ((floor(min(x02)) * 5):(ceiling(max(x02)) * 5)) / 5,
  xlim = range(
    floor(min(x02)):ceiling(max(x02))
    ),
  main = 'Histogram of x02',
  xlab = 'x02'
  )

hist(
  x = y01,
  breaks = ((floor(min(y01)) * 5):(ceiling(max(y01)) * 5)) / 5,
  xlim = range(
    floor(min(y01)):ceiling(max(y01))
    ),
  main = 'Histogram of y01',
  xlab = 'y01'
  )

# Descriptive Statistics
# Univariate Exploratory Data Analysis (is Desc.Stat the parent of EDA or vice versa?)
str(x01)
typeof(x01)
x01.fivenum <- 
  t(
    as.data.frame(
      fivenum(x01)
      )
    )
colnames(x01.fivenum) <- 
  c(
    'min',
    'lower-hinge',
    'median',
    'upper-hinge',
    'max'
    )

str(x02)
typeof(x02)
x02.fivenum <- 
  t(
    as.data.frame(
      fivenum(x02)
      )
    )
colnames(x02.fivenum) <- 
  c(
    'min',
    'lower-hinge',
    'median',
    'upper-hinge',
    'max'
    )

str(y01)
typeof(y01)
y01.fivenum <- 
  t(
    as.data.frame(
      fivenum(y01)
      )
    )
colnames(y01.fivenum) <- 
  c(
    'min',
    'lower-hinge',
    'median',
    'upper-hinge',
    'max'
    )

boxplot(
  x = x01, 
  notch = T
  ) 
# Solid line = median
# Notch = CI around median
# Box bottom = 1st IQR
# Box Top = 3rd IQR
# Whiskers = Most extreme non-outlier values
# Circles = Outliers
boxplot(
  x = x02, 
  notch = T
  )
# Note changes in scale of y-axis on boxplot
boxplot(
  x = y01, 
  notch = T
  )

library(vioplot)

vioplot(
  x = x01, 
  names = 'x01'
  )
vioplot(
  x = x02, 
  names = 'x02'
  )
vioplot(
  x = y01, 
  names = 'y01'
  )

qqnorm(
  y = x01,
  main = 'Normal Q-Q Plot for x01'
  )
qqline(y = x01)

qqnorm(
  y = x02,
  main = 'Normal Q-Q Plot for x02'
  )
qqline(y = x02)

qqnorm(
  y = y01,
  main = 'Normal Q-Q Plot for y01'
  )
qqline(y = y01)

# Let's explore Q-Q Plots in more detail in the next script 2.1.2

rm(
  list = ls()
  ) # Clears global environment of all visible objects (some hidden objects
    # may remain)

# END

