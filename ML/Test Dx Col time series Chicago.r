####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# ts() function from R core library can convert a numeric vector into time 
# series.

#wrkng.path <- 'H:/DSU/MLM/R Code/'


install.packages('tseries', dependencies=T)
library(tseries)

install.packages('forecast', dependencies=T)
library(forecast)



DX_Col_Chi  <- c(  900,  964,  1031,  861,  1035,  1015,  930,  1019,  1149,  914,
                   1080,  1049,  943,  1011,  1096,  990,  1135,  1150,  1070,  910,
                   982,  1009,  1149,  1138,  987,  994,  1149,  1089,  1032,  1307,
                   1060,  1116,  1292,  1301,  1321,  1364  
)


ts.DX_Col_Chi <- ts(data = DX_Col_Chi, start=c(2015,1), frequency = 12)



#data(ts.DX_Col_Chi)
str(ts.DX_Col_Chi)
ts.DX_Col_Chi


# Plot time series data
png(
  file = paste(
    wrkng.path,
    'DIAG_COL_CHICAGO_GI_PD11.png',
    sep = ''
  ), 
  width = 800, 
  height = 800
)

tsdisplay(
  x = ts.DX_Col_Chi, 
  cex.lab = 1.5, 
  cex.main = 1.5, 
  cex = 1.5
)

dev.off()

# You will need to zoom to see the plot below or else open the .png file

tsdisplay(
  x = ts.DX_Col_Chi, 
  cex.lab = 1.5, 
  cex.main = 1.5, 
  cex = 1.5
)

png(
  file = paste(
    wrkng.path,
    'chi_monthplot.png',
    sep = ''
  ), 
  width = 800, 
  height = 800
)

monthplot(
  x = ts.DX_Col_Chi, 
  cex.lab = 1.5, 
  cex.main = 1.5, 
  cex = 1.5
)

dev.off()

monthplot(
  x = ts.DX_Col_Chi, 
  cex.lab = 1.5, 
  cex.main = 1.5, 
  cex = 1.5
)

png(
  file = paste(
    wrkng.path,
    'chi_seasonplot.png',
    sep = ''
  ), 
  width = 800, 
  height = 800
)

seasonplot(
  x = ts.DX_Col_Chi, 
  cex.lab = 1.5, 
  cex.main = 1.5, 
  cex = 1.5,
  ylab = 'DX_Col_Chi'
)

dev.off()

seasonplot(
  x = ts.DX_Col_Chi, 
  cex.lab = 1.5, 
  cex.main = 1.5, 
  cex = 1.5,
  ylab = 'DX_Col_Chi'
)

# Decompose time series into trend, seasonality and remainder using functions 
# decompose() or stl().

png(
  file = paste(
    wrkng.path,
    'chi_decompose_additive.png',
    sep = ''
  ), 
  width = 800, 
  height = 800
)

parts.add <- 
  decompose(
    x = ts.DX_Col_Chi, 
    type = 'additive' # y = Trend + Cycle + Seasonal + Noise
  )

plot( # plot.decompose.ts
  x = parts.add, # the plot() is smart enough to 'understand' this is a 
  # decomposition of a time-series
  cex.lab = 1.5, 
  cex.main = 1.5, 
  cex = 1.5
)

dev.off()

png(
  file = paste(
    wrkng.path,
    'chi_decompose_multiplicative.png',
    sep = ''
  ), 
  width = 800, 
  height = 800
)

parts.mult <- 
  decompose(
    x = ts.DX_Col_Chi, 
    type = 'multiplicative' # y = Trend * Cycle * Seasonal * Noise
  )

plot(
  x = parts.mult, # the plot() is smart enough to 'understand' this is a time-series
  cex.lab = 1.5, 
  cex.main = 1.5, 
  cex = 1.5
)

# Note what changes...

# The y-axis and the residuals (random portion)!

dev.off()

parts.mult <- 
  decompose(
    x = ts.DX_Col_Chi, 
    type = 'multiplicative' # y = Trend * Cycle * Seasonal * Noise
  )

plot(
  x = parts.mult, # the plot() is smart enough to 'understand' this is a time-series
  cex.lab = 1.5, 
  cex.main = 1.5, 
  cex = 1.5
)

png(
  file = paste(
    wrkng.path,
    'chi_stl.png',
    sep = ''
  ), 
  width = 800, 
  height = 800
)

parts.stl <- 
  stl( # Seasonal Decomposition of Time Series by Loess
    x = ts.DX_Col_Chi, 
    s.window = 'periodic'
  )

plot(
  x = parts.stl, 
  cex.lab = 1.5, 
  cex.main = 1.5, 
  cex = 1.5
)

# More R weirdness, not unique to R but more common in open source than in 
# commercial. Where did the chart and axis labels go? Why did the
# decompositions change order? Why the change in residual formatting?
# Especially as residual now looks like an ACF or PACF...

dev.off()

parts.stl <- 
  stl( # Seasonal Decomposition of Time Series by Loess
    x = ts.DX_Col_Chi, 
    s.window = 'periodic'
  )

plot(
  x = parts.stl, 
  cex.lab = 1.5, 
  cex.main = 1.5, 
  cex = 1.5
)

# De-seasonalize a time series.

png(
  file = paste(
    wrkng.path,
    'chi_de_seasonalize.png',
    sep = ''
  ), 
  width = 800, 
  height = 800
)

par(mfrow = c(3, 1)) # Because we are going to put 3 graphs in one file

parts.stl.sa <- 
  seasadj(
    object = parts.stl
  )

plot(
  x = ts.DX_Col_Chi, 
  type = 'l', 
  main = 'DX_Col_Chi original', 
  cex.lab = 1.5, 
  cex.main = 1.5, 
  cex = 1.5
) 

plot(
  x = parts.stl.sa, 
  type = 'l', 
  main = 'DX_Col_Chi season adjusted', 
  cex.lab = 1.5, 
  cex.main = 1.5, 
  cex = 1.5
)

seasonplot(
  x = parts.stl.sa, 
  s = 12, # Seasonal Frequency 
  col = rainbow(12), 
  year.labels = T, 
  main = 'DX_Col_Chi seasonal plot', 
  cex.lab = 2
) 

dev.off()

parts.stl.sa <- 
  seasadj(
    object = parts.stl
  )

plot(
  x = DX_Col_Chi, 
  type = 'l', 
  main = 'DX_Col_Chi original', 
  cex.lab = 1.5, 
  cex.main = 1.5, 
  cex = 1.5
) 

plot(
  x = parts.stl.sa, 
  type = 'l', 
  main = 'DX_Col_Chi season adjusted', 
  cex.lab = 1.5, 
  cex.main = 1.5, 
  cex = 1.5
)

seasonplot(
  x = parts.stl.sa, 
  s = 12, # Seasonal Frequency 
  col = rainbow(12), 
  year.labels = T, 
  main = 'DX_Col_Chi seasonal plot', 
  cex.lab = 2
) 

# Test if a time series is stationary

adf.test( # Augmented Dickey-Fuller Test
  x = ts.DX_Col_Chi, 
  alternative = 'stationary', # 'explosive'
  k = trunc((length(ts.DX_Col_Chi)-1)^(1/3)) # Lag Order
)

# p-value = 0.01 suggests rejection of Null Hypothesis that wineind is 
# auto-correlated with lag order (k) = 5 and is therefore consistent with the 
# alternative  hypothesis that wineind is stationary for this lag order. The
# default formula for k is: trunc((length(x)-1)^(1/3))

adf.test( # Augmented Dickey-Fuller Test
  x = ts.DX_Col_Chi, 
  alternative = 'stationary', # 'explosive'
  k = 12 # Lag order
)

# As expected, consistent with rejection of hypothesis that wineind is 
# stationary for a lag order of 12 (yearly)

# Kwiatkowski-Phillips-Schmidt-Shin Test for Stationarity

kpss.test(
  ts.DX_Col_Chi, 
  null = 'Level', 
  lshort = T
)

# p-value = 0.01 suggests rejection of Null Hypothesis that wineind is 
# auto-correlated with lag order (k) = 3 and is therefore consistent with the 
# alternative  hypothesis that wineind is stationary for this lag order.
# The lshort = T formula for k is:  trunc(3*sqrt(length(x))/13).

kpss.test(
  ts.DX_Col_Chi, 
  null = 'Level', 
  lshort = F
)

# p-value = 0.01254 suggests rejection of Null Hypothesis that wineind is 
# auto-correlated with lag order (k) = 9 and is therefore consistent with the 
# alternative  hypothesis that wineind is stationary for this lag order.
# The lshort = F formula for k is: trunc(10*sqrt(n)/14)

# Compute and plot auto-correlation and partial auto-correlation.

png(
  file = paste(
    wrkng.path,
    'chi_Acf.png',
    sep = ''
  ), 
  width = 800, 
  height = 800
)

Acf( # Remember case-sensitivity!
  x = ts.DX_Col_Chi, 
  cex.lab = 1.5, 
  cex.main = 1.5, 
  cex = 1.5
)

dev.off()

Acf( # Remember case-sensitivity!
  x = ts.DX_Col_Chi, 
  cex.lab = 1.5, 
  cex.main = 1.5, 
  cex = 1.5
)

# Note implicit in the Acf() function is a print command

png(
  file = paste(
    wrkng.path,
    'chi_Pacf.png',
    sep = ''
  ), 
  width = 800, 
  height = 800
)

Pacf(
  x = ts.DX_Col_Chi, 
  cex.lab = 1.5, 
  cex.main = 1.5, 
  cex = 1.5
)

dev.off()

# Spikes that cross horizontal blue lines are the lags significantly correlated 
# with current series.

####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+
# ARIMA model

fit.arima <- 
  auto.arima(
    y = ts.DX_Col_Chi
  )

summary(fit.arima)
str(fit.arima)

# Predictive accuracy
accuracy(fit.arima)

# Check normality of residuals from fitting.

png(
  file = paste(
    wrkng.path,
    'chi_ARIMA_residuals.png',
    sep = ''
  ), 
  width = 800, 
  height = 800
)

qqnorm(
  residuals(fit.arima), 
  cex.lab = 1.5, 
  cex.main = 1.5, 
  cex = 1.5, 
  main = 'ARIMA model residuals normal Q-Q plot'
)
qqline(
  residuals(fit.arima)
)

# As always, a great fit +/- 2 quantiles!

dev.off()

# Predict next 10 observations
forecast( # forecast.Arima
  object = fit.arima, 
  h = 10 # Nbr of periods for forecasting
)

png(
  file = paste(
    wrkng.path,
    'chi_ARIMA_forecast_10.png',
    sep = ''
  ), 
  width = 800, 
  height = 800
)



plot(
  forecast(
    object = fit.arima, 
    h = 10
  ), 
  cex.lab = 1.5, 
  cex.main = 1.5, 
  cex = 1.5
)

dev.off()

####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+
# GARCH model

fit.garch <- 
  garch(
    x = ts.DX_Col_Chi, 
    grad = 'numerical', 
    trace = F
  )

summary(fit.garch) # Note model is broken! Always stop to look and think!
str(fit.garch)

coef(fit.garch)

#png(
#  file = paste(
#    wrkng.path,
#    'chi_GARCH.png',
#    sep = ''
#    ), 
#  width = 800, 
#  height = 800
#  )

#plot(
#  object = fit.garch, 
#  ask = F
#  )

#dev.off()

# Check normality of residuals from fitting.
#png(
#  file = paste(
#    wrkng.path,
#    'chi_GARCH_residuals.png',
#    sep = ''
#    ), 
# width = 800, 
#  height = 800
#  )

#qqnorm(
#  residuals(fit.garch), 
#  cex.lab = 1.5, 
#  cex.main = 1.5, 
#  cex = 1.5, 
#  main = 'GARCH model residuals normal Q-Q plot'
#  )
#qqline(
#  residuals(fit.garch)
#  )

#dev.off()

#rm(
#  list = ls()
#)

# End
