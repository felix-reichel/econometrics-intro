# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 40th / NYSE - ARCH/GARCH Volatility models
# Author: Felix Reichel
# ---------------------------------------------------------
require(fGarch)
require(astsa)
require(tseries)
require(forecast)

ts_nyse <- ts(nyse[2])

plot(ts_nyse)
acf(ts_nyse, lag.max = 100)
acf(ts_nyse^2,lag.max = 100)

pacf(ts_nyse)
pacf(ts_nyse^2) # has a single spike at lag 1 suggesting an AR(1) model for the squared series.


# Fit ARCH(r) processes of different order to NYSE
nyse.arch4 <- garch(ts_nyse, order = c(0,4))
nyse.arch4
summary(nyse.arch4)
plot(nyse.arch4)

nyse.arch5 <- garch(ts_nyse, order = c(0,5))
nyse.arch5
summary(nyse.arch5)
plot(nyse.arch5)

nyse.arch6 <- garch(ts_nyse, order = c(0,6))
nyse.arch6
summary(nyse.arch6)
plot(nyse.arch6)

# Fit GARCH(1,1) process to NYSE
nyse.garch11 <- garch(ts_nyse, order = c(1,1))
nyse.garch11
summary(nyse.garch11)
plot(nyse.garch11)


