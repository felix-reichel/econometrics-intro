# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 25th / Order of MA(q) processes
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)
source("https://raw.githubusercontent.com/robjhyndman/forecast/master/R/armaroots.R")

Tau <- 1000
sgma <- 1

# MA(1)
q1 <- 1
thetas <- c(0.3, -0.5, 0.7, 0.97)

# MA(q) is invertible if MA(q) -> AR(Inf.) convergers
# meaning the unit roots of the characteristic polynomial have to be
# outside the unit circle.
# MA(1): the MA(1) process is invertible if |θ| < 1
ts.sim1 <- arima.sim(list(order = c(0,0,q1), ma = thetas[1], sgma = sgma), n = Tau)
plot(ts.sim1)
acf(ts.sim1)

ts.sim2 <- arima.sim(list(order = c(0,0,q1), ma = thetas[2], sgma = sgma), n = Tau)
plot(ts.sim2)
acf(ts.sim2)

ts.sim3 <- arima.sim(list(order = c(0,0,q1), ma = thetas[3], sgma = sgma), n = Tau)
plot(ts.sim3)
acf(ts.sim3)

ts.sim4 <- arima.sim(list(order = c(0,0,q1), ma = thetas[4], sgma = sgma), n = Tau)
plot(ts.sim4)
acf(ts.sim4)
# ACF cuts off at q = Lag = 1

# MA(2)
q2 <- 2
ARIMA_0_0_2 <- c(0, 0, q2)
thetas <- c(1.5, -0.8)
ts.sim <- arima.sim(list(order = ARIMA_0_0_2, ma = thetas, sgma = sgma), n = Tau)
ts.plot(ts.sim)
acf(ts.sim)
pacf(ts.sim)
ts.sim <- arima.sim(list(order = ARIMA_0_0_2, ma = thetas, sgma = sgma), n = Tau)
arima_model <- arima(ts.sim, order = ARIMA_0_0_2, fixed = thetas, include.mean = FALSE)
plot.armaroots(maroots(arima_model), xlab="x", ylab="y")

# MA(5)
ARIMA_0_0_5 <- c(0,0,5)
set.seed(2345)
thetas <- c(0.9, 1.4, 0.2, 0.7, 0.1)
ts.sim <- arima.sim(list(order = ARIMA_0_0_5, ma=thetas, sgma = sgma), n = Tau)
ts.plot(ts.sim)
acf(ts.sim)
pacf(ts.sim)
ts.sim <- arima.sim(list(order = ARIMA_0_0_5, ma=thetas, sgma = sgma), n = Tau)
arima_model <- arima(ts.sim, order = ARIMA_0_0_5, fixed = thetas, include.mean = FALSE)
plot.armaroots(maroots(arima_model), xlab="x", ylab="y")
