# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 26th / ARMA(p,q) processes
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)
source("https://raw.githubusercontent.com/robjhyndman/forecast/master/R/armaroots.R")

Tau <- 200
sgma <- 1

# p = 1, q = 1 with ϕ = 0.6 and θ = 0.2
ARIMA_1_0_1 <- c(1,0,1)
phi <- 0.6
theta <- 0.2
ts.sim1 <- arima.sim(list(order = ARIMA_1_0_1, ar = phi, ma = theta, sgma = sgma), n = Tau)
plot(ts.sim1)
arima_model <- arima(ts.sim1, order = ARIMA_1_0_1, fixed = c(phi,theta), include.mean = FALSE)
# causal if inverse ar-roots are inside of the unit circle
plot.armaroots(arroots(arima_model), xlab="x", ylab="y")
# invertible if inverse ma-roots are inside of the unit circle
plot.armaroots(maroots(arima_model), xlab="x", ylab="y")
acf(ts.sim1) # ~ decays exponentially 
pacf(ts.sim1) # ~ cuts of at Lag of 1
# AR(1) for a pure AR(p) process
# Fitting
fit1 <- arima(ts.sim1, order = c(1,0,0), include.mean = FALSE) # phi = 0.6419
ts.fit1 <- arima.sim(list(order = c(1,0,0), ar = c(fit1$coef), sgma = fit1$sigma2), n = Tau)
plot(ts.sim1)
lines(ts.fit1, col = "RED")

arma(ts.sim1, order = c(1,0), include.intercept = FALSE) # ϕ = 0.645

# p = 2, q = 0 with ϕ1 = 0.6 and ϕ2 = 0.2
ARIMA_2_0_0 <- c(2,0,0)
phis <- c(0.6, 0.2)
ts.sim2 <- arima.sim(list(order = ARIMA_2_0_0, ar = phis, sgma = sgma), n = Tau)
plot(ts.sim2)
arima_model <- arima(ts.sim2, order = ARIMA_2_0_0, fixed = c(phis), include.mean = FALSE)
plot.armaroots(arroots(arima_model), xlab="x", ylab="y")
acf(ts.sim2)
pacf(ts.sim2)
# AR(2) for a pure AR(p) process
# Fitting
arima(ts.sim1, order = c(2,0,0), include.mean = FALSE) # ϕ1 = 0.7333, ϕ2 = -0.1411
arma(ts.sim1, order = c(2,0), include.intercept = FALSE)

# p = 0, q = 2 with θ1 = 0.6 and θ2 = 0.2
ARIMA_0_0_2 <- c(0,0,2)
thetas <- c(0.6, 0.2)
ts.sim3 <- arima.sim(list(order = ARIMA_0_0_2, ma = thetas, sgma = sgma), n = Tau)
plot(ts.sim3)
arima_model <- arima(ts.sim3, order = ARIMA_0_0_2, fixed = c(thetas), include.mean = FALSE)
plot.armaroots(maroots(arima_model), xlab="x", ylab="y")
acf(ts.sim3) # cuts of at Lag = q = 2
pacf(ts.sim3) # decays exponentially
# MA(2) for a pure MA(q) process
arima(ts.sim1, order = c(0,0,2), include.mean = FALSE) # θ1 = 0.7225, θ2 = 0.3187
arma(ts.sim1, order = c(0,2), include.intercept = FALSE)

