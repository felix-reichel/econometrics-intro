# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 21th / AR(p) processes
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)

Tau <- 100
AR_order <- c(1, 2, 5)
phis <- c(0.3, -0.5, 0.7, 0.97)

ARIMA_AR_order_1 <- c(AR_order[1], 0, 0)

for (phi in phis) {
  ts.sim <- arima.sim(list(order = ARIMA_AR_order_1, ar = phi), n = Tau)
  ts.plot(ts.sim)
  acf(ts.sim)
  pacf(ts.sim) # The PACF cuts off at lag 1 for AR(1) processes
  # AR(1) process is stationary and causal only if |φ| < 1, which is the case given phis.
}

ARIMA_AR_order_2 <- c(AR_order[2], 0, 0)
ts.sim <- arima.sim(list(order = ARIMA_AR_order_2, ar = c(0.3, -0.5)), n = Tau)
ts.plot(ts.sim)
acf(ts.sim)
pacf(ts.sim) # The PACF cuts off at lag 2 for an AR(2) process

# Causality and Stationarity can be determined by calculation of the unit roots of the characteristic polynomial,
# where:
# I: φ(z) = 1 - φ1*z - ... - φp*z = 0 => |z| <> 1 if a unique stationary solution exists
# II: φ(z) = 1 - φ1*z - ... - φp*z = 0 => |z| > 1 if the process if causal

require(plotrix)

phi1 <- 0.3
phi2 <- -0.5
zx_result <- NA
zy_result <- NA

discr <- phi1 ^ 2 + 4 * phi2
if (discr >= 0) {
  zx_result <- c(-phi1+sqrt(discr), -phi1-sqrt(discr))/(2*phi2)
  zy_result <- rep(0,2)
} else {
  zx_result <- rep(-phi1/(2*phi2),2)
  zy_result <- c(1,-1)*(sqrt(-discr)/(2*phi2))
}
plot(c(-3.5, 3.5), c(-3.5,3.5), type = "n", xlab="x", ylab="y")
draw.circle(x=0, y=0, radius=1, border="blue")
points(zx_result, zy_result, pch=20) # => Inside the unit circle and therefore not casual


ARIMA_AR_order_5 <- c(AR_order[3], 0, 0)
ts.sim <- arima.sim(list(order = ARIMA_AR_order_5, ar = c(0.3, 0.2, 0.2, -0.2, 0.1)), n = Tau)
ts.plot(ts.sim)
acf(ts.sim)
pacf(ts.sim) # The PACF cuts off at lag 5 for an AR(5) process

# Causality
z <- polyroot(c(1, 0.3, 0.2, 0.2, -0.2, 0.1))
z         # |z| > 1 if the process if causal
Mod(z)

