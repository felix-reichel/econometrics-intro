# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 22th / AR(1) Parameter estimation
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)
source("https://raw.githubusercontent.com/robjhyndman/forecast/master/R/armaroots.R")

Tau <- 100000
sgma2 <- 1
phi <- .6
mu <- 10 # where is the unconditional mean

Tau_subset <- c(50, 100, 1000, 10000)

set.seed(2345)

ARIMA_order <- c(1,0,0)
ts.sim <- mu + arima.sim(list(order = ARIMA_order, ar = c(phi)), n = Tau, sd = sgma2)

plot(ts.sim[1:100], type = "l")


# Estimate the parameters using Yule-Walker equations, OLS, MLE
ar(ts.sim[1:Tau_subset[1]], method = "yule-walker")
plot(ar(ts.sim[1:Tau_subset[1]], method = "yule-walker"))

ar(ts.sim[1:Tau_subset[1]], method = "ols")
plot(ar(ts.sim[1:Tau_subset[1]], method = "ols"))

ar(ts.sim[1:Tau_subset[1]], method = "mle")
plot(ar(ts.sim[1:Tau_subset[1]], method = "mle"))


for (idx in 2:4) {
  ar(ts.sim[1:Tau_subset[idx]], method = "yule-walker")
  ar(ts.sim[1:Tau_subset[idx]], method = "ols")
  ar(ts.sim[1:Tau_subset[idx]], method = "mle")
}


x1 <- 2:50 # yt to yT*1

ts.sim.demeaned = ts.sim[x1] - mu
plot(ts.sim.demeaned[x1], type ="l")

lm = lm(x1 ~ ts.sim.demeaned[x1])
lm
