# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 22th / AR(1) Parameter estimation
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)
source("https://raw.githubusercontent.com/robjhyndman/forecast/master/R/armaroots.R")

Tau <- 100000
Tau_subset <- c(50, 100, 1000, 10000)
sgma2 <- 1
phi <- .6
mu <- 10 # unconditional mean

set.seed(2345)

AR_1 <- c(1,0,0)
AR_1.sim <- mu + arima.sim(list(order = AR_1, ar = c(phi)), n = Tau, sd = sgma2)
plot(AR_1.sim[1:100], type = "l")

ar(AR_1.sim[1:Tau_subset[1]], method = "yule-walker")
plot(ar(AR_1.sim[1:Tau_subset[1]], method = "yule-walker"))
ar(AR_1.sim[1:Tau_subset[1]], method = "ols")
plot(ar(AR_1.sim[1:Tau_subset[1]], method = "ols"))
ar(AR_1.sim[1:Tau_subset[1]], method = "mle")
plot(ar(AR_1.sim[1:Tau_subset[1]], method = "mle"))

# No for-loop just for pdf knitting:
ar(ts.sim[1:Tau_subset[2]], method = "yule-walker")
ar(ts.sim[1:Tau_subset[2]], method = "ols")
ar(ts.sim[1:Tau_subset[2]], method = "mle")

ar(ts.sim[1:Tau_subset[3]], method = "yule-walker")
ar(ts.sim[1:Tau_subset[3]], method = "ols")
ar(ts.sim[1:Tau_subset[3]], method = "mle")


ar(AR_1.sim[1:Tau_subset[4]], method = "yule-walker")
plot(ar(AR_1.sim[1:Tau_subset[4]], method = "yule-walker"))
ar(AR_1.sim[1:Tau_subset[4]], method = "ols")
plot(ar(AR_1.sim[1:Tau_subset[4]], method = "ols"))
ar(AR_1.sim[1:Tau_subset[4]], method = "mle")
plot(ar(AR_1.sim[1:Tau_subset[4]], method = "mle"))


# No for-loop just for pdf knitting:
t <- 2:Tau_subset[1] # yt to yT*1
AR_1.sim.demeaned = AR_1.sim[t] - mu
lm = lm(AR_1.sim.demeaned[t] ~ t)
summary(lm)
lm$coefficients

t <- 2:Tau_subset[2] # yt to yT*1
AR_1.sim.demeaned = AR_1.sim[t] - mu
lm = lm(AR_1.sim.demeaned[t] ~ t)
summary(lm)
lm$coefficients

t <- 2:Tau_subset[3] # yt to yT*1
AR_1.sim.demeaned = AR_1.sim[t] - mu
lm = lm(AR_1.sim.demeaned[t] ~ t)
summary(lm)
lm$coefficients

t <- 2:Tau_subset[4] # yt to yT*1
AR_1.sim.demeaned = AR_1.sim[t] - mu
lm = lm(AR_1.sim.demeaned[t] ~ t)
summary(lm)
lm$coefficients
