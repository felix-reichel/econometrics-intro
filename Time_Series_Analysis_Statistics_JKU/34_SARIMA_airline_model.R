# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 34th / Simulation of SARIMA(0,1,1) x (0,1,1)_4 model
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)

Tau <- 1000
theta <- .6
THETA <- .3
sgma <- .1
sgma2 <- sgma^2

freq <- 4
y_seas <- c(4, 2.5, 3, 1.5)
# create seasonal dummies
s <- rep(y_seas, Tau/freq)
xh <- C(as.factor(s), contr.sum)
x <- cbind(model.matrix(~xh), 1:Tau)

# simulation of SARIMA(0,1,1)x(0,1,1)_4
tsplot(sarima.sim(d = 1, ma = theta, D = 1, sma = THETA, S = 4, n = Tau))

       