# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 39th / Simulation of GARCH(1,1) processes
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)
require(fGarch)

Tau <- 10000
alpha0 <- 0.05
alpha1 <- 0.6
gamma <- 0.2

# GARCH(1,1) - specify omega/alpha/beta
spec = garchSpec(model = list(omega = alpha0, alpha = alpha1, beta = gamma))
garch1_1_sim <- garchSim(spec, n = Tau)

plot(garch1_1_sim)
acf(garch1_1_sim)
acf(garch1_1_sim^2)
garch1_1 <- garch(garch1_1_sim)
summary(garch1_1)

# Tau <- 100
plot(garch1_1_sim[1:100], type='l')
acf(garch1_1_sim[1:100])
acf(garch1_1_sim[1:100]^2)
garch1_1 <- garch(garch1_1_sim[1:100])
summary(garch1_1)

# Tau <- 1000
plot(garch1_1_sim[1:1000], type='l')
acf(garch1_1_sim[1:1000])
acf(garch1_1_sim[1:1000]^2)
garch1_1 <- garch(garch1_1_sim[1:1000])
summary(garch1_1)


