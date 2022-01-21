# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 38th / Simulation of ARCH(2) processes
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)

install.packages("fGarch")
require(fGarch)

Tau <- 10000
alpha0 <- 0.05
alpha1 <- 0.6
alpha2 <- 0.2

spec = garchSpec(model = list(omega = alpha0, alpha = c(alpha1, alpha2), beta = 0))
arch2_sim <- garchSim(spec, n = Tau)

plot(arch2_sim)
acf(arch2_sim)
acf(arch2_sim^2)
arch2 <- garch(arch2_sim, order = c(0,2))
summary(arch2)


# Tau <- 100
plot(arch2_sim[1:100],type='l')
acf(arch2_sim[1:100])
acf(arch2_sim[1:100]^2)
arch2 <- garch(arch2_sim[1:100], order = c(0,2))
summary(arch2)

# Tau <- 1000
plot(arch2_sim[1:1000],type='l')
acf(arch2_sim[1:1000])
acf(arch2_sim[1:1000]^2)
arch2 <- garch(arch2_sim[1:1000], order = c(0,2))
summary(arch2)



