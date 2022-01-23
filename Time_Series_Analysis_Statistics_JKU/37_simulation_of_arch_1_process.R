# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 37th / Simulation of ARCH(1) processes
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)
require(fGarch)

Tau <- 1000
alpha0 <- 0.05
alphas1 <- c(.3, .6, .9)

# ARCH(1):
# sigma_t ^ 2 = alpha0 + alpha1 * Y_t-1 ^ 2

spec = garchSpec(model = list(omega = alpha0, alpha = c(alphas1[1]), beta = 0))
arch1_sim1 <- garchSim(spec, n = Tau)
plot(arch1_sim1)
acf(arch1_sim1)
acf(arch1_sim1^2)

arch_model_estimated <- garch(arch1_sim1)
summary(arch_model_estimated) # Model: GARCH(1,1)

arch1 <- garch(arch1_sim1, order = c(0,1))
summary(arch1)
#
# Coefficient(s):
#  Estimate  Std. Error     t value   Pr(>|t|)    
#  a0  0.051779    0.003383   15.307  < 2e-16 ***
#  a1  0.292909    0.050348    5.818 5.97e-09 ***

spec = garchSpec(model = list(omega = alpha0, alpha = c(alphas1[2]), beta = 0))
arch1_sim2 <- garchSim(spec, n = Tau)
plot(arch1_sim2)
acf(arch1_sim2)
acf(arch1_sim2^2)

arch_model_estimated <- garch(arch1_sim2)
summary(arch_model_estimated) # Model: GARCH(1,1)

arch1 <- garch(arch1_sim2, order = c(0,1))
summary(arch1)
# Coefficient(s):
#   Estimate  Std. Error  t value Pr(>|t|)    
#  a0  0.041336    0.003104   13.319   <2e-16 ***
#  a1  0.597669    0.066912    8.932   <2e-16 ***

spec = garchSpec(model = list(omega = alpha0, alpha = c(alphas1[3]), beta = 0))
arch1_sim2 <- garchSim(spec, n = Tau)
plot(arch1_sim2)
acf(arch1_sim2)
acf(arch1_sim2^2)
      
arch_model_estimated <- garch(arch1_sim2)
summary(arch_model_estimated) # Model: GARCH(1,1)2)

arch1 <- garch(arch1_sim2, order = c(0,1))
summary(arch1)

#
# Coefficient(s):
#     Estimate  Std. Error  t value Pr(>|t|)    
#  a0  0.051881    0.004365    11.89   <2e-16 ***
#  a1  0.898388    0.081470    11.03   <2e-16 ***



