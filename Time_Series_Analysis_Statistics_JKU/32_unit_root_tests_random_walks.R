# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 32th / Unit root tests Random Walk /with drift,
#                 AR(1) model fit
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)

# init
nsim <- 10000
Tau <- 100

set.seed(2345)

# functions
sim_rnd_wlk = function(Tau, sgma2) {
  return(cumsum(rnorm(Tau, sd=sgma2))) 
}
sim_rnd_wlk_drft = function(Tau, sgma2, drift) {
  return(cumsum(rnorm(Tau, sd=sgma2)) + cumsum(rep(drift, Tau))) 
}

# start simulations of stochastic processes
rnd_wlks <- matrix(nrow = nsim, ncol = Tau)
for(n in 1:nsim) rnd_wlks[n,] <- sim_rnd_wlk(Tau, 1)
plot(rnd_wlks[3,], type='l')

rnd_wlks_drft <- matrix(nrow = nsim, ncol = Tau)
for(n in 1:nsim) rnd_wlks_drft[n,] <- sim_rnd_wlk_drft(Tau, 1, rnorm(1))
plot(rnd_wlks_drft[3,], type='l')

# Plot distributions of phi_hats
phi_hats_1 <- c()
for(n in 1:nsim) 
  phi_hats_1 <- c(phi_hats_1,ar(rnd_wlks[n, ], order.max = 1)$ar) # uses yule-walker for estimation
hist(phi_hats_1)

phi_hats_2 <- c()
for(n in 1:nsim) 
  phi_hats_2 <- c(phi_hats_2, ar(rnd_wlks_drft[n, ], order.max = 1)$ar) # uses yule-walker for estimation
hist(phi_hats_2)

# Because phi = 1, it can be shown that for rnd. walks are non-stationary, 
# because the variance increases with time.

defaultW <- getOption("warn") 
options(warn = -1) 

alpha <- .05 # significance lvl
# Number where Augmented Dickey-Fuller test (falsely) rejects H0
x <- 0
for(n in 1:nsim) if (adf.test(rnd_wlks[n, ])$p.value > alpha) x <- x+1 
print(nsim - x) #  nsim - 9573 (falsely) rejects H0
#  9573 times x > 0.05 => accepts H0 => non-stationary

# Number where KPSS test (correctly) rejects H0
x <- 0
for(n in 1:nsim) if (kpss.test(rnd_wlks[n, ])$p.value > alpha) x <- x+1 
print(nsim - x) # nsim - 1796 times x > 0.05 => (correctly) rejects H0
# 1796 times x > 0.05 => H0 => Has level stationary



# Number where Augmented Dickey-Fuller test (falsely) rejects H0
x <- 0
for(n in 1:nsim) if (adf.test(rnd_wlks_drft[n, ])$p.value > alpha) x <- x+1 
print(nsim - x) #  nsim - 9547 (falsely) rejects H0
#  9547 times x > 0.05 => accepts H0 => non-stationary

# Number where KPSS test (correctly) rejects H0
x <- 0
for(n in 1:nsim) if (kpss.test(rnd_wlks_drft[n, ])$p.value > alpha) x <- x+1 
print(nsim - x) # nsim - 208 correctly rejects H0 where p-Value < alpha 
# 208 times x > 0.05 => H0 => Has level stationary

options(warn = defaultW)


