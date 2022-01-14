# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 29th / AR(1) Phi_hat LS estimates distribution
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)

# Simulate an AR(1) process
sim_ar1 = function(phi, Tau, sgma2 = 1) {
  eps <- rnorm(Tau) * sqrt(sgma2)
  y <- rep(0, Tau)
  y[1] <- eps[1]
  
  for (t in 2:Tau){
    y[t] <- phi * y[t-1] + eps[t]
  }
  return(y)
}

# init
nsim <- 10000
Tau <- 100
Tau2 <- 1000
phis <- c(0.8, 0.9, 1.0) # AR(1) with phi=1 is a random walk

# if performance problems occur: 
# nsim <- 100
nsim_of_prcs1 <- matrix(ncol = Tau, nrow = nsim)
nsim_of_prcs2 <- matrix(ncol = Tau, nrow = nsim)
nsim_of_prcs3 <- matrix(ncol = Tau, nrow = nsim)
# start simulations
for(n in 0:nsim) nsim_of_prcs1[n, ] <- sim_ar1(phis[1], Tau)
for(n in 0:nsim) nsim_of_prcs2[n, ] <- sim_ar1(phis[2], Tau)
for(n in 0:nsim) nsim_of_prcs3[n, ] <- sim_ar1(phis[3], Tau)

# LS estimation of phi_hat
phi_hats_1 <- c()
for(n in 1:nsim) phi_hats_1 <- c(phi_hats_1,ar(nsim_of_prcs1[n, ], order.max = 1, method = "ols")$ar) # uses ordinary least-squares for estimation
head(phi_hats_1, 10)
tail(phi_hats_1, 10)

phi_hats_2 <- c()
for(n in 1:nsim) phi_hats_2 <- c(phi_hats_2,ar(nsim_of_prcs2[n, ], order.max = 1, method = "ols")$ar) # uses ordinary least-squares for estimation
head(phi_hats_2, 10)
tail(phi_hats_2, 10)

phi_hats_3 <- c()
for(n in 1:nsim) phi_hats_3 <- c(phi_hats_3,ar(nsim_of_prcs3[n, ], order.max = 1, method = "ols")$ar) # uses ordinary least-squares for estimation
head(phi_hats_3, 10)
tail(phi_hats_3, 10)

hist(phi_hats_1)
hist(phi_hats_2)
hist(phi_hats_3)
# => it is already very slow so i'll not use Tau2 = 1000



