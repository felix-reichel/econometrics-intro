# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: UNDONE - 29th / AR(1) Phi_hat LS estimates distribution
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
phis <- c(0.8, 0.9, 1.0) # AR(1) with phi=1 is a random walk
Tau2 <- 1000


# if performance problems occur: 
nsim_test <- 100
nsim <- nsim_test

nsim_of_prcs1 <- matrix(ncol = Tau, nrow = nsim)

# start simulations
for(n in 0:nsim) {
  prcs <- sim_ar1(phis[1], Tau)
  nsim_of_prcs1[n, ] <- prcs
}

# LS estimation of phi_hat
ARIMA_AR_1 = c(1,0,0)

phi_hats_of_prcs1 <- matrix(nrow = nsim, ncol = 1)

for(n in 1:nsim) {
  arima_model <- arima(nsim_of_prcs1[n, ], order = ARIMA_AR_1, method = "CSS") # use conditional-sum-of-squares
  phi_hats_of_prcs1[n, ]  <- arima_model$coef[1]
}

# plot distribution of phi_hat
hist(phi_hats_of_prcs1[,1])


