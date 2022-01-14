# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 31th / KPSS test
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)
# init 
nsim <- 10
Tau <- 100
xi <- c(.1, 0, .1)
sgma2_ut <- c(0, .25, .25)

######## FUNCTIONS #########################################
# The underlying model is : 
# Y_t = xi * t + Z_t-1 + u_t + e_t  
# ...xi * t -> linear trend
# ...Z_t-1 + u_t -> random walk with u_t ~ White noise(0, sgma2_ut)
# ...e_t is IID ~ (0,1)

sim_rnd_wlk = function(Tau, sgma2_ut) {
  return(cumsum(rnorm(Tau, sd=sgma2_ut))) 
}

sim_kpss_prcs = function(xi, Tau, sigma2_ut, sgma2=1) {
  yt <- c()
  rnd_wlk <- sim_rnd_wlk(Tau, sgma2_ut)
  eps <- rnorm(Tau)

  for(t in 0:Tau) {
    realization <- xi*t + rnd_wlk[t] + eps[t]
    yt <- c(yt, realization)
  }
  return(yt)
}

########################################################
set.seed(2345)

defaultW <- getOption("warn") 
options(warn = -1) 

# PARAMS 1
for (n in 1:nsim) {
  sim_1 <- sim_kpss_prcs(xi[1], 100, sgma2_ut[1])
  plot(sim_1, type = "l")
  print(kpss.test(sim_1)) # For all simulations p-value < 0.05 => non-stationary as xi = .1
}

# PARAMS 2
for (n in 1:nsim) {
  sim_1 <- sim_kpss_prcs(xi[2], 100, sgma2_ut[2])
  plot(sim_1, type = "l")
  
  # print(kpss.test(sim_1)) # For all simulations some p-values < others > 0.05 
  print(kpss.test(sim_1, null = "Trend"))
}

# PARAMS 3
for (n in 1:nsim) {
  sim_1 <- sim_kpss_prcs(xi[3], 100, sgma2_ut[3]) 
  plot(sim_1, type = "l")
  
  print(kpss.test(sim_1)) # For all simulations p-value < 0.05 => non-stationary as xi = .1
}

