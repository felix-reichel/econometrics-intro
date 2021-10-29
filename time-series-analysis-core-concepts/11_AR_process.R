# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 11th / Auto-regressive process
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)

# 1.
# Yt = ϕYt−1 + εt     t = 1,...,T  where εt is white noise and Y0 = 0 and |ϕ| < 1.

# theoretical ACF limT→∞
# limT→∞ ρ(τ) = γ(τ)/γ(0) = Cov(yt,yt+,h)/Var(yt)
# limT→∞ ρ(τ) = limT→∞ ϕ^τ = 0

# yt = ϕYt−1 + εt    / * yt-h
# yt-h * yt  = ϕ*yt-h*Yt−1 + yt-h*εt
# E(yt-h*yt) = E(ϕ*yt-h*Yt−1) + E(yt-h*εt)
# E(yt-h*yt) = E(ϕ*yt-h*Yt−1)

ARMAacf(ar = c(0.5), lag.max = 10)
ARMAacf(ar = c(0.5), lag.max = 100)
# ACF(Yt) limT→∞ = 0

# 2.
T <- 100
phi_c <- c(-0.9,-0.5, 0.1, 0.1, 0.5, 0.9);

AR_models = matrix()
first_iteration <- TRUE

for (phi in phi_c) {
  if (first_iteration) {
    first_iteration <- FALSE
    AR_models <- arima.sim(model = list(ar = phi), n = T)
  } 
  else {
    AR_models <- cbind(AR_models, arima.sim(model = list(ar = phi), n = T))
  }
}

plot(AR_models[,])
plot(acf(AR_models[,1]))
plot(acf(AR_models[,2]))
plot(acf(AR_models[,3]))
plot(acf(AR_models[,4]))
plot(acf(AR_models[,5]))
plot(acf(AR_models[,6]))

