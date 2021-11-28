# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 22th / AR(1) Parameter estimation
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)

Tau <- 100000
sgma2 <- 1
phi <- .6
micron <- 10 # where is the unconditional mean

Tau_subset <- c(50, 100, 1000, 10000)

set.seed(2345)

ARIMA_order <- c(1,0,0)
ts.sim.demeaned <- arima.sim(list(order = ARIMA_order, ar = c(phi)), n = Tau, sd = sgma2)
ts.sim <- micron + ts.sim.demeaned

plot(ts.sim[1:100], type = "l")


# AR method to estimate the chosen parameters

# ar(ts.sim[1:Taus[1]], method = "yule-walker")
# ar(ts.sim[1:Taus[1]], method = "ols")
# ar(ts.sim[1:Taus[1]], method = "mle")

for (idx in 1:4) {
  idx
  ywr_est <- ar(ts.sim[1:Tau_subset[idx]], method = "yule-walker")
  ywr_est 
  ols_est <- ar(ts.sim[1:Tau_subset[idx]], method = "ols")
  ols_est
  mle_est <- ar(ts.sim[1:Tau_subset[idx]], method = "mle")
  mle_est
}


for (idx in 1:4) {
  t <- Tau_subset[idx]
  x <- 1:t
  linear_model <- lm(x ~ ts.sim.demeaned[1:t])

}


x <- 1:100
plot(ts.sim.demeaned[1:100], type ="l")
linear_model <- lm(x ~ ts.sim.demeaned[1:100])
summary(linear_model)

df <- data.frame(ltr = 1:100)
lm_fit <- ts(predict(linear_model, newdata = df))

plot(lm_fit)
plot(ts.sim.demeaned[1:100], type ="l")




