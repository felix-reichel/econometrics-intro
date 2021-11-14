# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 15th / Linear Regression vs. Exponential Smoothing
#                  with deterministic seasonal pattern
# Author: Felix Reichel
# ---------------------------------------------------------

# Simulate data from a model with linear trend and a deterministic (quarterly)
# seasonal pattern (T = 100, σ^2 = 1) and plot the series.

TT <- 100
sigma <- sqrt(1)
periods <- 4

ltr <-1:TT
seas_pattern <- c(4, 2.5, 3, 1.5)
seas <- rep(seas_pattern, TT/periods)

a = 0.15

set.seed(2345)

err <- rnorm(TT)*sigma
ytrseas <- a*ltr + seas + err

plot(ytrseas, type="l")
lines(a * ltr + seas, col="blue",xlab="t")

# Fit a linear regression model to the series and perform a residual analysis

s <- C(as.factor(rep(1:periods, TT/periods)), contr.sum)
lm = lm(ytrseas ~ ltr + s)
summary(lm)

# Residual analysis
lm$residuals

# Plot residuals
plot(lm$residuals, type="l")
abline(h = mean(lm$residuals), col="red")

require(lmtest)
# Durbin Watson
dwtest(lm) # one sided test: rho(1)>0

acf(lm$residuals) # ci: (- 1.96/sqrt(TT),+ 1.96/sqrt(TT))
Box.test(lm$residuals, lag = 3, type = "Ljung")

# heteroscedasticity
plot(lm$residuals^2,type="l",xlab="t")
bptest(lm)

# normal distribution
hist(lm$residuals)

qqnorm(lm$residuals)
qqline(lm$residuals,col="red")

jarque.bera.test(lm$residuals)

# Use the appropriate exponential smoothing method for the data and compare
# the insample performance to that of the linear regression.

hw_exp_train <- HoltWinters(ts(ytrseas, frequency = periods), seasonal = "additive")

df <- data.frame(ltr = 1:TT, C(as.factor(rep(1:periods, TT/periods)), contr.sum))
lm_fit <- ts(predict(lm, newdata = df), frequency = periods)

plot(hw_exp_train)
lines(lm_fit, col="blue")

# MSE, MAE and MAPE
require(Metrics)

hw_exp_fit_actual <- c(hw_exp_train$fitted[,1])
lm_fit_actual <- c(lm_fit)[5:100]
expected <- ytrseas[5:100]

mse(expected, hw_exp_fit_actual)
mse(expected, lm_fit_actual)

mae(expected, hw_exp_fit_actual)
mae(expected, lm_fit_actual)

mape(expected, hw_exp_fit_actual)
mape(expected, lm_fit_actual)

# Forecast the next 20 values with both methods.
fc <- 20
Tf <- TT+fc

lm_df = data.frame(ltr=1:(TT+fc), s = C(as.factor(rep(1:periods, Tf/periods)), contr.sum))
lm_pred = predict(lm,newdata = lm_df)

hw_exp_fc = predict(hw_exp_train, n.ahead = fc)

plot(c(hw_exp_fc), type="l")
lines(lm_pred[101:120], col="blue")

# Now simulate a realisation of these 20 values and compare out-of sample performance. 
# Plot the original and the new simulated data as well as the forecasts
# from both methods. Which method performs better?


