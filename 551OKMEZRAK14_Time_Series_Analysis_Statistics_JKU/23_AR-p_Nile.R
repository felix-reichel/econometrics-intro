# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 23th / Nile AR(p) 1
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)
require(stats)

plot(Nile)
acf(Nile)
pacf(Nile) # cuts off at lag 1 -> AR(1) model

AR_1 <- ar(Nile, order.max = 1)
AR_1

AR_1$ar
AR_1$asy.var.coef

SE_AR_1 = sqrt(AR_1$asy.var.coef)
AR_1$ar + c(-2, 2) * c(SE_AR_1)

pred <- predict(AR_1, n.ahead = 5)

ts_incl_pred <-ts (data = c(Nile, pred$pred), start = start(Nile), frequency = frequency(Nile))
ts_incl_pred_lower <-ts (data = c(Nile, pred$pred - pred$se/2), start = start(Nile), frequency = frequency(Nile))
ts_incl_pred_upper <-ts (data = c(Nile, pred$pred + pred$se/2), start = start(Nile), frequency = frequency(Nile))

plot(ts_incl_pred)
lines(ts_incl_pred_lower, col="blue")
lines(ts_incl_pred_upper, col="red")
lines(ts_incl_pred)
abline(v = end(Nile)[1])

x <- 1:100 # end - start + 1
lm = lm(x ~ Nile)
lm$coefficients
plot(lm$residuals)
abline(h=mean(lm$residuals), col="red")

lm_intercept_only = lm(Nile ~ 1)
new_data <- data.frame(x=1, y=1)

lm_predict_predition <- predict(lm_intercept_only, newdata = new_data, interval = "prediction")
lm_predict_predition

hw_exp_ses_train <- HoltWinters(ts(Nile, frequency = 1), beta = FALSE, gamma = FALSE)
hw_exp_ses_pred <- predict(object = hw_exp_ses_train, n.ahead = 5, prediction.interval = TRUE)

hw_exp_ts <- ts(c(hw_exp_ses_train$fitted[,1], hw_exp_ses_pred[,1]),
  start = start(Nile), 
  end = end(Nile)[1] + 5, 
  frequency = 1)

plot(ts_incl_pred)
lines(ts_incl_pred_lower, col="blue")
lines(ts_incl_pred_upper, col="red")
lines(ts_incl_pred)
lines(hw_exp_ts, col="purple")
abline(v = end(Nile)[1])
for (i in 1:3) {
  abline(h = lm_predict_predition[i], col="green")
}