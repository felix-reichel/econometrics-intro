# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 8th / Holt-Winters with seasonal component
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)
require(Metrics)

len_AirPassengers <- length(AirPassengers)
plot(AirPassengers)

# (1/12)*(11-1) = 0.8333333333
AirPassengers_train <- ts(data = AirPassengers, start = 1949.8333333333,end = 1960.8333333333, frequency = 12)
AirPassengers_test <- AirPassengers[len_AirPassengers-1:len_AirPassengers]

AirPassengers_exp_m = HoltWinters(AirPassengers_train, seasonal = "multiplicative")
AirPassengers_exp_a = HoltWinters(AirPassengers_train, seasonal = "additive")

plot(AirPassengers_exp_m)
plot(AirPassengers_exp_a)

AirPassengers_pred_1 = predict(AirPassengers_exp_m, n.ahead = 1, prediction.interval = T)
AirPassengers_pred_2 = predict(AirPassengers_exp_a, n.ahead = 1, prediction.interval = T)

plot(AirPassengers_exp_m, AirPassengers_pred_1, lwd = 2)
plot(AirPassengers_exp_a, AirPassengers_pred_2, lwd = 2)

AirPassengers_pred_1
fit1 <- AirPassengers_pred_1[1]
print(fit1)

AirPassengers_pred_2
fit2 <- AirPassengers_pred_2[1]
print(fit2)

# errs.

mse_err1 = mse(AirPassengers_test,fit1)
print(mse_err1)
mse_err2 = mse(AirPassengers_test,fit2)
print(mse_err2)

mae_err1 = mae(AirPassengers_test,fit1)
print(mae_err1)
mae_err2 = mae(AirPassengers_test,fit2)
print(mae_err2)

mape_err1 = mape(AirPassengers_test,fit1)
print(mape_err1)
mape_err2 = mape(AirPassengers_test,fit2)
print(mape_err2)

# 2.)

AirPassengers_exp_m = HoltWinters(AirPassengers, seasonal = "multiplicative")
AirPassengers_exp_a = HoltWinters(AirPassengers, seasonal = "additive")

AirPassengers_predict_1 = predict(AirPassengers_exp_m, n.ahead = 12, prediction.interval = T)
AirPassengers_predict_2 = predict(AirPassengers_exp_a, n.ahead = 12, prediction.interval = T)

plot(AirPassengers_exp_m, AirPassengers_predict_1, lwd = 2)
plot(AirPassengers_exp_a, AirPassengers_predict_2, lwd = 2)


