# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 6th / Comparison of SES and naive methods
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)
require(Metrics)

# 1.)

# 2.)
# AVG fc
nile_v <- c(Nile)
nile_avg <- sum(nile_v)/length(nile_v)

# NAIVE fc
nile_naive <- nile_v[length(nile_v)-1]

# SES fc
Nile_exp = HoltWinters(Nile[1:99], beta = FALSE, gamma = FALSE)
nile_pred <- predict(object = Nile_exp, n.ahead = 1, prediction.interval = FALSE)
nile_ses <- c(nile_pred)

plot(Nile_exp, nile_pred, lwd = 2)
nile_actual <- Nile[100]

# MSE
print(mse(nile_actual, nile_naive))
print(mse(nile_actual, nile_ses))
print(mse(nile_actual, nile_avg))

# MAE
print(mae(nile_actual, nile_naive))
print(mae(nile_actual, nile_ses))
print(mae(nile_actual, nile_avg))

# MAPE
print(mape(nile_actual, nile_naive)) # 3,5%
print(mape(nile_actual, nile_ses)) #12,4%
print(mape(nile_actual, nile_avg)) # 24%

# 3.)
training_sample <- window(x = Nile, start = 1871, end = 1965)
test_sample <- window(x = Nile, start = 1966, end = 1970)

forecase_model <- HoltWinters(training_sample, beta = FALSE, gamma = FALSE)
forecase_model$alpha

forecast <- predict(object = forecase_model, n.ahead = 5, prediction.interval = TRUE)

plot(Nile)
par(new=TRUE)
plot(forecase_model, forecast)

# err
forecast_ses <- c(forecast)[1:5]
mse(test_sample, forecast_ses)
mae(test_sample, forecast_ses)
mape(test_sample, forecast_ses)







