# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 4th / Nile
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)

# 1.

time(Nile)

training_set <- window(Nile, start = 1871, end = 1965)
test_set <-window(Nile, start = 1966, end = 1970)

sum_training_set <- sum(training_set)

length_training_set <- length(training_set)
length_test_set <- length(test_set)

average_method_forecasts <- c(1:length_test_set)
naive_method_forecasts <- c(1:length_test_set)

idx <- 0
while (idx <= length_test_set) {
  sum_prev_forecasts = sum(unlist(average_method_forecasts))
  calc_new_forecast = (sum_training_set + sum_prev_forecasts) / (length_training_set + idx)
  average_method_forecasts[idx] = calc_new_forecast
  naive_method_forecasts[idx] = training_set[length_training_set]
  idx = idx + 1
}


plot(Nile)
series1 <- c(training_set, average_method_forecasts)
abline(a = series1, b = 0., col = "red", lwd = 4)
series2 <- c(training_set, naive_method_forecasts)
abline(a = series2, b = 0., col = "blue", lwd = 2)

plot(test_set)
series1 <- c(average_method_forecasts)
abline(a = series1, b = 0., col = "red", lwd = 4)
series2 <- c(naive_method_forecasts)
abline(a = series2, b = 0., col = "blue", lwd = 2)

# 3.
predicted_err <- test_set - average_method_forecasts 
percentage_err <- predicted_err / test_set
squared_err <- predicted_err^2

MSE_averrage_method <- mean(squared_err)
MAE_averrage_method <- mean(abs(predicted_err))
MAPE_averrage_method <- mean(abs(percentage_err))

predicted_err <- test_set - naive_method_forecasts 
percentage_err <- predicted_err / test_set
squared_err <- predicted_err ^ 2

MSE_naive_method <- mean(squared_err)
MAE_naive_method <- mean(abs(predicted_err))
MAPE_naive_method <- mean(abs(percentage_err))





