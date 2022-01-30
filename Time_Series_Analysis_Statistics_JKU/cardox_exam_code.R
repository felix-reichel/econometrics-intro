setwd("./GitHub/learning-econometrics/Time_Series_Analysis_Statistics_JKU")
gebdate <- 20000117

require(astsa)
require(tseries)
require(forecast)

# 1.

set.seed(gebdate)

randomInt <- floor(runif(1, min = 1, max = 20))
J <- 1980 - randomInt

freq <- frequency(cardox)
J_end <- 2016+1-(1/freq) 

full_cardox_ts <- ts(data = cardox, start=1958+(2/12), end = 2019-(2/12), frequency = freq) 
length(full_cardox_ts)

cardox_ts <- window(x = full_cardox_ts, start = J, end = J_end) 

# 2.

plot(cardox_ts)
abline(h = mean(cardox_ts), col="red") # mean

plot(window(cardox_ts, start=2014, end=J_end))

decomposed <- decompose(cardox_ts, type = "additive")
plot(decomposed)

qqnorm(decomposed$random)
qqline(decomposed$random)
hist(decomposed$random)
jarque.bera.test(na.omit(decomposed$random)) 

decomposed2 <- decompose(window(cardox_ts, end=2015), type = "additive")
jarque.bera.test(na.omit(decomposed2$random)) # X-squared  = 6.9479 > 6 => H1 => Nicht Normalverteilt

decomposed3 <- decompose(window(cardox_ts, end=2000), type = "additive")
jarque.bera.test(na.omit(decomposed3$random)) # X-squared  = X-squared = 2.5633 < 6 => H0 => Normalverteilt

decomposed_mult <- decompose(cardox_ts, type = "multiplicative")
plot(decomposed_mult)
qqnorm(decomposed_mult$random)
qqline(decomposed_mult$random)
hist(decomposed_mult$random)
jarque.bera.test(na.omit(decomposed_mult$random))


# 3.

cardox_ts_exp_m <- HoltWinters(cardox_ts, seasonal = "multiplicative")
cardox_ts_exp_a <- HoltWinters(cardox_ts, seasonal = "additive")

plot(cardox_ts_exp_m)
plot(cardox_ts_exp_a)

cardox_len <- length(cardox_ts)

test_set_yrs <- 4
test_set_len <- test_set_yrs * freq    
train_set_len <- cardox_len - test_set_len
cut <- J_end - test_set_yrs

training_set <- window(cardox_ts, start = J, end = cut) 
test_set <- window(cardox_ts, start = cut, end = J_end) 

cardox_ts_train_exp_m <- HoltWinters(training_set, seasonal = "multiplicative")
cardox_ts_train_exp_a <- HoltWinters(training_set, seasonal = "additive")

cardox_ts_train_exp_m_pred = predict(cardox_ts_train_exp_m, n.ahead = test_set_len, prediction.interval = T)
cardox_ts_train_exp_a_pred = predict(cardox_ts_train_exp_a, n.ahead = test_set_len, prediction.interval = T)

plot(cardox_ts_train_exp_m, cardox_ts_train_exp_m_pred, lwd = 2)
plot(cardox_ts_train_exp_a, cardox_ts_train_exp_a_pred, lwd = 2)

actual_values <- tail(cardox_ts, test_set_len)

hw_m_predicted <- cardox_ts_train_exp_m_pred[,1]
hw_a_predicted <- cardox_ts_train_exp_a_pred[,1]

rbind(accuracy(actual_values, hw_m_predicted), 
      accuracy(actual_values, hw_a_predicted))


# 4.

d1 <- diff(cardox_ts, lag = freq)
plot(d1) 

adf.test(d1)
kpss.test(d1, null= "Level")
kpss.test(d1, null = "Trend")

d2 <- diff(d1)
plot(d2)

adf.test(d2) 
kpss.test(d2, null= "Level")
kpss.test(d2, null= "Trend") 

acf(d2)
acf(d2, lag.max = 60)

pacf(d2)
pacf(d2, lag.max = 60)

cardox_sarima_m1 <- sarima(xdata = cardox_ts, p = 1, d = 1, q = 1, P = 0, D = 1, Q = 1, S = freq)

residuals <- cardox_sarima_m1$fit$residuals
qqnorm(residuals)
qqline(residuals)
hist(residuals)
jarque.bera.test(residuals)

AIC(cardox_sarima_m1$fit)
BIC(cardox_sarima_m1$fit)

# install.packages('bayesforecast')
# require(bayesforecast)
# auto.sarima(cardox_ts, seasonal = TRUE)
cardox_auto_sarima_m <- sarima(xdata = cardox_ts, p = 1, d = 1, q = 1, P = 2, D = 1, Q = 1, S = freq)
AIC(cardox_auto_sarima_m$fit)
BIC(cardox_auto_sarima_m$fit)


# 5.
cardox_ts_next_2_yrs <- window(x = full_cardox_ts, start = 2017)

holtWinters_m_pred <- predict(cardox_ts_exp_m, n.ahead = 24, prediction.interval = T)
sarima_pred <- predict(cardox_sarima_m1$fit, n.ahead = 24)
auto_sarima_pred <- predict(cardox_auto_sarima_m$fit, n.ahead = 24)

ts.plot(cardox_ts_next_2_yrs)
lines(holtWinters_m_pred[,1], col="red")
lines(sarima_pred$pred, col="blue")
lines(auto_sarima_pred$pred, col="green")

rbind(
  accuracy(cardox_ts_next_2_yrs, holtWinters_m_pred[,1]),
  accuracy(cardox_ts_next_2_yrs, sarima_pred$pred),
  accuracy(cardox_ts_next_2_yrs, auto_sarima_pred$pred))

# 6.
