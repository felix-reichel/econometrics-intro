# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 24th / Nile AR(p) 2
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)

train_set <- c(Nile[1:80])
test_set <- Nile[81:100]

acf(train_set)
pacf(train_set) # cuts off at lag 1 -> AR(1) model

AR_1 <- ar(train_set, order.max = 1)
AR_auto_order <- ar(train_set) # Order selected 2 sigma^2 estimated as 22402

AR_1.pred <- predict(AR_1, n.ahead = 20)$pred
AR_auto_order.pred <- predict(AR_auto_order, n.ahead = 20)$pred

lm_intercept = lm(train_set ~ 1)
lm_intercept.pred <- predict(lm_intercept)
lm_intercept.pred <- rep(lm_intercept.pred[1], 20)

ses_train <- HoltWinters(train_set, beta = FALSE, gamma = FALSE)
ses_train.pred <- predict(object = ses_train, n.ahead = 20, prediction.interval = TRUE)
ses_train.pred <- rep(ses_train.pred[1], 20)

require(Metrics)

mse(test_set, c(AR_1.pred))
mse(test_set, c(AR_auto_order.pred))
mse(test_set, c(lm_intercept.pred))
mse(test_set, c(ses_train.pred))

plot(c(Nile), type="l")
lines(c(train_set, AR_1.pred), col="red", type="l")
lines(c(train_set, AR_auto_order.pred), col="purple", type="l")
lines(c(train_set, lm_intercept.pred), col="green", type="l")
lines(c(train_set, ses_train.pred), col="blue", type="l")
lines(c(train_set), col="black", type="l")
abline(v=80)

