# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 36th / US monthly live births
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)

plot(birth)
acf(birth, lag.max = 60)
pacf(birth)

deseas <- diff(birth, 12)
plot(deseas)

d1 <- diff(deseas)
plot(d1)
# Unit root test for determining integration order d 
adf.test(d1) # p-value = 0.01 < 0.05 => H1 => Stationary 
kpss.test(d1) # p-value = 0.1 > 0.05 => H0 => Level Stationarity
# => d = D = 1

acf(birth, lag.max = 60)
pacf(birth)

acf(d1, lag.max = 60) 
# spikes after every 12th lag => Q = 1, 
# ACF of a MA(q) process cuts off at lag q = 1
pacf(d1, lag.max = 60) 
# spikes after lag = 12 => P = 1, Order p could be 1,2,3,4 or 5
# PACF of an AR(p) process ’cuts off’ at lag p

# => SARIMA(1,1,1)(1,1,1)_12
birth_m <- sarima(xdata = birth, p = 1, d = 1, q = 1, P = 1, D = 1, Q = 1, S = 12)
summary(birth_m)
AIC(birth_m$fit)

birth_m2 <-sarima(xdata = birth, p = 2, d = 1, q = 1, P = 1, D = 1, Q = 1, S = 12)
summary(birth_m2)
AIC(birth_m2$fit)

birth_m3 <-sarima(xdata = birth, p = 3, d = 1, q = 1, P = 1, D = 1, Q = 1, S = 12)
summary(birth_m3)
AIC(birth_m3$fit)

# sticking with SARIMA(1,1,1)(1,1,1)_12 according to AIC

# plot series with prediction
pred.tr <- predict(birth_m$fit, n.ahead = 24)
ts.plot(birth)
lines(pred.tr$pred, col="red")


