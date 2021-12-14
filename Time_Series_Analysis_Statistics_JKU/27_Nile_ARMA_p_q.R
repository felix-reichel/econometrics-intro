# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 27th / Nile ARMA(p,q)
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)
source("https://raw.githubusercontent.com/robjhyndman/forecast/master/R/armaroots.R")

plot(Nile)

acf(Nile) # decays
pacf(Nile) # cuts of at Lag 1

# as pure AR(p) 
y.aic <- c(rep(0,10))
y.bic <- c(rep(0,10))
for (p in 0:10) {
  ar_p_fit <- arima(Nile, c(p,0,0), include.mean = FALSE)
  y.aic[p] = ar_p_fit$aic
  y.bic[p] = BIC(ar_p_fit)
}
y.aic
which.min(y.aic) # p = 8 according to AIC 
which.min(y.bic) # p = 3 according to BIC

# AR(3)
fit1 <- arima(Nile, c(3,0,0), include.mean = FALSE)
AIC(fit1)
BIC(fit1)


# MA(q)
y.aic <- c(rep(0,10))
y.bic <- c(rep(0,10))
for (q in 0:10) {
  ma_q_fit <- arima(Nile, c(0,0,q), include.mean = FALSE)
  y.aic[q] = ma_q_fit$aic
  y.bic[q] = BIC(ma_q_fit)
}
y.aic
y.bic
which.min(y.aic) # q = 9
which.min(y.bic) # q = 9

fit2 <- arima(Nile, c(0,0,9), include.mean = FALSE)
AIC(fit2)
BIC(fit2)


# ARMA(qmax, qmax) model selection
y <- Nile
nt <- length(y)
pmax <- 2
qmax <- 2

y.aic <- matrix(0, nrow = pmax + 1, ncol = qmax + 1)
y.bic <- matrix(0, nrow = pmax + 1, ncol = qmax + 1)

for (p in 0:pmax) { 
  for (q in 0:qmax) {
    m <- arima(y, order = c(p,0,q), include.mean = FALSE)
    k <- p+q+1
    y.aic[p+1,q+1] <- m$aic
    y.bic[p+1,q+1] <- -2*m$loglik+log(nt)*k
  }
}
which.min(y.aic) # p = 1, q = 2 according to AIC
which.min(y.bic) # q = 1, q = 1 according to BIC

# ARMA(1,1) (BIC)
fit3 <- arima(Nile, c(1,0,1), include.mean = FALSE) # aic = 1287.64
BIC(fit3)
AIC(fit3)

# ARMA(2,1) (AIC)
fit3_ARMA_2_0_1 <- arima(Nile, c(2,0,1), include.mean = FALSE) # aic = 1287.64
BIC(fit3)
AIC(fit3)

# Residual analysis AR(3)
plot(fit1$residuals)
acf(fit1$residuals)
Box.test(fit1$residuals, lag = 3, type = "Ljung") # p-value = 0.2749 => H0: model fits
plot(fit1$residuals^2) # no heteroskedasticity
hist(fit1$residuals)
jarque.bera.test(fit1$residuals) # p-value = 0.9247 => H0: Normality

plot(fit2$residuals)
acf(fit2$residuals)
Box.test(fit2$residuals, lag = 3, type = "Ljung") 
plot(fit2$residuals^2) # no heteroskedasticity
hist(fit2$residuals)
jarque.bera.test(fit2$residuals)

plot(fit3$residuals)
acf(fit3$residuals)
Box.test(fit3$residuals, lag = 3, type = "Ljung") # p-value = 0.6569 => H0
plot(fit3$residuals^2) # no heteroskedasticity
hist(fit3$residuals)
jarque.bera.test(fit3$residuals) # p-value = 0.9777 => H0: Normality

# -> ARMA(1,1)

par(mfrow=c(1,1))
plot(Nile, type="l")
prediction <- predict(fit3, n.ahead=10, prediction.interval=TRUE)
lines(prediction$pred, col="red")
lines(prediction$pred -1.96*prediction$se, lty=2, col="blue")
lines(prediction$pred + 1.96*prediction$se, lty=2, col="blue")
