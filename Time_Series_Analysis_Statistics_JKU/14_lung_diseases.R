# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 14th / Lung diseases
#           monthly deaths from bronchitis, emphysema and asthma in the UK, 1974–1979
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)

plot(mdeaths)
plot(fdeaths)

freq <- frequency(mdeaths)  # 12
start <- start(mdeaths)     # num[1:2]1974 1
end <- end(mdeaths)         # num[1:2]1979 12
delatat <- deltat(mdeaths)  # 1/freq
Tau <- length(mdeaths)      # Tau
years <- Tau/freq           # 6
t <- rep(1:72)        

# Normalized seasonal vector
plot(window(mdeaths, start=1974, end=1975))
seasonal_decomp <- matrix(decompose(mdeaths)$seasonal)[1:12,]
normalize <- function(x){(x - min(x)) / (max(x) - min(x))}
seasonal_norm <- normalize(seasonal_decomp)
seasonal_norm

# Fit an appropriate regression model 
ltr <- 1:Tau
seas <- C(as.factor(rep(1:freq, years)), contr.sum)

lm = lm(mdeaths ~ ltr + seas)
summary(lm)

# Residual analysis
lm$residuals

# Plot residuals
plot(lm$residuals, type="l")
abline(h = mean(lm$residuals), col="red")

require(lmtest)
# Durbin Watson
dwtest(lm) # one sided test: rho(1)>0
# 0 < DW =1.6512 < 2  => reject H0 => positive autocorrelation of errors

acf(lm$residuals) # ci: (- 1.96/sqrt(TT),+ 1.96/sqrt(TT))
Box.test (lm$residuals, lag = 3, type = "Ljung")
# p-value = 0.08588 > 0.05 => not significant => H0 (model does not show lack of fit)

# heteroscedasticity
plot(lm$residuals^2,type="l",xlab="t")
bptest(lm)
# p-value = 0.003693 < 0.05 => H1 (heteroskedasticity)

# normal distribution
hist(lm$residuals)

qqnorm(lm$residuals)
qqline(lm$residuals,col="red")

jarque.bera.test(lm$residuals)
# p-value = 1.879e-11 < 0.05 => H1 (no normal distribution)

# Seasonal effect for December
seasonal_effect_december <- seasonal_norm[1]
seasonal_effect_december

# Plot the series with the fitted values
df <- data.frame(ltr = 1:Tau, s = rep(1:12, years))
pred <- predict(lm, newdata = df)

plot(mdeaths, col="black")
lines(ts(pred, start=1974, end=1980, deltat = 1/12),type='l',col="blue",xlab="t",ylab="ytrseas")
