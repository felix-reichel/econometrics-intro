# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 13th / Fitting linear and quadratic trend models,
#           estimating and residual analysis
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)

# set path
# current_path = rstudioapi::getActiveDocumentContext()$path 
# setwd(dirname(current_path))

# uspop_1971_2020 <- c(data = 
#  read.csv(file = "13_data/uspop_worldbank_1971_2020_Data.csv")[1,])[5:54]

# uspop2_world_bank_data <- c(
#  round(as.numeric(uspop_1971_2020[10])*10^-6, 1),
#  round(as.numeric(uspop_1971_2020[20])*10^-6, 1),
#  round(as.numeric(uspop_1971_2020[30])*10^-6, 1),
#  round(as.numeric(uspop_1971_2020[40])*10^-6, 1)
# )
uspop2 <- c(226.5, 248.7, 281.4, 308.7)

# add observations to existing time series uspop
uspop_mod <- ts(data = c(uspop, uspop2), start = start(uspop), frequency = frequency(uspop))

# linear trend model
fc_steps <- 2
Tf <- length(uspop_mod) + fc_steps
t <- (time(uspop_mod) - start(uspop_mod)) * frequency(uspop_mod) + 1

linear_model <- lm(uspop_mod ~ t)
newdata_lm = data.frame(t=1:Tf)

lm_fc = predict(linear_model, newdata = newdata_lm, interval="predict")
lm_fc.ts = ts(lm_fc, start = start(uspop_mod), end = end(uspop_mod), frequency = frequency(uspop_mod))

# plot uspop and linear trend model forecast 
plot(uspop, xlim=c(start(uspop),end(uspop_mod)),ylim=c(min(lm_fc.ts),max(lm_fc.ts)))
lines(lm_fc.ts[,1],col="red")

# quadratic trend model
tq <- t^2
quadratic_model <- lm(uspop_mod ~ t+tq)
newdata_lm_q <- data.frame(t=1:Tf, tq=(1:Tf)^2)

lm_q_fc <- predict(quadratic_model, newdata= newdata_lm_q, interval="predict")
lm_q_fc.ts <- ts(lm_q_fc, start=start(uspop), end=end(uspop_mod), frequency=frequency(uspop))

plot(uspop, 
     xlim=c(start(uspop),end(uspop_mod)), 
     ylim=c(min(lm_q_fc.ts,lm_fc.ts),max(lm_q_fc.ts,lm_fc.ts)))
lines(lm_fc.ts[,1],col="red") # fit
lines(lm_fc.ts[,2],col="red") # lwr
lines(lm_fc.ts[,3],col="red") # upr
lines(lm_q_fc.ts[,1],col="green") # fit
lines(lm_q_fc.ts[,2],col="green") # lwr
lines(lm_q_fc.ts[,3],col="green") # upr


# residual analysis
linear_model$residuals
plot(matrix(time(uspop_mod)),linear_model$residuals, ylab="residuals",xlab="year",
     type="b",col="red")
abline(h=mean(linear_model$residuals))

quadratic_model$residuals
plot(matrix(time(uspop_mod)),quadratic_model$residuals, ylab="residuals",xlab="year",
     type="b",col="green")
abline(h=mean(quadratic_model$residuals))

#autocorrelations
require(lmtest)
dwtest(linear_model) #one sided test: rho(1)>0
dwtest(quadratic_model) #one sided test: rho(1)>0

acf(linear_model$residuals)
Box.test(linear_model$residuals, lag = 3, type = "Ljung")

acf(quadratic_model$residuals)
Box.test(quadratic_model$residuals, lag = 3, type = "Ljung")

# heteroscedasticity
plot(linear_model$residuals^2,type="l",xlab="t")
bptest(linear_model)

plot(quadratic_model$residuals^2,type="l",xlab="t")
bptest(quadratic_model)

# normal distribution
hist(linear_model$residuals)
hist(quadratic_model$residuals)

qqnorm(linear_model$residuals)
qqline(linear_model$residuals,col="red")

qqnorm(quadratic_model$residuals)
qqline(quadratic_model$residuals,col="red")

jarque.bera.test(linear_model$residuals)
jarque.bera.test(quadratic_model$residuals)


