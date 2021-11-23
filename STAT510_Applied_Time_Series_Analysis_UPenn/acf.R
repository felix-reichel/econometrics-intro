require(astsa)

current_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))

x <- ts(scan("quakes.dat"))
plot(x, type="b")

lag1.plot(x, 1) # Plots x versus x lag 1

acf(x, xlim=c(1,19)) # Plots the ACF of x for lags 1 to 19

xlag1 <- lag(x, -1) # Creates a lag 1 of x variable.
y <- cbind(x, xlag1)
ar_model <- lm(y[,1] ~ y[,2])
summary(ar_model)

plot(ar_model$fit,ar_model$residuals) # plot of residuals versus fits
acf(ar_model$residuals, xlim=c(1,18)) # ACF of the residuals for lags 1 to 18 
