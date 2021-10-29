# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 5th / Nile Holt-Winters
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)
require(Metrics)

# 1.)
alpha_1 = 0.1
Nile_exp_1 = HoltWinters(Nile, alpha = alpha_1, beta = FALSE, gamma = FALSE)

alpha_2 = 0.3
Nile_exp_2 = HoltWinters(Nile, alpha = alpha_2, beta = FALSE, gamma = FALSE)

alpha_3 = 0.8
Nile_exp_3 = HoltWinters(Nile, alpha = alpha_3, beta = FALSE, gamma = FALSE)

plot(Nile_exp_1, lwd = 2)
plot(Nile_exp_2, lwd = 2)
plot(Nile_exp_3, lwd = 2)

# 2.)
actual_values <- c(Nile[2:100])
actual_values

predicted_values_1 <- c(Nile_exp_1$fitted[1:99])
predicted_values_1

mse1 <- mse(actual_values, predicted_values_1)
mse1
mae1 <- mae(actual_values, predicted_values_1)
mae1
mape1 <- mape(actual_values, predicted_values_1)
mape1

predicted_values_2 <- c(Nile_exp_2$fitted[0:99])
predicted_values_2

mse2 <- mse(actual_values, predicted_values_2)
mse2
mae2 <- mae(actual_values, predicted_values_2)
mae2
mape2 <- mape(actual_values, predicted_values_2)
mape2

predicted_values_3 <- c(Nile_exp_3$fitted[0:99])
predicted_values_3

mse3 <- mse(actual_values, predicted_values_3)
mse3
mae3 <- mae(actual_values, predicted_values_3)
mae3
mape3 <- mape(actual_values, predicted_values_3)
mape3

# in my chosen grid: alpha = 0.3 

# 3.) 
# -> Unknown parameters are determined by minimizing the squared one-step prediction error.
Nile_exp_opt = HoltWinters(Nile, beta = FALSE, gamma = FALSE)
alpha_opt <- Nile_exp_opt$alpha
alpha_opt

plot(Nile_exp_opt)

predicted_values_opt <- c(Nile_exp_opt$fitted[0:99])
predicted_values_opt
mse4 <- mse(actual_values, predicted_values_opt)
mae4 <- mae(actual_values, predicted_values_opt)
mape4 <- mape(actual_values, predicted_values_opt) 
mape4 # ~ 13.07% mean abs. perc. err.

# 4.)
alphas <- c(alpha_1, alpha_opt, alpha_2, alpha_3)

mse_s <- c(mse1, mse4, mse2, mse3)
mae_s <- c(mae1, mae4, mae2, mae4)
mape_s <- c(mape1, mape4, mape2, mape3)

# MSE
df_mse <- data.frame(alphas, mse_s)
# could fit a function using a linear model
plot(df_mse)
lines(df_mse)
abline(v = alpha_opt)

# MAE
df_mae <- data.frame(alphas, mae_s)
plot(df_mae)
lines(df_mae)
abline(v = alpha_opt)

# MAPE
df_mape <- data.frame(alphas, mape_s)
plot(df_mape)
lines(df_mape)
abline(v = alpha_opt)




