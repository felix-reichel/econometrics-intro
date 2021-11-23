# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 7th / Comparison of SES & Holt's linear trend method
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)
require(Metrics)

# 1.)
gtemp_train <- window(gtemp, start = 1880, end = 2008)
gtemp_test <- window(gtemp, start = 2009, end = 2009)

# SES
gtemp_ses = HoltWinters(gtemp_train, beta = F, gamma = F)
gtemp_ses$alpha

# HES
gtemp_hes = HoltWinters(gtemp_train, gamma = F)
gtemp_hes$alpha
gtemp_hes$beta

gtemp_ses_pred = predict(object = gtemp_ses, n.ahead = 1, prediction.interval = F)
gtemp_hes_pred = predict(object = gtemp_hes, n.ahead = 1, prediction.interval = F)

plot(gtemp_ses, gtemp_ses_pred, lwd = 2)
plot(gtemp_hes, gtemp_hes_pred, lwd = 2)

mse_SES = mse(gtemp_test, gtemp_ses_pred)
mse_HES = mse(gtemp_test, gtemp_hes_pred)

mae_SES = mae(gtemp_test, gtemp_ses_pred)
mae_HES = mae(gtemp_test, gtemp_hes_pred)

mape_SES = mape(gtemp_test, gtemp_ses_pred)
mape_HES = mape(gtemp_test, gtemp_hes_pred)

gtemp_ses_pred2 = predict(object = gtemp_ses, n.ahead = 5, prediction.interval = F)
gtemp_hes_pred2 = predict(object = gtemp_hes, n.ahead = 5, prediction.interval = F)

plot(gtemp_ses, gtemp_ses_pred2, lwd = 2)
plot(gtemp_hes, gtemp_hes_pred2, lwd = 2)

# 2.)
uspop_train <- window(uspop, start = 1790, end = 1969)
uspop_test <- window(uspop, start = 1970, end = 1970)

uspop_ses = HoltWinters(uspop_train, beta = F, gamma = F)
uspop_ses$alpha

uspop_hes = HoltWinters(uspop_train, gamma = F)
uspop_hes$alpha
uspop_hes$beta

uspop_ses_pred = predict(object = uspop_ses, n.ahead = 1, prediction.interval = F)
uspop_hes_pred = predict(object = uspop_hes, n.ahead = 1, prediction.interval = F)

plot(uspop_ses, gtemp_ses_pred, lwd = 2)
plot(uspop_hes, gtemp_hes_pred, lwd = 2)

mse_SES = mse(uspop_test, uspop_ses_pred)
mse_HES = mse(uspop_test, uspop_hes_pred)

mae_SES = mae(uspop_test, uspop_ses_pred)
mae_HES = mae(uspop_test, uspop_hes_pred)

mape_SES = mape(uspop_test, uspop_ses_pred)
mape_HES = mape(uspop_test, uspop_hes_pred)

uspop_ses_pred2 = predict(object = uspop_ses, n.ahead = 5, prediction.interval = F)
uspop_hes_pred2 = predict(object = uspop_hes, n.ahead = 5, prediction.interval = F)

plot(uspop_ses, uspop_ses_pred2, lwd = 2)
plot(uspop_hes, uspop_hes_pred2, lwd = 2)



