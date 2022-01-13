# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 30th / Unit root test
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)

# Determine whether the series (both are definitely not) or the "differenced" series are stationary

# gtemp
plot(gtemp)
plot(diff(gtemp))

# adf
adf.test(gtemp, k = 0)              # p-value = 0.01 < 0.05 => reject H0 => stationary
adf.test(gtemp)                     # Lag order 5: p-value = 0.7057 > 0.05 => H0 => not stationary

# kpss
kpss.test(gtemp, null = "Trend")    # p-value = 0.01 < 0.05 => Reject H0 => not stationary
kpss.test(gtemp, null = "Level")    # p-value = 0.01 < 0.05 => Reject H0 => not stationary

# adf
adf.test(diff(gtemp), k = 0)      # p-value = 0.01 < 0.05 => reject H0 => stationary
adf.test(diff(gtemp))             # Lag order 5: 0.01 < 0.05 => reject H0 => stationary
# kpss
kpss.test(diff(gtemp), null = "Level")  # p-value = 0.1 > 0.05 => H0 => level stationary
kpss.test(diff(gtemp), null = "Trend")  # p-value = 0.1 > 0.05 => H0 => trend stationary


# gnp
plot(gnp)
plot(diff(gnp))

adf.test(gnp, k = 0)            # p-value = 0.99 => not stat.
adf.test(gnp)                   # Lag order = 6, p-value = 0.99 => not stat.
kpss.test(gnp, null = "Trend")  # p-value = 0.01 < 0.05 => reject H0 => not stat.
kpss.test(gnp, null = "Level")  # p-value = 0.01 < 0.05 => reject H0 => not stat.

adf.test(diff(gnp), k = 0)           # p-value = 0.01 < 0.05
adf.test(diff(gnp))                  # p-value = 0.01 < 0.05
kpss.test(diff(gnp), null = "Level") # p-value = 0.01 < 0.05  => reject H0 => not stat.
kpss.test(diff(gnp), null = "Trend") # p-value = 0.1 > 0.05 => H0 => Trend stat.




