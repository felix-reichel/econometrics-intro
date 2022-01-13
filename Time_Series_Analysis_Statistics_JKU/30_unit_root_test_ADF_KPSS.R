# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 30th / Unit root test
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)

# Determine whether the series (both are definitely not) or the "differenced" series are stationary

### Revision of ADF and KPSS Tests ###

# Augmented Dickey-Fuller Test
x <- rnorm(1000)    # no unit-root
adf.test(x)         # p-value = 0.01 < 0.05 => H1 significant => stationary

y <- diffinv(x)     # contains a unit-root
adf.test(y)         # p-value = 0.5259 > 0.05 => Accept H0 => non-stationary


# Kwiatkowski–Phillips–Schmidt–Shin (KPSS) Test
x <- rnorm(1000)                    # no unit-root, is level stationary
kpss.test(x, null = "Level")        # p-value = 0.1 > 0.05 => Level Stationary

y <- diffinv(x)     # contains a unit-root
kpss.test(y)        # p-value = 0.01 < 0.05 => non-stationary

x <- 0.3*(1:1000)+rnorm(1000)   # is trend stationary
kpss.test(x, null = "Trend")    # p-value = 0.1 > 0.05 => Trend Stationary
kpss.test(x, null = "Level")    # p-value = 0.01 < 0.05 => Level non-stationary

############################################


# gtemp
plot(gtemp)       # exhibits a visible trend => therefore not trend stationary 
plot(diff(gtemp)) # looks stationary with increasing variance at the end

# Augmented Dickey-Fuller Test of gtemp
adf.test(gtemp, k = 0)              # p-value = 0.01 < 0.05 => reject H0 => stationary
adf.test(gtemp)                     # Lag order 5: p-value = 0.7057 > 0.05 => H0 => non-stationary

adf.test(diff(gtemp), k = 0)      # p-value = 0.01 < 0.05 => reject H0 => stationary
adf.test(diff(gtemp))             # Lag order 5: 0.01 < 0.05 => reject H0 => stationary


# Kwiatkowski–Phillips–Schmidt–Shin (KPSS) test of gtemp
?kpss.test
kpss.test(gtemp, null = "Trend")    # p-value = 0.01 < 0.05 => Reject H0 => Trend non-stationary
kpss.test(gtemp, null = "Level")    # p-value = 0.01 < 0.05 => Reject H0 => Level not stationary

kpss.test(diff(gtemp), null = "Level")  # p-value = 0.1 > 0.05 => H0 => Has level stationary
kpss.test(diff(gtemp), null = "Trend")  # p-value = 0.1 > 0.05 => H0 => Has trend stationary


# A: => We conclude that gtemp is non-stationary, but diff(gtemp) is already stationary so we can choose an integration order d = 1 
# when using an apropiate ARIMA(p,d,q) model for forecasting



# gnp
plot(gnp)
plot(diff(gnp))

adf.test(gnp, k = 0)            # p-value = 0.99 => Accept H0 => not stat.
adf.test(gnp)                   # Lag order = 6, p-value = 0.99 => Accept H0 => not stat.
kpss.test(gnp, null = "Trend")  # p-value = 0.01 < 0.05 => Reject H0 => Trend non-stationary
kpss.test(gnp, null = "Level")  # p-value = 0.01 < 0.05 => Reject H0 => Level not stationary

adf.test(diff(gnp), k = 0)           # p-value = 0.01 < 0.05 => reject H0 => stationary
adf.test(diff(gnp))                  # p-value = 0.01 < 0.05 => reject H0 => stationary
kpss.test(diff(gnp), null = "Level") # p-value = 0.01 < 0.05  => reject H0 => stationary
kpss.test(diff(gnp), null = "Trend") # p-value = 0.1 > 0.05 => H0 => Has trend stationary

# => A: We conclude that only the differenced time series of gnp is trend stationary.




