# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 40th / NYSE - ARCH/GARCH Volatility models
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)

install.packages("fGarch")
require(fGarch)

plot(nyse)
