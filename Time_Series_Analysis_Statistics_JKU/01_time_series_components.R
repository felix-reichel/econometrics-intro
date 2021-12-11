# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 1st / Components of a time series
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)

gtemp
time(gtemp)     # 1880 - 2009
plot(gtemp)
plot(window(gtemp, 1979, 2009))   # last 30 yrs.

plot(speech)
plot(window(speech, 750, 1000))     # last 250ms.
abline(a = mean(speech), b = 0, col="red", lwd = 2)   # add mean line

plot(nyse)
plot(window(nyse, 1500, 2000))
abline(a = mean(nyse), b = 0, col="red", lwd = 2)

?gnp
plot(gnp)
abline(a = mean(gnp), b = 0, col="red", lwd = 2)
plot(decompose(ts(gnp, frequency = 4)))
plot(decompose(ts(gnp, start= 1992, end = 2002, frequency = 4)))

?EQ5
plot(EQ5)
abline(a = mean(EQ5), b = 0, col="red", lwd = 2)
plot(log(EQ5))

?birth
plot(birth)
abline(a = mean(birth), b = 0, col="red", lwd = 2)
plot(decompose(ts(birth, start= 1959, end = 1979, frequency = 12), type = "additive"))

