# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 3rd / Moving averages
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)

#1
plot(gtemp)
abline(a = mean(gtemp), b = 0., col = "red", lwd = 2)

gtemp_sm1 = filter(gtemp,rep(1/5, 5))    # 5-point moving average
lines(gtemp_sm1, col = "blue", lwd = 2)

gtemp_sm2 = filter(gtemp, rep(1/21, 21)) # 21-point moving average
lines(gtemp_sm2, col="red", lwd=2)

#2
plot(birth)
abline(a = mean(birth), b = 0., col = "red", lwd = 2)
birth_sm1 = filter(birth, rep(1/5, 5))
lines(birth_sm1, col = "blue", lwd = 2)
birth_sm2 = filter(birth, rep(1/21, 21))
lines(birth_sm2, col="green", lwd=2)
