# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 2nd / Descriptive analysis of ts ausbeer
# Author: Felix Reichel
# ---------------------------------------------------------
require(fpp)

#1
ausbeer <- fpp::ausbeer
?ausbeer
time(ausbeer)
frequency(ausbeer)
cycle(ausbeer)
deltat(ausbeer)

#2
plot(ausbeer)
plot(window(ausbeer, 1960, 1965))
decomposedAusbeer <- decompose(ts(ausbeer, frequency = frequency(ausbeer)))
plot(decomposedAusbeer)

#3
require(forecast)
monthplot(ausbeer)
seasonplot(ausbeer)

#4 
ausbeerFrom2000Q1 <- window(ausbeer, 2000, 2008.50)
plot(ausbeerFrom2000Q1)
monthplot(ausbeerFrom2000Q1)
seasonplot(ausbeerFrom2000Q1)
