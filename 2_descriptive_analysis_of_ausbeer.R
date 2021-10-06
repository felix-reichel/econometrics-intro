# Course: Time series analysis 1
# Author: Felix Reichel
require(fpp)

#1
ausbeer <- fpp::ausbeer
help(ausbeer)
time(ausbeer)
frequency(ausbeer)
cycle(ausbeer)
deltat(ausbeer)

#2
plot(ausbeer)
plot(window(ausbeer, 1960, 1965))
#trend: Upward-trend ~[1956, 1975], no trend ~[1975, 1993], downward-trend [1993, yt]
#seasonality: Yes, lowest production in Q2, small increase Q3, Q4 highest prod. Q1 decrease
#cycle: Each yr.
#error/noise:
#mode:

#3
require(forecast)
monthplot(ausbeer)
seasonplot(ausbeer)
# The seasonality can be observed wonderfully as stated in #2.

#4 
ausbeerFrom2000Q1 <- window(ausbeer, 2000, 2008.50)
plot(ausbeerFrom2000Q1)
monthplot(ausbeerFrom2000Q1)
seasonplot(ausbeerFrom2000Q1)
