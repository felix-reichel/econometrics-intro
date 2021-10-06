# Course: Time series analysis 1
# Author: Felix Reichel
require(astsa)

# Question: Do the series exhibit trend, seasonality, cycle or outliers? Do you think an additive
# or a multiplicative model is appropriate?

time(gtemp) # 1880 - 2009
plot(gtemp)  

# Answer: 
# Trend: Since approx. 1950 an upward-trend can be seen.
# Seasonality: Not observed using one data-point per year only
# Cycle: Not observed using one data-point per year only
# Outliers: E.g. 1950
# Model: Multiplicative model is more suitable.

plot(speech)

# Answer: 
# Trend: No trend. Volatility seems to decrease -> ARCH/GARCH
# Seasonality: Yes ;)
# Cycle: Approx 3 cycles each 200ms => approx. 67ms cycle time
# Outliers: Every cycle (marks the beginning/end of a cycle)
# Model: Additive model

plot(nyse)
plot(window(nyse, 1500, 2000))

# Answer: 
# Trend: -
# Seasonality: -
# Cycle: -
# Outliers: Yes, approx. 950th day, approx. 1450th day, e.g.
# Model: Additive model

plot(gnp)
# Trend: An upward-trend can be observed.
# Seasonality: Hits plateau every ~10yrs (breaking the upward-trend)
# Cycle: -
# Outliers: -
# Model: Multiplicative model

plot(EQ5)

# Trend: - Seems to get more volatile -> ARCH/GARCH model
# Seasonality: Seems pretty random to me
# Cycle: - 
# Outliers: Yes approx. 1450th day
# Model: - 

plot(birth)
# Trend: Multiple upward and downward trends.
# Seasonality: Yes
# Cycle: Each year
# Outliers: Each year marking a new cycle
# Model: - 

