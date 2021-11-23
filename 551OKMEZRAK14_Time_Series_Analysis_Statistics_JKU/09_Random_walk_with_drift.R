# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 9th / Random walk with drift
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)
require(Metrics)

T <- 100
n <- 3

par(mfrow=c(1,1))

# random walk
y1 <- matrix(rnorm(n*T), ncol = n)
random_walk_y <- apply(y1, 2, cumsum)


# random walk with drift
y2 <- matrix(rnorm(n*T, mean = 1, sd = 5), ncol = n)
random_wal_with_drift_y <- apply(y2, 2, cumsum) 

# 1.)
matplot(random_wal_with_drift_y, type="l", lty=1, xlab="t", ylab="y", col="blue", 
        main="random walk with drift in blue", lwd=2)
matlines(random_walk_y, type="l", lty=1, xlab="t", ylab="y", col="red", 
         main="random walk", lwd=2)


# 2.)
lag1.plot(random_walk_y[,1], 1) 
plot(diff(random_walk_y[,1], lag=1, differences = 1), type="l")
plot(diff(random_wal_with_drift_y[,1], lag=1, differences = 1), type="l")

# 3.)
# Both processes are non-stationary:

# random walk:
# yt = (rho*) yt-1 + et  # et iid.~ N(0, sigma^2) # |rho| < 1
# yt-1 = yt-2 + et-1 + et
# yt-i = y0 + sum i=0, t-1 of et-i
# with y0 = 0 and et iid ~ N(0, sigma^2) 
# I: E(yt) = E(y0) + E(sum of et-i) = E(y0) = 0 # => const. mean.
# II: Var(yt) = sum i=0, t-1 Var(et-1) = t * sigma^2 => Non stationary.
# III: Cov(yt, yt-h) only dependent on lag h? => II violated. no more need to check. 

# random walk with drift:
# yt = a + yt-1 + et
# yt-1 = a + a + yt-2 + et-1 + et
# yt-2 = a + a + a + yt-3 + et-2 + et-1 + et
# yt-i = a*t + y0 + sum i=0, t-1 of et-i)
# with y0 = 0 and et iid ~ N(0, sigma^2) 
# I: E(yt) = a*t => Non stationary, because E(yt) ~ t.


