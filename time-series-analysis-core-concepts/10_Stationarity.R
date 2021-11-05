# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 10th / Stationarity
# Author: Felix Reichel
# ---------------------------------------------------------
require(astsa)
require(tseries)

T <- 100
n <- 5

par(mfrow=c(1,1))

# yt = 3 + et
y1 <- 3 + matrix(rnorm(n*T), ncol=n)
matplot(y1,type="l", lty=1,xlab="t",ylab="y", col=1:n, main="yt = 3 + et",lwd=2)

# yt = 3(-1)^t + et
y2 <- matrix(rnorm(n*T),ncol=n)

for(t in 1:nrow(y2)) {
  for(observed in 1:ncol(y2)) {
    y2[t, observed] <- 3 * (-1)^t + y2[t, observed]
  }
}
matplot(y2,type="l", lty=1,xlab="t",ylab="y", col=1:n, main="yt = 3(-1)^t + et",lwd=2)

# yt = 1 - 0.2t + et
y3 <- matrix(rnorm(n*T), ncol=n)

for(t in 1:nrow(y3)) {
  for(observed in 1:ncol(y3)) {
    y3[t, observed] <- 1 - 0.2*t + y3[t, observed]
  }
}
matplot(y3,type="l", lty=1,xlab="t",ylab="y", col=1:n, main="yt = 1 - 0.2t + et",lwd=2)

# yt = -2+et | t %% 4 = 1, 3+et | t %% 4 = 2, et | t %% 4 = 3, -1 + et | t %% 4 = 0
y4 <- matrix(rnorm(n*T), ncol=n)

for(t in 1:nrow(y4)) {
  for(observed in 1:ncol(y4)) {
    if (t %% 4 == 1) y4[t, observed] <- -2 + y4[t, observed]
    else if (t %% 4 == 2) y4[t, observed] <- 3 + y4[t, observed]
    else if (t %% 4 == 4) y4[t, observed] <- -1 + y4[t, observed]
    
  }
}
matplot(y4,type="l", lty=1,xlab="t",ylab="y", col=1:n, main="stochastic y 4",lwd=2)


# Given σ^2(et) = σ^2
# determine (weakly) stationary y and compute E(yt) and Var(yt), if (strictly) stationary compute the auto-correlation-function

# def (weakly) stationary y: 
# I: µ(t) = µ 
# II: σ^2(t) = σ^2 
# III: γ(t, s) = γ(t − s)

# 1.)
# yt = 3 + et

# I. µ(t) = 3 const. meets 1st criteria
# II. σ^2(t) = V(et) meets 2nd criteria
# III. γ(t, s) = γ(t − s):
# Cov(yt, yt+h)
# Cov(3 + et, 3 + et+h) = Cov(et, et+h) ...equal covariance for all t. meets 3rd criteria
# Therefore (weakly) stationary.

acf(y1[,3], lag.max = T)

# 2.) 
# 
# yt = 3(-1)^t + et
# I. µ(t) = 3(-1)^t ... is dependent on t and therefore does not meet criteria I.
# Therefore No stationarity.

# 3.) 
# Yt = 1 − 0.2t + ε
# I. µ(t) = -0.2t ... is dependent on t and therefore does not meet criteria I.
# Therefore No stationarity.

# 4.) 
# yt = -2+et | t %% 4 = 1, 3+et | t %% 4 = 2, et | t %% 4 = 3, -1 + et | t %% 4 = 0
# if t %% 4 = 1 # µ(t) = -2, σ^2(t) = V(et)
# if t %% 4 = 2 # µ(t) = +3, σ^2(t) = V(et)
# if t %% 4 = 3 # µ(t) =  0, σ^2(t) = V(et)
# if t %% 4 = 0 # µ(t) = -1, σ^2(t) = V(et)

row_select <- c(0,4,8,12,16,20,24,28,32,36,40);
acf(y4[row_select,3], lag.max = T)

row_select <- row_select + 1
acf(y4[row_select,3], lag.max = T)

row_select <- row_select + 2
acf(y4[row_select,3], lag.max = T)

row_select <- row_select + 3
acf(y4[row_select,3], lag.max = T)

acf(y4[,3])

