# ---------------------------------------------------------
# Course: Time series analysis
# Exercise: 17th / AR(2) processes
# Author: Felix Reichel
# ---------------------------------------------------------
require(plotrix)

phi1 <- c(0.3, 0.5, 0.6, -0.9, 0.4)
phi2 <- c(-0.1, -0.8, 0.4, 0.5, 0.8)

zx_results <- list()
zy_results <- list()

for (i in 1:5) {
  discr <- phi1[i] ^ 2 + 4 * phi2[i]
  if (discr >= 0) {
    
    zx_results[[i]] <- c(-phi1[i]+sqrt(discr), -phi1[i]-sqrt(discr))/(2*phi2[i])
    zy_results[[i]] <- rep(0,2)
  } else {
    zx_results[[i]] <- rep(-phi1[i]/(2*phi2[i]),2)
    zy_results[[i]] <- c(1,-1)*(sqrt(-discr)/(2*phi2[i]))
  }
}


plot(c(-3.5, 3.5), c(-3.5,3.5), type = "n",xlab="x",ylab="y")
draw.circle(x=0, y=0, radius=1,border="blue")

for (i in 1:5) {
  points(zx_results[[i]],zy_results[[i]],pch=20,col=i)
}



sim_ar2 = function(phi1, phi2, Tt, sgma2 = 1) {
  set.seed(2345)
  eps <- rnorm(Tt) * sqrt(sgma2)
  y <- rep(0, Tt)
  y[1] <- eps[1]
  y[2] <- phi1 * y[1] + eps[1]
  
  for (t in 3:Tt){
    y[t]= phi1 * y[t-1] + phi2 * y[t-2] + eps[t]
  }
  return(y)
}

Tau <- 100
ar_2_results <- list()

for (i in 1:5) {
  ar_2_results[[i]] <- sim_ar2(phi1[i], phi2[i], Tau)
}

plot(ar_2_results[[1]], type = "l")
acf(ar_2_results[[1]])
pacf(ar_2_results[[1]])

plot(ar_2_results[[2]], type = "l")
acf(ar_2_results[[2]])
pacf(ar_2_results[[2]])

# phi1+phi2 = 1
plot(ar_2_results[[3]], type = "l") 
acf(ar_2_results[[3]])
pacf(ar_2_results[[3]])

plot(ar_2_results[[4]], type = "l") 
acf(ar_2_results[[4]])
pacf(ar_2_results[[4]])

plot(ar_2_results[[5]], type = "l") 
acf(ar_2_results[[5]])
pacf(ar_2_results[[5]])







