require(fGarch)

plot(ts(sp500dge))

sp_garch <- garchFit(
  ~ arma(0,1) + garch(1,1),
  data = ts(100 * sp500dge),
  trace= FALSE)

summary(sp_garch)
plot(sp_garch)
