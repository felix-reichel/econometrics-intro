setwd("./GitHub/learning-econometrics/Time_Series_Analysis_Statistics_JKU")
gebdate <- 20000117

require(astsa)
require(tseries)
require(forecast)

# 1.
# Seed auf Geburtsdatum (YYYYMMDD) setzen 
set.seed(gebdate)

# Ganzahlige Zufallszahl 1 <= x <= 20 generieren (mit aktuellen Seed randomInt = 5)
randomInt <- floor(runif(1, min = 1, max = 20))

# Startjahr bestimmen
J <- 1980 - randomInt # J=1975

# Zeitreihe ansehen
# ?cardox
# The format is: Time-Series [1:729] from March, 1958 to November 2018
freq <- frequency(cardox)
J_end <- 2016+1-(1/freq) # 2017-(1/12) entspricht Dec 2016

# Zeitreihenobjekte anlegen
# Ganze Zeitreihe als Objekt
full_cardox_ts <- ts(data = cardox, start=1958+(2/12), end = 2019-(2/12), frequency = freq) 
length(full_cardox_ts) # = 729

# Zeitreihenausschnitt für das Exam
cardox_ts <- window(x = full_cardox_ts, start = J, end = J_end) 

# 2.

# Zeitreihe plotten
plot(cardox_ts)
abline(h = mean(cardox_ts), col="red") # mean

# Ein paar neuere Zeitreihenausschnitte plotten
# plot(window(cardox_ts, start=2010, end=J_end))
plot(window(cardox_ts, start=2014, end=J_end))

# Zusätzliche Überprüfung mittels decompose
decomposed <- decompose(cardox_ts, type = "additive")
plot(decomposed)
# Ein klassischen additives Dekompositionsmodell scheint hier ausreichend zu sein, 
# da weder saisonale noch irreguläre Schwankungen ("sichtbar") proportional zum Level der Zeitreihe ansteigen.
# Später wird sich jedoch herausstellen, dass die saisonale Komponente multiplikativ besser modelliert werden kann.

# Ist die Irreguläre Komponente normalverteilt?
qqnorm(decomposed$random)
qqline(decomposed$random)
# Der QQ-Plot zeigt leider einige Ausreißer ("dicke Enden")
# => Vermutung: Irreguläre Komponente nicht normalverteilt
hist(decomposed$random)
# Das Histogramm ist leicht linksschief und weist einen isolierten Balken auf. (Ausreißer)
jarque.bera.test(na.omit(decomposed$random)) 
# Interpretation
# X-squared = 17.451 > 6 und der p-Wert = 0.0001624 < 0.05 => H0 wird daher verworfen, 
# Ergebnis: Die irreguläre Komponente ist in unserem Zeitreihenausschnitt nicht normalverteilt.


### Zeitreihenkomponenten - Analyse Ergebnisse: ###
# Trend Komponente (µt): Die Zeitreihe besitzt einen positiven Trend µt. 
  # Erläuterung: 
  # Die Zeitreihe beginnt deutlich unterhalb dem Mittelwert und bleibt überhalb Mittelwert sobald dieser überschritten wurde, 
  # ausgenommen von seasonalen Schwankungen, welche im darauffolgenden Jahr dazu führen das ein Wert noch unterhalb des bereits überschrittenen Mittelwert ist

# Saisonale Komponente (st): Die Zeitreihe besitzt saisonale Schwankungen.
  # Erläuterung: Die tiefsten Werte scheinen beinahe zu immer im September erreicht zu werden.

# Irreguläre Komponente (et): Entfernt man die Trendkomponente und Saisonkomponente bleiben irreguläre Fluktuationen über. 
  # Erläuterung: Diese scheinen lt. QQ-Plot, Histogram und Jarque-Bera-Statistik (X-squared > 6) im Modell mit additiver saisonaler Komponente nicht normalverteilt zu sein.
  # Ich glaube dass dieses Ergebnis an den letzten Werten der Zeitreihe liegt und das gerade wegen den letzten Werten der Zeitreihe wir zu einem Modell mit multiplikativer 
  # saisonaler Komponente greifen müssen.
  # Überprüfung Vermutung:
decomposed2 <- decompose(window(cardox_ts, end=2015), type = "additive")
jarque.bera.test(na.omit(decomposed2$random)) # X-squared  = 6.9479 > 6 => H1 => Nicht Normalverteilt

decomposed3 <- decompose(window(cardox_ts, end=2000), type = "additive")
jarque.bera.test(na.omit(decomposed3$random)) # X-squared  = X-squared = 2.5633 < 6 => H0 => Normalverteilt
 
decomposed_mult <- decompose(cardox_ts, type = "multiplicative")
plot(decomposed_mult)
qqnorm(decomposed_mult$random)
qqline(decomposed_mult$random)
hist(decomposed_mult$random)
jarque.bera.test(na.omit(decomposed_mult$random))
# Ergebnis: Die irreguläre Komponente im Modell mit multiplikativer saisonale Komponente ist für den gesamten Zeitreihenausschnitt normalverteilt!

# Nicht saisonale Schwankungen (ct): Keine /Nicht vorhanden 

# 3.
# Wir haben festgestellt, dass wir eine lineare Trendkomponente und multiplikative saisonale Komponente haben
# daher wählen wir Holt-Winters Forecasting mit multiplikativer saisonaler Komponente als Methode zum exponentiellen Glätten aus.

cardox_ts_exp_m <- HoltWinters(cardox_ts, seasonal = "multiplicative")
# Nur zum Vergleich noch HoltWinters mit additiver saisonale Komponente
cardox_ts_exp_a <- HoltWinters(cardox_ts, seasonal = "additive")

plot(cardox_ts_exp_m)
plot(cardox_ts_exp_a)
# Sehen eigentlich beide ganz gut aus! 
# Aber schneidet unser Modell mit multiplikativer saisonaler Komponente auch besser ab?
# Erinnerung: Überprüfung mit den späteren Werten der Zeitreihe, sollte zu deutlich besseren Ergebnissen für unser multiplikatives Modell führen.
 
# Modelle durch Splitting in Training und Test Set validieren
cardox_len <- length(cardox_ts)

test_set_yrs <- 4 # die letzten 4 Jahre als Test Set zur Validierung entsprich 48 von 504 Beobachtungen (~10%)
test_set_len <- test_set_yrs * freq    
train_set_len <- cardox_len - test_set_len
cut <- J_end - test_set_yrs

training_set <- window(cardox_ts, start = J, end = cut) 
test_set <- window(cardox_ts, start = cut, end = J_end) 

cardox_ts_train_exp_m <- HoltWinters(training_set, seasonal = "multiplicative")
cardox_ts_train_exp_a <- HoltWinters(training_set, seasonal = "additive")

cardox_ts_train_exp_m_pred = predict(cardox_ts_train_exp_m, n.ahead = test_set_len, prediction.interval = T)
cardox_ts_train_exp_a_pred = predict(cardox_ts_train_exp_a, n.ahead = test_set_len, prediction.interval = T)

plot(cardox_ts_train_exp_m, cardox_ts_train_exp_m_pred, lwd = 2) # Konfidenzintervalle sind größer beim Modell mit multiplikativer Saisonkomponente
plot(cardox_ts_train_exp_a, cardox_ts_train_exp_a_pred, lwd = 2)

actual_values <- tail(cardox_ts, test_set_len)

hw_m_predicted <- cardox_ts_train_exp_m_pred[,1]
hw_a_predicted <- cardox_ts_train_exp_a_pred[,1]

# Compare ME, RMSE, MAE, MAPE  Errors
rbind(accuracy(actual_values, hw_m_predicted), accuracy(actual_values, hw_a_predicted))
# => Bei allen drei Fehlern (MAE, MAPE, MSE) schneidet das Modell mit der multiplikativen saisonalen Kompoenente besser ab.


# 4.

# Eine Zeitreihe mit einem Trend µt ist nicht stationär, weil der Erwartungswert nicht konstant ist, sondern eben von t abhängt.

# Bildung von Differenzenprozesse
# Wir "differenzieren" die Zeitreihe bei Lag 12 um die Saisonalität und anschließend bei Lag 1 um den Trend zu entfernen.

d1 <- diff(cardox_ts, lag = freq)
plot(d1) # sieht noch nicht stationär aus
# Scheint noch einen Trend nach oben zu haben und kehrt nicht regelmäßig zum Erwartungswert zurück.

# Einheitswurzeltests für Stationarität von d1
adf.test(d1) # p-value = 0.01 < 0.05 => H1 => Stationary 
kpss.test(d1, null= "Level") # p-value = 0.01 < 0.05 => H1 => No Level Stationarity
kpss.test(d1, null = "Trend") # p-value = 0.023 < 0.05 => H1 => No Trend Stationarity
# => Laut KPSS Test wie vermutet noch nicht stationär

d2 <- diff(d1)
plot(d2)

# Einheitswurzeltests für Stationarität von d2
adf.test(d2) # p-value = 0.01 < 0.05 => H1 => Stationary  
kpss.test(d2, null= "Level") # p-value = 0.1 > 0.05 => H0 => Level Stationarity
kpss.test(d2, null= "Trend") # p-value = 0.1 > 0.05 => H0 => Trend Stationarity

# ADF und KPSS Tests der 2. Differenzen schlagen vor, dass dieser Prozess nun stationär ist.
# Im Plot sieht dieser Prozess auch schon ziemlich stationär aus! (Kehrt regelmäßig zum Erwartungswert zurück)

# Plotten von ACF und PACF
acf(d2)
acf(d2, lag.max = 60)
# ACF schlägt bei Lag 1 aus und danach nicht mehr regelmäßig. Bzw. meist nur noch ganz leicht
# über den Grenzen, daher entscheide ich mich für eine Ordnung von p = 1 und P = 0 für das saisonale Modell.

pacf(d2)
pacf(d2, lag.max = 60)
# Die PACF zeigt regelmäßige Ausschläge beim 1. bzw. danach jeden 12ten bzw. 1. Lag, 
# Daher entscheide ich mich für q = 1 und Q = 1 für das saisonale Modell

# Ich also ein SARIMA(1,1,1)x(0,1,1) mit Lag=12 Modell zu fitten.

# Da wir eine Zeitreihe mit saisonaler Komponente haben können wir entweder ein ARIMA Modell mit seasonalen Dummies und ARIMA Fehlern fitten oder ein SARIMA Modell verwenden.
# Ich verwende letzteres, da dieses nicht explizit durch die Angabe ausgeschlossen wird.


# Fit eines geeigneten Modells
cardox_sarima_m1 <- sarima(xdata = cardox_ts, p = 1, d = 1, q = 1, P = 0, D = 1, Q = 1, S = freq)

# Beurteilung der Anpassung

# Alle p-Werte in der Ljung-Box statistik sind > 0.05 das heißt wir können H0 verwerfen sprich die Fehler sind voneinander unabhängig, was wir für unser Modell wollen
# Die Residuale sind im QQ-Plot leider nicht ganz normalverteilt wegen ein paar Ausreißer außerhalb der Intervalle ("dicke Enden")
# In der ACF der ist sind keine hohen Ausschläge mehr sichtbar. Sprich keine Korrelationen zwischen den Fehlern

# Residualanalyse
residuals <- cardox_sarima_m1$fit$residuals
qqnorm(residuals)
qqline(residuals)
hist(residuals)
jarque.bera.test(residuals)
# X-squared = 17.154 > 6 und p-value = 0.0001884 < 0.05 => Residuale nicht normalverteilt

# Informationskritieren (weniger bedeutet besserer Fit)
AIC(cardox_sarima_m1$fit)
BIC(cardox_sarima_m1$fit)


# Modellvergleich
# Aus Interesse möchte ich das gewählte Modell mit auto.sarima vergleichen
##############################################
# install.packages('bayesforecast')
# require(bayesforecast)
# auto.sarima(cardox_ts, seasonal = TRUE)
###############################################
# auto.sarima schlägt das Modell folgendes Modell vor:
# y ~ Sarima(1,1,1)(2,1,1)[12] 
cardox_auto_sarima_m <- sarima(xdata = cardox_ts, p = 1, d = 1, q = 1, P = 2, D = 1, Q = 1, S = freq)
AIC(cardox_auto_sarima_m$fit)
BIC(cardox_auto_sarima_m$fit)
# Tatsächlich hat mein gewähltes Modell aber einen geringerem Wert beim AIC und BIC als das von auto.sarima gewählte Modell.


# 5.
# Vorhersagen der nächsten 2 Jahre verglichen mit der echten Zeireihen

# Die nächsten 2 Jahre als Zeitreihenobjekt
cardox_ts_next_2_yrs <- window(x = full_cardox_ts, start = 2017)

# Modell 1 ... cardox_ts_exp_m ....... Holt-Winters Exponentielles Glätten mit multiplikativer Saisonkomponente
# Modell 2 ... cardox_sarima_m1 ...... SARIMA(1,1,1)x(0,1,1) Lag=12 
# Modell 3 ... cardox_auto_sarima_m .. SARIMA(1,1,1)x(2,1,1) Lag=12

holtWinters_m_pred <- predict(cardox_ts_exp_m, n.ahead = 24, prediction.interval = T)
sarima_pred <- predict(cardox_sarima_m1$fit, n.ahead = 24)
auto_sarima_pred <- predict(cardox_auto_sarima_m$fit, n.ahead = 24)

# Schwarz = echte Werte, Rot = HoltWinters, Blau = SARIMA, Grün = Auto-SARIMA
ts.plot(cardox_ts_next_2_yrs)
lines(holtWinters_m_pred[,1], col="red")
lines(sarima_pred$pred, col="blue")
lines(auto_sarima_pred$pred, col="green")

# Die beiden SARIMA Modelle schneiden annähernd gleich und schneiden hier deutlich besser als HoltWinters Exp. Glätten ab.
rbind(
  accuracy(cardox_ts_next_2_yrs, holtWinters_m_pred[,1]),
  accuracy(cardox_ts_next_2_yrs, sarima_pred$pred),
  accuracy(cardox_ts_next_2_yrs, auto_sarima_pred$pred))


# 6.

# Es ist meiner Meinung hier nicht notwendig nach Volatilität zu modellieren, weil es hier um eine Kohlendioxid Menge in der Luft gemessen am Mauna Loa Observatory, Hawaii handelt.
# CO2 zu ca. 4% in der Atemluft und wird sich verteilen. Wenn der Ort nicht selbst CO2 Emissionen erzeugt kann es keine starken Veränderungen in der Varianz (sprich Schocks) geben.
# Daher halt modelliere ich die Volatilität nicht.

# Volatiliätsmodelle sind sinnvoll bei:
# starken Veränderungen in der Varianz
# Perioden mit höheren und niedrigeren Schwankungen (Varianz)
# "Volatitäts-Cluster"

# z.B. Aktienkurse (unterliegen ökonomischen Schocks z.B. Finanzkrise 2008)
