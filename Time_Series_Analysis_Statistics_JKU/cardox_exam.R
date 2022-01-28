setwd("C:\Users\ccref\Documents\GitHub\learning-econometrics\Time_Series_Analysis_Statistics_JKU")
gebdate <- 20000117

# Load Requirements
require(astsa)
require(tseries)
require(forecast)
# 1.

# Seed auf Geburtsdatum (YYYYMMDD) setzen 
set.seed(gebdate)

# Ganzahlige Zufallszahl 1 <= x <= 20 generieren. (mit aktuellen Seed randomInt = 5)
randomInt <- floor(runif(1, min = 1, max = 20))

# Startjahr bestimmen
J <- 1980 - randomInt # J=1975

# Zeitreihe astsa::cardox ansehen
# ?cardox 


?cardox
# Monthly mean carbon dioxide (in ppm) measured at Mauna Loa Observatory, Hawaii. 
freq <- frequency(cardox)
J_end <-2016+1-(1/freq) # 2017-(1/12) entspricht Dec. 2016

# Zeitreihenobjekte anlegen

full_cardox_ts <- ts(data = cardox, start=1958+(2/12), end = 2019-(2/12), frequency = freq) # Ganze Zeitreihe als Objekt
length(full_cardox_ts) # The format is: Time-Series [1:729] from March, 1958 to November 2018

cardox_ts <- window(x = full_cardox_ts, start = J, end = J_end) # Zeitreihenausschnitt für das Exam

# 2.

# Zeitreihe plotten
plot(cardox_ts)
abline(h = mean(cardox_ts), col="red") # mean

# Ausschnitte plotten
# plot(window(cardox_ts, start=2010, end=J_end))
plot(window(cardox_ts, start=2014, end=J_end))

# Zusätzliche Überprüfung mittels decompose
decomposed <- decompose(cardox_ts, type = "additive")
# Ein klassischen additives Dekompositionsmodell ist hier ausreichend, 
# da weder saisonale noch irreguläre Schwankungen (sichtbar) proportional zum Level der Zeitreihe ansteigen.
plot(decomposed)

# Irreguläre Komponente normalverteilt?
qqnorm(decomposed$random)
qqline(decomposed$random)
hist(decomposed$random)
decomposed$random
jarque.bera.test(na.omit(decomposed$random)) # Interpretation!

### Zeitreihenkomponenten - Analyse Ergebnisse: ###

# Trend Komponente µt: Die Zeitreihe besitzt einen positiven Trend µt. 
  # Erläuterung: 
  # Die Zeitreihe beginnt deutlich unterhalb dem Mittelwert und bleibt überhalb Mittelwert sobald dieser überschritten wurde, 
  # ausgenommen von seasonalen Schwankungen, welche im darauffolgenden Jahr einem Wert unterhalb des bereits überschrittenen Mittelwert führt

# Saisonale Komponente st: Die Zeitreihe besitzt seasonale Schwankungen.
  # Erläuterung: Die tiefsten Werte scheinen beinahe zu immer im September erreicht zu werden.

# Irreguläre Komponente et: Entfernt man die Trendkomponente und Saisonkomponente bleiben irreguläre Fluktuationen über.

# Nicht saisonale Schwankungen ct: Keine /Nicht vorhanden 


# 3.
# Wir haben festgestellt dass wird eine lineare Trendkomponente und (vermutlich) additive saisonale Komponente haben
# daher Holt-Winters Forecasting als Methode zum exponentiellen Glätten auswählen

cardox_ts_exp_a <- HoltWinters(cardox_ts, seasonal = "additive")
# Zum Vergleich HoltWinters mit multiplikativer saisonale Komponente
cardox_ts_exp_m <- HoltWinters(cardox_ts, seasonal = "multiplicative")

plot(cardox_ts_exp_a)
plot(cardox_ts_exp_m)
# Sehen beide ganz gut aus! 
# Aber welches schneidet besser hier ab?
 
# Modelle durch Splitting in Training und Test Set validieren
cardox_len <- length(cardox_ts)

test_set_yrs <- 4 # die letzten 4 Jahre als Test Set zur Validierung entsprich 48 von 504 Beobachtungen (~10%)
test_set_len <- test_set_yrs * freq    
train_set_len <- cardox_len - test_set_len
cut <- J_end - test_set_yrs

training_set <- window(cardox_ts, start = J, end = cut) 
test_set <- window(cardox_ts, start = cut, end = J_end) 

cardox_ts_train_exp_a <- HoltWinters(training_set, seasonal = "additive")
cardox_ts_train_exp_m <- HoltWinters(training_set, seasonal = "multiplicative")

cardox_ts_train_exp_a_pred = predict(cardox_ts_train_exp_a, n.ahead = test_set_len, prediction.interval = T)
cardox_ts_train_exp_m_pred = predict(cardox_ts_train_exp_m, n.ahead = test_set_len, prediction.interval = T)

plot(cardox_ts_train_exp_a, cardox_ts_train_exp_a_pred, lwd = 2)
plot(cardox_ts_train_exp_m, cardox_ts_train_exp_m_pred, lwd = 2) # Konfidenzintervalle sind größer beim Modell mit multiplikativer Saisonkomponente

actual_values <- tail(cardox_ts, test_set_len)

hw_a_predicted <- cardox_ts_train_exp_a_pred[,1]
hw_m_predicted <- cardox_ts_train_exp_m_pred[,1]

# Compare ME, RMSE, MAE, MAPE  Errors
rbind(accuracy(actual_values, hw_a_predicted), accuracy(actual_values, hw_m_predicted))

# Bei allen drei Fehlern (MAE, MAPE, MSE) schneidet das Modell mit der multiplikativen saisonalen Kompoenente besser, 
# weswegen ich fortan nur Holt-Winters-Multiplicative Forecasting für Vorhersagen verwenden werde.

# Nachdem das Holt-Winters-multiplikative Modell besser abschneidet heißt dass das die saisonalen Schwankungen mit Fortschreiten (kaum sichtbar, aber akkumulativ)
# größer werden und nicht konstante Werte bleiben.


# 4.

# Eine Zeitreihe mit einem Trend µt ist nicht stationär, weil der Erwartungswert nicht konstant ist, sondern eben von t abhängt.

# Bildung von Differenzenprozesse
# Wir "differenzieren" die Zeitreihe bei Lag 12 um die Saisonalität und anschließend bei Lag 1 um den Trend zu entfernen.

d1 <- diff(cardox_ts, lag = freq)
plot(d1) # sieht noch nicht stationär aus

# Einheitswurzeltests für Stationarität von d1
adf.test(d1) # p-value = 0.01 < 0.05 => H1 => Stationary 
kpss.test(d1) # p-value = 0.01 < 0.05 => H1 => No Level Stationarity

d2 <- diff(d1)
plot(d2)

# Einheitswurzeltests für Stationarität von d2
adf.test(d2) # p-value = 0.01 < 0.05 => H1 => Stationary 
kpss.test(d2) # p-value = 0.1 > 0.05 => H0 => Level Stationarity

# Plotten von ACF und PACF
acf(d2)
acf(d2, lag.max = 60)
# ACF schlägt bei Lag 1 aus und danach nicht mehr regelmäßig. Bzw. meißt nur leicht
# über den Grenzen, daher entscheide ich mich für eine Ordnung von p = 1 und P = 0 für das saisonale Modell.

pacf(d2)
pacf(d2, lag.max = 60)
# Die PACF zeigt regelmäßige Ausschläge beim 1. bzw. danach jeden 12ten Lag, 
# Daher entscheide ich mich für q = 1 und Q = 1 für das saisonale Modell

# Ich beabsichtige ein SARIMA(1,1,1)x(0,1,1) Lag=12 Modell zu fitten.

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
jarque.bera.test(residuals) # Interpretation!

# Informationskritieren (Weniger bedeuted besserer Fit)
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
# Tatsächlich hat aber mein gewähltes Modell einen geringeren BIC als das von auto.sarima gewählte Modell.


# 5.
# Vorhersagen der nächsten 2 Jahre verglichen mit der echten Zeireihen

# Die nächsten 2 Jahre als Zeitreihenobjekt
cardox_ts_next_2_yrs <- window(x = full_cardox_ts, start = 2017)

# Modell 1 ... cardox_ts_exp_m .... Holt-Winters Exponentielles Glätten mit multiplikativer Saisonkomponente
# Modell 2 ... cardox_sarima_m1 ... SARIMA(1,1,1)x(0,1,1) Lag=12 

holtWinters_m_pred <- predict(cardox_ts_exp_m, n.ahead = 24, prediction.interval = T)
sarima_pred <- predict(cardox_sarima_m1$fit, n.ahead = 24)
auto_sarima_pred <- predict(cardox_auto_sarima_m$fit, n.ahead = 24)

ts.plot(cardox_ts_next_2_yrs)
lines(holtWinters_m_pred[,1], col="red")
lines(sarima_pred$pred, col="blue")
lines(auto_sarima_pred$pred, col="green")

# Die beiden SARIMA Modelle schneiden annähernd gleich und schneiden hier deutlich besser als HoltWinters Exp. Glätten ab.
rbind(
  accuracy(cardox_ts_next_2_yrs, cardox_ts_exp_m_pred[,1]),
  accuracy(cardox_ts_next_2_yrs, cardox_sarima_m1_pred$pred),
  accuracy(cardox_ts_next_2_yrs, cardox_auto_sarima_pred$pred))


# 6.

# Es ist nicht logisch hier nach Volatilität zu modellieren weil es hier um eine Kohlendioxid Menge in der Luft an einem Bestimmten Ort (Mauna Loa Observatory, Hawaii) handelt.
# CO2 ist meines Wissens nach zu ca. 4% in der Atemluft und wird sich verteilen. Wenn der Ort nicht selbst CO2 Emissionen erzeugt kann es keine starken Veränderungen in der Varianz (Schocks) geben.
# Daher modelliere ich die Volatilität nicht.

# 2. Argument: Es handelt sich hier um Naturgegebenheiten und daher konstante Varianz bei einer kurzen Beobachtungsdauer.

# Volatiliätsmodelle sind sinnvoll bei:
# Perioden mit höheren und niedrigeren Schwankungen (Varianz)
# Cluster von Volatitäten
