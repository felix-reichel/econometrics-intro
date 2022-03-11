# ---------------------------------------------------------
# Course: Exploratory Data Analysis Using R 
# Exercise: Statistical Project: SuicideChina (Stat2Data)
# Description: Suicide Attempts in Shandong, China
# Source: https://vincentarelbundock.github.io/Rdatasets/
# Author: Felix Reichel
# ---------------------------------------------------------

#setwd("/Users/felixreichel/Documents/UNI/2021W/Explorative_R/stat_project")
filename <- "SuicideChina.csv"      # Suicide Attempts in Shandong, China
suicideChina <- read.csv(filename, stringsAsFactors = TRUE)
attach(suicideChina)


str(suicideChina)
colnames(suicideChina)
head(suicideChina, 5)

# univariat quantitativ: Age

# mean, var, sd
mean(Age)
var(Age)
sd(Age)

minAge <- Age[which.min(Age)]
maxAge <- Age[which.max(Age)]

fivenum(Age)
summary(Age)

hist(Age)
hist(Age, breaks = seq(minAge, maxAge, by = 1))
hist(Age, freq = FALSE, breaks = seq(minAge, maxAge, by = 1))
lines(density(Age), col = "red")

boxplot(Age)

table(Age)
table(Age)[which.max(table(Age))]
table(Age)[which.min(table(Age))]



# univariat qualitativ: method
table(method)
methodTab <- table(method)
barplot(methodTab)

prop.table(methodTab)
barplot(prop.table(methodTab))

pie(methodTab)


cor(Age, Year)   # < |0.05|
cor(Age, Month)  # < |0.05|
cor(Month, Year) # < |0.05|

cor.test(Age, Year)  # => p > 0.05 => H0 wird beibehalten, Korrelationskoeffizient nicht signifikant von 0 verschieden
cor.test(Age, Month) # => p > 0.05 => H0 wird beibehalten, Korrelationskoeffizient nicht signifikant von 0 verschieden
cor.test(Month, Year)# => p > 0.05 => H0 wird beibehalten, Korrelationskoeffizient nicht signifikant von 0 verschieden

# Hier möchte ich den Zusammenhand zwischen Ausbildungsstatus und Erfolg des Suizidversuches untersuchen.

# Education (Factor), Died (Binärvariable)

table(Education, Died)            
tabelle <- table(Education, Died) 

prop.table(tabelle) 

mosaicplot(tabelle)
spineplot(tabelle)


# Gender/Sex,  Died (Erfolg des Suizidversuches)
table(Sex, Died)  
tabelle <- table(Sex, Died) 
prop.table(tabelle) 
mosaicplot(tabelle)
spineplot(tabelle)



# Erfolg des Suizidversuchs erklärt durch Alter?
spineplot(Died ~ Age)

tabelle <- spineplot(Died ~ Age) 

plot(Died ~ Age, breaks = fivenum(Age))

Age2 <- cut(Age, fivenum(Age), include.lowest = TRUE)
summary(Age2)

tabelle <- table(Age2, Died)
prop.table(tabelle, 1)


# Forschungsdesigns
# Vorhersagen, Stochastische Prozesse, ML Modelle
# Korrelation != Kausalität



detach(suicideChina)

# Source: https://vincentarelbundock.github.io/Rdatasets/doc/Ecdat/Macrodat.html
filename <- "Macrodat.csv"        # Macroeconomic Time Series for the United States
macrodat <- read.csv(filename)

str(macrodat)
colnames(macrodat)
head(macrodat, 5)

attach(macrodat)
# Zusammenhang zwischen Arbeitslosigkeit und 1 Jahres - US Staatsanleihen Zinssatz
cor(lhur, fygt1) #  0.3733959

# lhur = unemployment rate (average of months in quarter)
# fygt1 = 1 year treasury bond interest rate (last month in quarter)

plot(lhur, fygt1)
lm <- lm(lhur ~ fygt1) # lhur erklärt durch fygt1
abline(lm) 

summary(lm) # beta0 + beta1 stat. signifikant, Adjusted R-squared:  0.1342 

# Fazit:
# Die Arbeitslosenrate in den US (Durchschnitt der Monate in Quartal) ist mit einem 
# Pearson-Korrelationskoeffizient von 0,37 positiv mit der 1 Jahres US Anleihen Zinsrate
# (Letzter Monat im Quartal korreliert)

# Makroökonomischer Begründungsversuch:
# Wenn die Zinsrate auf US-Anleihen steigt wird die Währung abgewertet. Man spricht dann von Preisinflation
# Laut der Philips Kurve (1958) sollte bei einer hohen Inflation die Arbeitslosigkeit wieder sinken.
# Man sieht hier an den empirischen Daten, dass das hier nicht der Fall ist.

# Sieht man sich das gefittet lineare Modell an, sieht dass beide Regressionskoeffizienten (beta0 + beta1) 
# statistisch signifikant auf dem 1% - Niveau sind. Adjusted R-squared:  0.1342 drückt aus, dass 
# sich lediglich 13,4 % von lhur (Arbeitslosigkeit) durch fygt1 (1 Jahres US anleihenzins) erklären lassen.
# Man braucht hier vermutlich ein anderes Regressionsmodell dass zu einem höheren R^2 führt.



