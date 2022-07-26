# 6th assignment sheet
# course: Explorative Data analysis Using R
# author: Felix Reichel K12008176

#setwd("/Users/felixreichel/Documents/UNI/2021W/Explorative_R/uebungsblatt_06")
# 1.
# a.
BookClub <- read.csv2(file = "BookClub.csv", stringsAsFactors = TRUE)
#structure(BookClub)
attach(BookClub)

# Univariate EDA of qualitative variable $choice
summary(choice) # Häufigkeitstabelle (absolut)
table(choice) # Häufigkeitstabelle (absolut)
choiceTab <- table(choice)
prop.table(choiceTab) # Häufigkeitstabelle (relativ)
choiceTab/sum(choiceTab) # Häufigkeitstabelle (relativ)
plot(choice) # Säulendiagramm
barplot(choiceTab) # Säulendiagramm
barplot(prop.table(choiceTab), horiz = TRUE) # Balkendiagramm (relativ)
pie(choiceTab) 

dim(BookClub)[1]
# Fazit: Rund 69% (absolut 900) haben bei Choice Nein angegeben und 31% (absolut 400) Ja.
# Außerdem ist für alle Zeilen ein Wert vorhanden. vlg. mit 1300 = dim(BookClub)[1]

# b.
# # Univariate EDA of quantitative variable $freq
mean(freq)
var(freq); sd(freq)
fivenum(freq)
summary(freq)
hist(freq)
plot(density(freq))
hist(freq, freq = FALSE)
lines(density(freq), col = "red")
boxplot(freq) # weitere Komprimierung der Daten (nützlicher für mehrere Gruppen)
table(freq) # Häufigkeitstabelle für diskrete Variablen

# Fazit: Im Mittel ist die BuchClub Frequenz bei 12. 
# Die Verteilung der diskreten Werte ist linksschief, sprich niedrigere Werte sind häufiger.
# Der Boxplot zeigt einige Ausreißer nach oben.

# c.
newObj <- fivenum(freq)
saveRDS(newObj, file = "BookClubFreqTukey.rds")
rm(list = ls())
readObj <- readRDS(file = "BookClubFreqTukey.rds")
str(readObj)

# d.
setwd("/Users/felixreichel/Documents/UNI/2021W/Explorative_R/uebungsblatt_06")
load("Umfrage.rda")
#structure(Umfrage)
attributes(Umfrage)
attach(Umfrage)

# qualitative univariate EDA of $M15.romantic
summary(M15.romantic) # Häufigkeitstabelle (absolut)
table(M15.romantic) # Häufigkeitstabelle (absolut)
plot(M15.romantic) # Säulendiagramm
barplot(prop.table(table(M15.romantic)), horiz = TRUE) # Balkendiagramm (relativ)
# pie(romaticTab) 
# Fazit: 8276 haben bei romantisch Nein angekreuzt, 
#        5012 haben Ja angekreuzt und 1283 enthalten sich.

# quantitative univariate EDA of $income
mean(income)
var(income); sd(income)
fivenum(income)
summary(income)
hist(income)
boxplot(income)
table(income)
income2 <- na.omit(income)
plot(density(income2))
hist(income2, freq = FALSE)
lines(density(income2), col = "red")
# Fazit: Im Mittel beträgt das angegebene Einkommen 35155.0 (2984 NA's / Enthaltungen)

# e.
# Export
write.table(Umfrage, file = "Umfrage_EN.csv", sep = ",", dec = ".", col.names = TRUE)
write.table(Umfrage, file = "Umfrage_DE.csv", sep = ";", dec = ",", col.names = TRUE)

# 2.
# a.
setwd("/Users/felixreichel/Documents/UNI/2021W/Explorative_R/uebungsblatt_06")
load("Umfrage.rda")
#structure(Umfrage)
attach(Umfrage)

# income
mean(income)
var(income); sd(income)
fivenum(income)
summary(income)
hist(income)
boxplot(income)
table(income)
income2 <- na.omit(income)
plot(density(income2))
hist(income2, freq = FALSE)
lines(density(income2), col = "red")
# Fazit: Im Mittel beträgt das angegebene Einkommen 35155.0 (2984 NA's / Enthaltungen)

# expenditure
summary(expenditure)
hist(expenditure)
boxplot(expenditure)
table(expenditure)
expenditure2 <- na.omit(expenditure)
plot(density(expenditure2))
hist(expenditure2, freq = FALSE)
lines(density(expenditure2), col = "red")
# Fazit Die Verteilung von expenditure sieht ähnlich zu Verteilung von income aus.
# Im Mittel betragen die Ausgaben 967.98 (3214 NA's / Enthaltungen)

# b. multivariate EDA income ~ occupation
summary(income)
summary(occupation)
plot(income ~ occupation)
tapply(income, occupation, summary)
# Fazit: Selbständige haben den größten Ausreißer nach oben.
# Leit. Ang. verdienen im Mittel mehr. Arbeitslose und Studenten am wenigsten.
# Bei den Pensonen gibt es ein paar beachtliche Ausreißer. (hoher als jener
# eines Leit. Ang., etc. ...)

# c. multivariate EDA gender, accomodation
# Kontigenztafeln
table(gender, accomodation) 
xtabs(~ gender + accomodation)
tabelle <- table(gender, accomodation)
prop.table(tabelle, 1)
mosaicplot(tabelle)
spineplot(tabelle)
# Fazit Frauen leben öfter im Hotel oder auf der Farm. Alle anderen Arten
# von accomodation werden von Mönnern häufiger gewählt

# d. multivariate EDA accomodation ~ expenditure
plot(accomodation ~ expenditure)
tabelle <- spineplot(accomodation ~ expenditure) # Kontigenztafel
tabelle
plot(accomodation ~ expenditure, breaks = fivenum(expenditure))
# Man sieht dass bei häheren Ausgaben öfter das Hotel als Unterkunft gewählt wird.

# e. Multivariate EDA: age, income and expenditure
search()
  # length(Umfrage$age)
  # length(Umfrage$age[!is.na(Umfrage$income)])
  # not_na_income <- na.omit(income)
  # cor(c(age[!is.na(income)]), c(not_na_income), method="pearson")
cor(age, income, use = "complete.obs")
cor(age, expenditure, use = "complete.obs")
# Alter ist mit Einkommen und Ausgaben negativ korreliert. Das könnte an Altersarbeitslosigkeit liegen.
# und falls mehr Beobachtungen von älteren Personen sind.
cor(expenditure, income, use = "complete.obs")
# Ausgaben und Einkommen sind positiv korreliert. r = 0,21

# Streudiagramme
plot(expenditure, income)
plot(age, income)
plot(age, expenditure)

# finally
detach(Umfrage)

# 3.
# a.
IS <- datasets::InsectSprays

# i. 
head(IS, 30)
# ii.
attach(IS)
summary(count) # Häufigkeitstabelle (absolut)
table(count) # Häufigkeitstabelle (absolut)
# iii.
table(count, spray) 
# same as: xtabs(~ count + spray)

# b.
count.cat <- cut(count,breaks = c(-Inf, 9, 18, Inf),labels = c("wenige", "mittel", "viele"))
table(count.cat)
table(count.cat, spray)
barplot(table(count.cat))
barplot(table(count.cat, spray))
barplot(table(count.cat, spray), beside = TRUE)

# c.
median(count) # = 7
mean(count) # = 9.5
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(count) # = 3

attributes(table(count, spray))
attributes(xtabs(~ count + spray))
attributes(IS)

# Es gibt kein Attribute $freq, trotzdem versuche ich den Befehl zu erklären:
# Befehl: names(freq)[which(freq == max(freq))]
# Erklärung: 
# 1. Die Funktion names soll das Set an Namen aus dem Object freq extrahieren
# 2. Es soll nur jener Name zurückgegeben werden wo freq dem Maximalwert entspricht.

# d.
range(count)
quantile(count)
quantile(count, probs = c(0.05, 0.95))
summary(count)
# Passt.

# e.
AQ <- datasets::airquality
dim(AQ)
colnames(AQ)
#structure(AQ)
head(AQ, 6)
mean(AQ$Solar.R, na.rm = TRUE)

# 4.

summary(attitude)
# a.
attitude
head(attitude, 10)
med.att <- rapply(attitude, median)
med.att

# b.
sweeped <- sweep(data.matrix(attitude), 2, med.att) # subtract the column medians
summary(sweeped) # -> Mediane alle 0 (normalisiert)

# c.
mean.att <- rapply(attitude, mean)
stdev.att <- rapply(attitude, sd)

# d.
sweeped2 <- sweep(data.matrix(attitude), 2, mean.att) 
sweeped3 <- sweeped2 / sweep(data.matrix(attitude), 2, stdev.att)

mean(sweeped3)
sd(sweeped3)

# e. (-0,2)
standardized <- scale(attitude)

mean(standardized)
sd(standardized)
# = Stimmt nicht mit d. überein -> Fehler -> Mit Musterlösung vergleichen!
