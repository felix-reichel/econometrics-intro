# ---------------------------------------------------------
# Name:         Explorative Datenanalyse mit R / Übungsblatt 1
# Autor:        Felix Reichel
# Datum:        2021-10-10
# ---------------------------------------------------------
# 1. Vektoren und Zuweisungen
# a. Erzeugen Sie einen Vektor x, der aus den geraden ganzen Zahlen von 0 bis 20 besteht.
x <- c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
# mittels Sequenz
x <- seq(from = 0, to = 20, by = 2)
# oder zufällig
x2 <- floor(runif(10, min = 0, max = 10))  # mit ungeraden ganzen Zahlen von 0 bis 10
x2 <- x * 2                                # mit geraden ganzen Zahlen von 0 bis 20

# b. Berechnen Sie die Länge des Vektors x mit Hilfe eines geeigneten Befehls.
length(x)

# c. Ermitteln Sie 𝑥2 und weisen Sie das Ergebnis einem Vektor y zu.
y <- x^2

# d. Berechnen Sie die Summe der Elemente des Vektors y mit Hilfe eines geeigneten Befehls.
sum(y)

# e. Berechnen Sie 𝑥/𝑦 sowie 𝑒𝑥/𝑦. Falls Sie richtig gerechnet haben, sind in den Ergebnissen die
#Konstanten NaN bzw. Inf enthalten. Beschreiben Sie in eigenen Worten die Bedeutung dieser
#Konstanten.
x/y 
# A: Für den ersten Wert wird 0/0 ausgeführt. 0/0 ist ein nicht definierter Ausdruck => daher NaN
exp(x)/y  # e^0/0 = 1/0 
# A: 1 geteilt durch 0 ist unendlich, denn Grenzwert lim x->0 n/x = ∞.

# 2. Funktionen, Vergleichsoperatoren und grafische Darstellung
# a. Überprüfen Sie die trigonometrische Beziehung sin2 𝑥 + cos2 𝑥 = 1 für die Werte x ∈ {−1,0,1}.
x <- c(-1, 0, 1) 
sin(x)^2 + cos(x)^2 == 1      # TRUE TRUE TRUE

# b. Erklären Sie, ob die Winkelfunktionen im Grad- oder Bogenmaß berechnet werden.
sin(1)
sin(pi/2)
plot(sin(seq(from = 0, to = 2 * pi, by = 0.1)))
# A: Die Winkelfunktion wird in Bogenmaß berechnet.

# c. Stellen Sie die Winkelfunktion 𝑠𝑖𝑛 𝑥 und 𝑐𝑜𝑠 𝑥 für 𝑥 ∈ [0,2π] in einem gemeinsamen
# Koordinatensystem grafisch dar und beschriften Sie es mit Hilfe des Befehls legend.
# Hinweis: Verwenden Sie den Befehl par(new=TRUE) nach dem ersten Plot-Befehl, damit beim
# nächsten Plot-Befehl das Fenster nicht gelöscht wird.
x <- seq(0, 2*pi, length.out = 101)
plot(x, sin(x), col = 2, type="l", ylab = "sin(x), cos(x)")
par(new=TRUE)
plot(x, cos(x), col = 4, type="l", ylab = "")
legend("top", legend = c("sin(x)", "cos(x)"), col = c(2, 4), lty = c(1, 1), pch = c(NA, NA))

# d. Berechnen Sie 𝑠𝑖𝑛 𝑥 für die Werte x ∈ {0, π/2, π,  3π/2,  2π} mit Hilfe einer einzigen
# Befehlszeile und interpretieren Sie das Ergebnis
result <- sin(c(0, pi/2, pi, 3*pi/2, 2*pi))

# e. Überprüfen Sie die Eigenschaft 𝑠𝑖𝑛(0) = 𝑠𝑖𝑛(2π).
sin(0) == sin(2*pi)        # => ohne Runden FALSE
sin(0) == round(sin(2*pi)) # => mit Runden TRUE


# 3. Erste statistische Datenanalysen
# a. Erzeugen Sie mit Hilfe des Befehls rnorm einen Vektor 𝑧, der 100 normalverteilte
# Zufallsvariablen mit dem Erwartungswert 10 und der Standardabweichung 1 enthält. 
z <- rnorm(100, mean = 10, sd = 1)

# b) Berechnen Sie das arithmetische Mittel, die Varianz und die Standardabweichung von 𝑧.
mean(z)
var(z)
sd(z)

# c) Erzeugen Sie ein Histogramm von 𝑧 und beschriften Sie sowohl das Histogramm als auch die
# horizontale Achse.
hist(z, main = "Histogramm von z", ylab = "Absolute Häufigkeit", xlab = "Wert")

# d) Erzeugen Sie einen Boxplot von 𝑧 und beschriften Sie es.
boxplot(z, main = "Boxplot von z", ylab = "Wert")

# e) Finden Sie einen Zusammenhang zwischen dem Ergebnis des Befehls summary(z) und des
#Boxplots in d).
summary(z)
# A: Ja, vom Boxplot lässt sich im Prinzip alles bis auf den arithmetischen Mittelwert ablesen.
