# 5th assignment sheet
# course: Explorative Data analysis Using R
# author: Felix Reichel K12008176

# 1.
# a.
Abschluss <- factor(c(2, 2, 1, 3, 1, 2), 
                    levels = c(1:3), labels = c("Bachelor", "Master", "PhD"))
Abschluss

# b.
class(Abschluss)
typeof(Abschluss)
attributes(Abschluss)
# Zusätzliche Attribute ggü. Vektor sind in unseren Bsp. $levels und $class
structure(Abschluss)

# c.
# 1.
AkademischerGrad <- c("BSc", "MSc", "Dr.")
# 2.
Index <- as.integer(Abschluss)
Index
# 3.
AkademischerGrad[Index]
# Es wird für jeden Index im Vektor AkademischerGrad der entsprechende AkademischerGrad zurückgegeben.
# Das entsprich bei unserem Faktor Abschluss dem Label für das jeweilige Level.

# d.
levels(Abschluss) <- AkademischerGrad
Abschluss

#e.
is.ordered(Abschluss)

AbschlussGeordnet <- ordered(Abschluss)
is.ordered(AbschlussGeordnet)

AbschlussGeordnet[3] < AbschlussGeordnet[6]
Abschluss[3] < Abschluss[6] # Funktioniert nicht, weil nicht 'meaningful' für diesen Faktor.



#2.
#a.
data <- c(-4, 0, 10, 23, 24, 26.5, 67, 99, 100, 120)
Wasser <- cut(x = data, labels = c("fest", "flüssig", "gasförmig"), breaks = c(-Inf, 0, 100, Inf))

class(Wasser)         # Geben Sie die Klasse
levels(Wasser)        # die Kategorien, 
nlevels(Wasser)       # Anzahl der Kategorien
is.ordered(Wasser)    # = nicht geordneter Factor

#b.
WasserOhne <- cut(x = data, breaks = c(-Inf, 0, 100, Inf))
WasserOhne 
# A: Die nächste Intervallgrenze wird immer noch in der aktuellen Kategorie eingeschlossen,
# also durch ein geschlossenes Intervall (right bracket)
# Konkret: 0 ist daher noch in (-Inf,0]

cut(x = data, breaks = c(-Inf, 0, 100, Inf), right = FALSE) # 0 ist in [0,100)
Wasser2 <-cut(x = data, labels = c("fest", "flüssig", "gasförmig"), breaks = c(-Inf, 0, 100, Inf), right = FALSE)
Wasser2

# c.

Wasser2 <- relevel(Wasser2, ref = "flüssig")
Wasser2 <- ordered(Wasser2)

Wasser2[5] <- "gasförmig"
Wasser2[9] <- "gasförmig"

Wasser2

Wasser2[1] <- "plasma"
# Nein ist nicht möglich, da es keine gültige Faktor-Kategorie ist.


# d.
Wasser2 <- Wasser2[2:10]
Wasser2 <- droplevels(Wasser2, "fest")
Wasser2

# e.
Wasser_int <- as.integer(Wasser)
Wasser_chr <- as.character(Wasser)
print(Wasser_int)
print(Wasser_chr)
summary(Wasser_int)
summary(Wasser_chr)
plot(Wasser_int)
# plot(Wasser_chr)


# BONUS

# a.
unclass(Abschluss)

#1b.
class(Abschluss)
typeof(Abschluss)
attributes(Abschluss)
structure(Abschluss)

# => Unclass gibt eine Kopie zurück ohne dem $class Attribute

#b.
sample_data <- sample(c(1:3), 100, replace = TRUE)
Abschluss <- factor(sample_data, levels = c(1:3), labels = c("Bachelor", "Master", "PhD"))
Abschluss

#c.

data <- sample(c("m", "f", "d"), 100, replace = TRUE)
Geschlecht <- factor(data, levels = c("m", "f", "d"), labels = c("männlich", "weiblich", "divers"))
Geschlecht

#d.
plot(Abschluss, Geschlecht)
plot(Geschlecht, Abschluss)

#e.

