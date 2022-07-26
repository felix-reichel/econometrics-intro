# 4th assignment sheet
# course: Explorative Data analysis Using R
# author: Felix Reichel K12008176

# 1.
# a.
datenbank <- list(
  "Franz" = list(location = "Wien", kids = NULL, job = "Angestellter"),
  "Bettina" = list(location = "Linz", kids = c("Petra", "Paul")))

structure(datenbank["Bettina"])
is.null(datenbank["Bettina"]$location)

datenbank["Bettina"]$Bettina$location

# Nur datenbank["Bettina"]$location existiert lt.
# dem Befehl structure(datenbank["Bettina"]) nicht.

# b.
length(datenbank["Bettina"]$Bettina$kids)

# c.
datenbank["Franz"]$Franz$location <- "Graz"
datenbank["Bettina"]$Bettina$kids <-
  c(datenbank["Bettina"]$Bettina$kids, "Magdalena")
structure(datenbank)

# d.
datenbank <- c(datenbank,list(Regina = list(location = "Salzburg", kids = c("Leonie"), job = "Lehrerin")))
structure(datenbank)

#e?
for (idx in 1:length(datenbank)) {
  print(datenbank[idx])
}


# 2.
# a.
my.dat1 <- data.frame(
  Geschlecht = c("m", "f", "f"), # character
  Alter = c(27, 42, 17), # numeric
  Gewicht = c(72.1, 84.9, 54.4), # numeric
  Groesse = c(1.76, 1.64, 1.72) # numeric
)
my.dat2 <- data.frame(
  Geschlecht = c("m", "m"), # character
  Alter = c(89, 32), # numeric
  Gewicht = c(62.1, 72.8), # numeric
  Groesse = c(1.79, 1.61) # numeric
)
# b.
my.dat <- rbind(my.dat1, my.dat2)
rownames(my.dat) <- c("Andi", "Sarah", "Jenny", "David", "Klaus")
my.dat

# c.
my.dat$BMI <- round(my.dat$Gewicht / my.dat$Groesse^2, 2)
my.dat

# d.
my.dat$Gewichtsklasse <- my.dat$Gewicht > 70
my.dat

# e.
my.dat[order(rownames(my.dat)), ]
my.dat[order(my.dat$BMI, decreasing = TRUE), ]
my.dat[order(rownames(my.dat), my.dat$BMI, decreasing = c(FALSE,TRUE)), ]


# 3.
# a.
force(data("ToothGrowth"))

class(ToothGrowth)
typeof(ToothGrowth)
str(ToothGrowth)
length(ToothGrowth)
dim(ToothGrowth)
nrow(ToothGrowth)
ncol(ToothGrowth)

is.data.frame(ToothGrowth)
is.list(ToothGrowth)
is.matrix(ToothGrowth)

attributes(ToothGrowth)
row.names(ToothGrowth)
colnames(ToothGrowth)

ToothGrowth

mat <- matrix(ToothGrowth)
mat
mat[1]
mat[2]
mat[3]

# Ja, rownames und colnames gehen evtl. verloren. Könnte man zusätzlich in 2 Listen ablegen.
# Das data.frame besitzt keine heterogene Daten


# b.
subset(ToothGrowth, select = c(1, 3))

medians <- apply(subset(ToothGrowth, select = c(1, 3)), 2, median, na.rm=T)
medians

cntr <- median(ToothGrowth[,1])
cntr

# c.
tapply(ToothGrowth$len, ToothGrowth$supp, mean)


# d.
ToothGrowth$len >= 20

nrow(ToothGrowth[ToothGrowth$len >= 20 & ToothGrowth$supp == "OJ",]) >
  nrow(ToothGrowth[ToothGrowth$len >= 20 & ToothGrowth$supp == "VC",])


# e.
ToothGrowth_desc <- ToothGrowth[order(ToothGrowth$len, decreasing = TRUE), ] 
myToothGrowth <- tail(ToothGrowth_desc, 50)
