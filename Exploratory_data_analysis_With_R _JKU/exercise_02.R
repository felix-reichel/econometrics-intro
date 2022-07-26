# ---------------------------------------------------------
# Course: Explorative Data Analysis Using R
# Exercise: 2nd assignment sheet
# Author: Felix Reichel (k12008176)
# ---------------------------------------------------------
# 1.
# a.
end_idx <- length(letters)
start_idx <- end_idx - 10 +1

b <- c(letters[start_idx:end_idx])
B <- c(LETTERS[start_idx:end_idx])

bB_combined <- c(b,B)
bB_combined

# b.
x <- c(FALSE, 5.5, 0)
class(x) # = "numeric"
x_logical <- as.logical(x)
x_logical
# FALSE -> bleibt beim Type-Casting FALSE
# 5.5 -> ist positiv >= 1 und wird zu TRUE (1)
# 0 -> ist 0 und wird zu FALSE

# c.
a <- c("4", "2", "0", "3")

class(a) # ="character"
# a*a # Nein. "non-numeric argument to binary operator"

string <- "4+2-0+3"
new_string <- paste(string, a)
new_string
length(new_string)

# d.
n <- c(1:50)
is.integer(n)

for (N in n) {
  if (N %% 2 == 1){
    n[N] = (-1)*n[N]
  }
}

which.min(n)
which.max(n)

n <- sort(n)
n <- n[n >= -5 & n <= 5]
n

# e.
p1 <- c(alter = 33, geschlecht = "m", wohnort = "dorf1", beruf = "prof", familienstand = "m")
p2 <- c(alter = 27, geschlecht = "f", wohnort = "stadt1", beruf = "teacher", familienstand = "m")

cat("Person 1 ist", p1['alter'], "alt und wohnt in", p1['wohnort'], "\n")
cat("Person 2 ist", p2['alter'], "alt und wohnt in", p2['wohnort'], "\n")

alter_kombiniert <- c(rbind(p1,p2)[,'alter'])


# 2.
# a.
set.seed(1)
x <- rnorm(10)
x
set.seed(1)
y <- rnorm(10)
y
z <- rnorm(10)
z
# Durch setzten eines Seed's lassen sich Zufallszahlen deterministisch reproduzieren!

# b.

x_mean1 <- sum(x) / length(x)
x_mean1 == mean(x)
# TRUE. Das Ergebnis ist gleich!

# c.
x_std_dev_1 <- sqrt(var(x))
x_std_dev_1 == sd(x)
# TRUE. Das Ergebnis ist gleich!

# d.
var(x) == sd(x)^2
# TRUE. Das Ergebnis ist gleich!

# e.
runs <- 100
results <- c(1:runs)
c1 <- c(1:runs)

c2_end <- 1000+runs
c2 <- c(1000:c2_end)

for (idx in c1) {
  n1 <- c2[idx]
  n2 <- c1[idx]
  
  e <- as.integer(n1/n2)
  r <- (n1/n2) - e

  results[idx] <- as.logical(n1 == e*n2+r)
  # print(cat(n1, ":n compared with (e*n2)+r: ", (e*n2)+r, "is", results[idx]))
}
as.logical(results)


# Richtige Version: n1 == (e+r)*n2
for (idx in c1) {
  n1 <- c2[idx]
  n2 <- c1[idx]
  
  e <- as.integer(n1/n2)
  r <- (n1/n2) - e
  
  results[idx] <- as.logical(n1 == as.integer((e+r)*n2))
  # print(cat(n1, ":n compared with (e*n2)+2r: ", (e+r)*n2, "is", results[idx]))
}
as.logical(results)


# 3. -> neues R File

