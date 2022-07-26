# ---------------------------------------------------------
# Course: Explorative Data Analysis Using R
# Exercise: 3rd assignment sheet
# Author: Felix Reichel (k12008176)
# ---------------------------------------------------------
# 1.)
# a
x <- c(1:50)
x[x %% 2 == 0] <- NA
x

# b
var(x, na.rm = TRUE)
summary(x)
var(x)

# c.
any(is.na(x))
length(x[is.na(x)]) > 0
length(x[is.na(x)])
max(x, na.rm=TRUE) < 50

# d.
x2 <- x[!is.na(x)]
var(x2)

# e.
plot(x)
barplot(x)
boxplot(x)
pie(x2)
hist(x)


# 2.)

# a.
mat1 <- matrix(nrow = 4, ncol = 4, data = 7)
mat1
class(mat1)
typeof(mat1)
length(mat1)
dim(mat1)

mat2 <-matrix(nrow = 1, ncol = 10, data = -99)
mat2
class(mat2)
typeof(mat2)
length(mat2)
dim(mat2)

# b.

data_A <- rep(c(-1, 0, 1), 5) # Für Matrix A
data_B <- rep(c(-3, -2, -1, 0, 1, 2, 3), each = 3) # Für Matrix B
data_C <- rep(c(1, 0, 0, 0, 0), length = 16) # Für Matrix C

A <- matrix(data = data_A[1:3], ncol = 5, nrow = 3)
A

B <- matrix(data = data_B[c(1:7)*3], nrow = 7, ncol = 3)
B  

C <- matrix(data = data_C, byrow = TRUE, nrow = 4, ncol = 4) 
C

# c.
n=m=4
v1 <- c(1:n)
v2 <- c(1:m)
res <- rbind(matrix(data = v1, nrow = 1), v2)
res
# Beide Vektoren haben die selbe Anzahl an Elementen. Keine Probleme

n=8
m=4
v1 <- c(1:n)
v2 <- c(1:m)
res <- rbind(matrix(data = v1, nrow = 1), v2)
res
# Vektor 1 hat mehr Elemente als Vektor 2. Vektor 2 wird wiederholt damit die Matrix aufgefüllt wird. n ist ein Vielfaches von m.

n=4
m=8
v1 <- c(1:n)
v2 <- c(1:m)
res <- rbind(matrix(data = v1, nrow = 1), v2)
res

# Der zweite Vektor hat mehr Elemente als der erste und kann daher nicht in die Matrix eingefügt werden..
# => number of columns of result is not a multiple of vector length (arg 2)

n=4
m=1
v1 <- c(1:n)
v2 <- c(1:m)
res <- rbind(matrix(data = v1, nrow = 1), v2)
res
# Vektor 1 hat mehr Elemente als Vektor 2. Vektor 2 wird wiederholt damit die Matrix aufgefüllt wird. n ist ein Vielfaches von m.

n=5
m=3
v1 <- c(1:n)
v2 <- c(1:m)
res <- rbind(matrix(data = v1, nrow = 1), v2)
res
# Vektor 2 kann nicht wiederholt eingefügt werden, weil die Anzahl der Elemente kein Vielfaches zum ersten Vektor ist
# number of columns of result is not a multiple of vector length (arg 2)

n=9
m=10
v1 <- c(1:n)
v2 <- c(1:m)
res <- rbind(matrix(data = v1, nrow = 1), v2)
res
# Vektor 2 kann nicht wiederholt eingefügt werden, weil die Anzahl der Elemente kein Vielfaches zum ersten Vektor ist
# number of columns of result is not a multiple of vector length (arg 2)

# d.
set.seed(9999)
mat <- matrix(ncol = 5, data= round(rnorm(100),1))
mat[1:6,4:5]

# e.
summary(mat)
# Die Summary Statistics werden pro Matrix-Spalte ausgegeben.
# Die nummerischen Ergebnisse lassen sich so zusammenfassen, dass jede Spalte 20 zufallsgenerierte normalverteilte gerundete Zahlen enthält. 

min(mat)
max(mat)
mean(mat)
sd(mat)
sum(mat)

# 3.)

# a.

# 310
# 10

# 190
# m[2,3]

# 110
# mat[4,3]

mat <- matrix(data = c(270,160,60,220, 100,10 ,190 ,10,330, 310 ,150, 350,340, 80, 110, 100,260, 50, 290, 0), nrow = 5, byrow = TRUE)
mat
mat[2,3]
mat[4,3]

#b.
 
mat1 <- matrix(c(B[3,], B[5,]), nrow = 2, byrow = TRUE)
mat1
mat2 <-matrix(B[1,], nrow = 1)
mat2 <- rbind(mat2, B[3:5,])
mat2

# c.
Land <- c("Norwegen", "Deutschland", "Kanada", "Vereinigte Staaten", "Niederlande", "Schweden", "S¸dkorea", "Schweiz", "Frankreich", "÷sterreich")
Medaillen <- matrix(c(14, 14, 11, 9, 8, 7, 5, 5, 5, 5, 14, 10, 8, 8, 6, 6, 8, 6, 4, 3, 11, 7, 10, 6, 6, 1, 4, 4, 6, 6),
                    ncol = 3,
                    dimnames = list(Land, c("Gold", "Silber", "Bronze")))
Medaillen

is.matrix(Medaillen)
dim(Medaillen)

rnames <- dimnames(Medaillen)[[1]] # rownames(Medaillen) also possible
is.character(rnames)
length(rnames)

cols <- colnames(Medaillen)
is.character(cols)
length(cols)

length(rnames) == dim(Medaillen)[1] && length(cols) == dim(Medaillen)[2]

rnames <- c("Norway", "Germany", "Canada", "USA", "Netherlands", "Sweden", "South Korea", "Switzerland", "France", "Austria")

Medaillen <- matrix(data=(Medaillen[,]),ncol=3, dimnames = list(rnames, c("Gold", "Silber", "Bronze")))
Medaillen


#d.
idx <- which(Medaillen[,1] > 10)
Medaillen[idx,]

#vs
Medaillen[Medaillen[,1] > 10,]

#e.

Land <- c("Norwegen", "Deutschland", "Kanada", "Vereinigte Staaten", "Niederlande", "Schweden", "S¸dkorea", "Schweiz", "Frankreich", "÷sterreich")
Medaillen <- matrix(c(14, 14, 11, 9, 8, 7, 5, 5, 5, 5, 14, 10, 8, 8, 6, 6, 8, 6, 4, 3, 11, 7, 10, 6, 6, 1, 4, 4, 6, 6),
                    ncol = 3,
                    dimnames = list(Land, c("Gold", "Silber", "Bronze")))

Medaillen[ order(rownames(Medaillen)), ]
Medaillen[ order(Medaillen[,1], Medaillen[,2], Medaillen[,3], decreasing = TRUE), ]


# Bonus
