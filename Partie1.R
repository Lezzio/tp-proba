source("generateurs.R")
source("utile.R")
library(randtoolbox)
#Q2.1
par(mfrow = c(2, 2)) #(2,2)
hist(RANDU(34, 1000), xlab = 'val', ylab = 'Occurence', main = 'Randu', breaks = 20)
hist(MersenneTwister(1000, 1, 34), xlab = 'val', ylab = 'Occurence', main = 'Mersenne-Twister', breaks = 20)
hist(StandardMinimal(34, 1000), xlab = 'val', ylab = 'Occurence', main = 'StandardMinimal', breaks = 20)
hist(VonNeumann(1000, 1, 34), xlab = 'val', ylab = 'Occurence', main = 'Von Neumann', breaks = 20)

#Q2.2 -> Faire aussi pour les autres, commentez
par(mfrow = c(2, 2))
u <- VonNeumann(1000, 1, 34)
plot(u[1:(n - 1)], u[2:n])
u <- MersenneTwister(1000, 1, 34)
plot(u[1:(n - 1)], u[2:n])
u <- StandardMinimal(34, 1000)
plot(u[1:(n - 1)], u[2:n])
u <- RANDU(34, 1000)
plot(u[1:(n - 1)], u[2:n])

#Q3 : Pval
par(mfrow = c(2, 2))
graines = sample.int(9999, 100)
x <- Frequency(MersenneTwister(1000, 1, graines[1]), 32)
for (i in 2:100) {
  x <- c(x, Frequency(MersenneTwister(1000, 1, graines[i]), 32))
}
hist(x)
print(mean(x)) #Pval Moyen
verifTest <- length(which(x > 0.01))
print(verifTest / 100) #Nb de Pval passant le test de vérif

x <- Frequency(VonNeumann(1000, 1, graines[1]), 14)
for (i in 2:100) {
  x <- c(x, Frequency(VonNeumann(1000, 1, graines[i]), 14))
}
hist(x)
print(mean(x)) #Pval Moyen
verifTest <- length(which(x > 0.01))
print(verifTest / 100) #Nb de Pval passant le test de vérif

x <- Frequency(StandardMinimal(graines[1], 1000), 31)
for (i in 2:100) {
  x <- c(x, Frequency(StandardMinimal(graines[i], 1000), 31))
}
hist(x)
print(mean(x)) #Pval Moyen
verifTest <- length(which(x > 0.01))
print(verifTest / 100) #Nb de Pval passant le test de vérif

x <- Frequency(RANDU(graines[1], 1000), 32)
for (i in 2:100) {
  x <- c(x, Frequency(RANDU(graines[i], 1000), 32))
}
hist(x)
print(mean(x)) #Pval Moyen
verifTest <- length(which(x > 0.01))
print(verifTest / 100) #Nb de Pval passant le test de vérif
#VN : 14bits, Mersenne : 32, RANDU + Standard : 31

#Q4
x <- Runs(RANDU(graines[1], 1000), 31)
for (i in 2:100) {
  x <- c(x, Runs(RANDU(graines[i], 1000), 31))
}
hist(x)
print(mean(x))
verifTest <- length(which(x > 0.01))
print(verifTest / 100)

x <- Runs(StandardMinimal(graines[1], 1000), 31)
for (i in 2:100) {
  x <- c(x, Runs(StandardMinimal(graines[i], 1000), 31))
}
hist(x)
print(mean(x))
verifTest <- length(which(x > 0.01))
print(verifTest / 100)

x <- Runs(VonNeumann(1000, 1, graines[1]), 14)
for (i in 2:100) {
  x <- c(x, Runs(VonNeumann(1000, 1, graines[i]), 14))
}
hist(x)
print(mean(x)) #Pval Moyen
verifTest <- length(which(x > 0.01))
print(verifTest / 100) #Nb de Pval passant le test de vérif

x <- Runs(MersenneTwister(1000, 1, graines[1]), 14)
for (i in 2:100) {
  x <- c(x, Runs(MersenneTwister(1000, 1, graines[i]), 14))
}
hist(x)
print(mean(x)) #Pval Moyen
verifTest <- length(which(x > 0.01))
print(verifTest / 100) #Nb de Pval passant le test de vérif

#Q5
par(mfrow = c(1, 2))
x <- order.test(as.vector(MersenneTwister(1000, 1, graines[1])), 4, FALSE)$p.value
for (i in 2:100) {
  x <- c(x, order.test(as.vector(MersenneTwister(1000, 1, graines[i])), 4, FALSE)$p.value)
}
hist(x, main = "Pval Mersenne-Twister", xlab = 'Valeur', ylab = 'Occurence', breaks = 20)
print(mean(x)) #Pval Moyen
verifTest <- length(which(x > 0.01))
print(verifTest / 100) #Nb de Pval passant le test de vérif

x <- order.test(as.vector(VonNeumann(1000, 1, graines[1])), 4, FALSE)$p.value
for (i in 2:100) {
  x <- c(x, order.test(as.vector(VonNeumann(1000, 1, graines[i])), 4, FALSE)$p.value)
}
hist(x, main = "Pval Von Neumann", xlab = 'Valeur', ylab = 'Occurence', breaks = 20)
print(mean(x)) #Pval Moyen
verifTest <- length(which(x > 0.01))
print(verifTest / 100) #Nb de Pval passant le test de vérif

x <- order.test(RANDU(graines[1], 1000), 4, FALSE)$p.value
for (i in 2:100) {
  x <- c(x, order.test(RANDU(graines[1], 1000), 4, FALSE)$p.value)
}
hist(x, main = "Pval RANDU", xlab = 'Valeur', ylab = 'Occurence', breaks = 20)
print(mean(x)) #Pval Moyen
verifTest <- length(which(x > 0.01))
print(verifTest / 100) #Nb de Pval passant le test de v�rif

x <- order.test(StandardMinimal(graines[1], 1000), 4, FALSE)$p.value
for (i in 2:100) {
  x <- c(x, order.test(StandardMinimal(graines[1], 1000), 4, FALSE)$p.value)
}
hist(x, main = "Pval Standard Minimal", xlab = 'Valeur', ylab = 'Occurence', breaks = 20)
print(mean(x)) #Pval Moyen
verifTest <- length(which(x > 0.01))
print(verifTest / 100) #Nb de Pval passant le test de v�rif
#-----------------Partie 3 -----------------------
#Q6
FileMM1(1, 1, 100)