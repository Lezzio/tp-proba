source("generateurs.R")
source("utile.R")
library(randtoolbox)
library(microbenchmark)
par(mfrow = c(1, 2))
binom <- LoiBinomiale(100, 0.2)
for (i in 1:1000) {
  binom <- c(binom, LoiBinomiale(100, 0.2))
}
plot(table(binom), main = 'Binomial', xlab = 'Nb de succ�s', ylab = 'Fr�quence')

gaussi <- LoiGausienne(100, 0.2)
for (i in 1:1000) {
  gaussi <- c(gaussi, LoiGausienne(100, 0.2))
}
plot(table(gaussi), main = 'Gausienne', xlab = 'Nb de succ�s', ylab = 'Fr�quence')

microbenchmark(100, SimInversion(), SimRejet())