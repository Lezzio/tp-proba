Frequency <- function(x, nb)
  #x = Tableau des 1000 vals, nb = nombres de bits
{
  size <- length(x)
  s <- 0
  for (j in 1:size) {
    bin <- binary(x[j])
    for (i in 32:(32 - nb + 1)) {
      s <- s + 2 * bin[i] - 1
    }
  }
  sobs <- abs(s) / sqrt(size * nb)
  p <- 2 * (1 - pnorm(sobs))
  return(p)
}

Runs <- function(x, nombre) {
  Pv <- 0.0
  n <- 0
  chiffreUn <- 0

  #On préalloue le vecteur bits pour gagner en performance et ne pas avoir à utiliser c qui concaténe par copie
  bits <- vector(length = nombre * length(x))

  x <- rev(x) #Inverse X

  for (i in x) {
    bin <- rev(binary(i)) #Inverse chiffres binaires
    for (j in 1:nombre) {
      if (bin[j] == 1) {
        chiffreUn <- chiffreUn + 1
        bits[n + j] <- 1
      }
    }
    n <- n + nombre
  }

  pi <- chiffreUn / n
  tau <- 2 / sqrt(n)

  if (abs(pi - 1 / 2) < tau) {
    VNObs <- 1
    for (i in 1:(n - 1)) {
      if (bits[i] != bits[i + 1]) {
        VNObs <- VNObs + 1
      }
    }
    Pv <- 2 * (1 - pnorm(abs(VNObs - 2 * n * pi * (1 - pi)) / (2 * sqrt(n) * pi * (1 - pi))))
  }

  return(Pv)
}

#Vérification run
a <- Runs(619, 10)
a
a <- Runs(c(19, 11), 5)
a