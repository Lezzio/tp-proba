VonNeumann <- function(n, p = 1, graine)
{
  x <- rep(graine, n * p + 1)
  for (i in 2:(n * p + 1))
  {
    numbers <- strsplit(format(x[i - 1]^2, scientific = FALSE), '')[[1]]
    while (length(numbers) > 4) {
      numbers <- numbers[2:(length(numbers) - 1)]
    }
    x[i] <- as.numeric(numbers) %*% (10^seq(length(numbers) - 1, 0, -1))
  }
  x <- matrix(x[2:(n * p + 1)], nrow = n, ncol = p)
  return(x)
}


MersenneTwister <- function(n, p = 1, graine)
{
  set.seed(graine, kind = 'Mersenne-Twister')
  x <- sample.int(2^32 - 1, n * p)
  x <- matrix(x, nrow = n, ncol = p)
  return(x)
}


binary <- function(x)
{
  if ((x < 2^31) & (x >= 0))
    return(as.integer(rev(intToBits(as.integer(x)))))
  else {
    if ((x < 2^32) & (x > 0))
      return(c(1, binary(x - 2^31)[2:32]))
    else {
      cat('Erreur dans binary : le nombre etudie n est pas un entier positif en 32 bits.\n')
      return(c())
    }
  }
}

RANDU <- function(seed, n)
{
  a <- 65539
  m <- 2^31
  x <- seed
  #b = 0
  x <- (x * a) %% m
  for (i in 2:n)
  {
    x <- c(x, (x[i - 1] * a) %% m)
  }
  #x <- x/m #Normalise x
  return(x)
}

StandardMinimal <- function(seed, n)
{
  a <- 16807
  m <- 2^31 - 1
  x <- seed
  #b = 0
  x <- (x * a) %% m
  for (i in 2:n)
  {
    x <- c(x, (x[i - 1] * a) %% m)
  }
  #x <- x/m #Normalise x
  return(x)
}

x <- StandardMinimal(34, 1000)
length(x)

LoiBinomiale <- function(n, p)
{
  u <- runif(1)
  k <- 0
  somme <- 0
  while (u > somme) {
    k <- k + 1
    pk <- choose(n, k) * (p^k) * ((1 - p)^(n - k)) #Taille d'intervalle Pk
    somme <- somme + pk
  }
  return(k) #Numéro de l'intervalle dans lequel on est
}

LoiGausienne <- function(n, p)
{
  u <- runif(1)
  k <- 0
  somme <- 0
  while (u > somme) {
    k <- k + 1
    pk <- dnorm(k, n * p, sqrt(n * p * (1 - p))) #Taille d'intervalle Pk
    somme <- somme + pk
  }
  return(k) #Numéro de l'intervalle dans lequel on est
}

SimInversion <- function() { #Difficile à appliquer généralement
  u <- runif(1)
  return(res = exp(sqrt(u) * log(2)) - 1)
}

#F-1, donc F est la fonction de répartition
SimRejet <- function() {
  u <- runif(1)
  y <- runif(1)
  while (u > (log(1 + y) / (1 + y))) { #Tant qu'on est pas sup à U...
    u <- runif(1) #... on re-simule u et y
    y <- runif(1)
  }
  return(y)
}