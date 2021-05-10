evolutionFile <- function(arrivee, depart)
{
  a <- 1
  d <- 1

  N <- c(0)
  T <- c(0)

  repeat {
    if (arrivee[a] < depart[d]) {
      N <- c(N, tail(N, 1) + 1)
      T <- c(T, arrivee[a])
      a <- a + 1
    } else if (arrivee[a] > depart[d]) {
      N <- c(N, tail(N, 1) - 1)
      T <- c(T, depart[d])
      d <- d + 1
    } else {
      a <- a + 1
      d <- d + 1
    }

    if (a == length(arrivee) + 1 || d == length(depart) + 1) {
      break
    }
  }

  if (a == length(arrivee) + 1) {
    repeat {
      if (d == length(depart)) {
        break
      }
      N <- c(N, tail(N, 1) - 1)
      T <- c(T, depart[d])
      d <- d + 1
    }
  }

  if (d == length(depart) + 1) {

    repeat {
      if (a == length(arrivee)) {
        break
      }

      N <- c(N, tail(N, 1) + 1)
      T <- c(T, arrivee[a])
      a <- a + 1
    }
  }

  return(
    list(T = T, N = N)
  )
}


evolutionFile <- function(arrivee, depart)
{
  a <- 1
  d <- 1

  N <- c(0)
  T <- c(0)

  repeat {
    if (arrivee[a] < depart[d]) {
      N <- c(N, tail(N, 1) + 1)
      T <- c(T, arrivee[a])
      a <- a + 1
    } else if (arrivee[a] > depart[d]) {
      N <- c(N, tail(N, 1) - 1)
      T <- c(T, depart[d])
      d <- d + 1
    } else {
      a <- a + 1
      d <- d + 1
    }

    if (a == length(arrivee) + 1 || d == length(depart) + 1) {
      break
    }
  }

  if (a == length(arrivee) + 1) {
    repeat {
      if (d == length(depart)) {
        break
      }
      N <- c(N, tail(N, 1) - 1)
      T <- c(T, depart[d])
      d <- d + 1
    }
  }

  if (d == length(depart) + 1) {

    repeat {
      if (a == length(arrivee)) {
        break
      }

      N <- c(N, tail(N, 1) + 1)
      T <- c(T, arrivee[a])
      a <- a + 1
    }
  }

  return(
    list(T = T, N = N)
  )
}

StatsMoyens <- function(arrivee, depart, lambda, mu)
{
  # E(N)
  alpha <- lambda / mu
  EN <- alpha / (1 - alpha)

  if (alpha >= 1) return(NULL)

  # E(W)
  # estime
  W <- 0

  for (i in 1:length(depart))
  {
    W <- W + depart[i] - arrivee[i]
  }
  EW = W / length(depart)


  return(
    list(
      EW = EW, EN = EN
    )
  )
}