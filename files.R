### Files
FileMM1 <- function(lambda, mu, D) {

  totalArriveeTime <- 0
  arrivee <- vector()

  repeat {
    #On génère la valeur suivant la loi exponentielle de paramètre lambda
    T <- rexp(1, lambda)

    #Incrémente total
    totalArriveeTime <- totalArriveeTime + T

    #On vérifie si on ne dépasse pas le temps d'observation de la chaîne D
    if(totalArriveeTime >= D) {
      break;
    }

    #On ajoute l'élément à la liste des arrivées
    arrivee <- c(arrivee, totalArriveeTime)
  }

  totalDepartTime <- arrivee[1]
  depart <- vector()

  for(arrival in arrivee) {
    #On génère la valeur suivant la loi exponentielle de paramètre mu
    stayTime <- rexp(1, mu)

    #Si l'arrivée courante est plus récente que totalDepartTime alors on part au moins de l'arrivée courante pour la date de départ
    if(totalDepartTime < arrival) {
      totalDepartTime <- arrival
    }

    #Incrémente total
    totalDepartTime <- totalDepartTime + stayTime

    if(totalDepartTime > D) {
      break
    }

    #On ajoute l'élément à la liste des départs
    depart <- c(depart, totalDepartTime)
  }

  return (list("arrivee" = arrivee, "depart" = depart))
}

#Algorithme de complexité n + m (n = taille arrivee et m = taille depart)
fileEvolution <- function(arrivee, depart) {

  N <- 0
  T <- 0

  aIndex <- 1
  dIndex <- 1
  currentCount <- 0

  aOver <- FALSE
  dOver <- FALSE

  #On itère les deux listes en même temps
  repeat {

    #On gère les index
    if(aIndex > length(arrivee)) {
      aIndex <- length(arrivee)
      aOver <- TRUE
    }
    if(dIndex > length(depart)) {
      dIndex <- length(depart)
      dOver <- TRUE
    }
    a <- arrivee[aIndex]
    d <- depart[dIndex]

    if(aOver && !dOver) { #Arrivées terminées => remplir avec départs restants
      #INCRÉMENTE DÉPART
      currentCount <- currentCount - 1
      N <- c(N, currentCount)
      T <- c(T, d)

      dIndex <- dIndex + 1
    } else if(dOver && !aOver) { #Départs terminées => remplir avec arrivées restantes
      #INCRÉMENTE ARRIVÉE
      currentCount <- currentCount + 1
      N <- c(N, currentCount)
      T <- c(T, a)

      aIndex <- aIndex + 1
    } else if(a == d) { #Cas très improbable d'avoir une arrivée et départ en même temps (à prendre en compte)
      #INCRÉMENTE LES DEUX
      N <- c(N, currentCount)
      T <- c(T, a)

      aIndex <- aIndex + 1
      dIndex <- dIndex + 1
    } else if(a < d && aIndex && !aOver) { #Arrivée avant départ pour ces index
      #INCRÉMENTE ARRIVÉE
      currentCount <- currentCount + 1
      N <- c(N, currentCount)
      T <- c(T, a)

      aIndex <- aIndex + 1
    } else if(d <= a && !dOver){ #Départ avant arrivée pour ces index
      #INCRÉMENTE DÉPART
      currentCount <- currentCount - 1
      N <- c(N, currentCount)
      T <- c(T, d)

      dIndex <- dIndex + 1
    } else { #Finalement break quand tous les indexes ont atteint leur max
      break
    }

  }

  return(list("T" = T, "N" = N))
}

esperanceFile <- function(arrivee, depart, T, N) {
  #Calcul de l'espérance E[N]
  sum <- 0
  for(i in 1:(length(N) - 1)) {
    sum <- sum + N[i] * (T[i+1] - T[i])
  }
  esperanceN <- sum / T[length(T)] #Calcul expérimental du E[N] qui devrait faire alpha / (1 - alpha)

  #Calcul de l'espérance E[W]
  W <- 0
  for(i in seq_along(depart)) {
    W <- W + (depart[i] - arrivee[i])
  }
  esperanceW <- W / length(depart)

  return (list("esperanceN" = esperanceN, "esperanceW" = esperanceW))
}