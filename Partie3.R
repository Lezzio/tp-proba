source("files.R")



#En moyenne 10 clients arrivent par heure et 11 sortent par heure
a <- FileMM1(10 / 60, 0.183, 12 * 60) #Lambda ~ 0.1667

#En moyenne 11 clients arrivent par heure et 11 sortent par heure
a <- FileMM1(11 / 60, 0.183, 12 * 60) #Lambda ~ 0.1833

#En moyenne 15 clients arrivent par heure et 11 sortent par heure
a <- FileMM1(0.25, 0.183, 12 * 60)

#En moyenne 6 clients arrivent par heure et 11 sortent par heure
a <- FileMM1(0.1, 0.183, 1000)

b <- evoFileFinal(a[[1]], a[[2]])
plot(b[[1]], b[[2]], type = "s", main = "Évolution d'une file", ylab = "N (nombre de requêtes)", xlab = "Temps (m)")

#e <- esperance(a[[1]], a[[2]], b[[1]], b[[2]])
#e