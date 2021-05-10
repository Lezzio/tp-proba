source("files.R")

#En moyenne 6 clients arrivent par heure et 11 sortent par heure
a <- FileMM1(0.1, 0.183, 12 * 60)

#En moyenne 10 clients arrivent par heure et 11 sortent par heure
a <- FileMM1(10 / 60, 0.183, 12 * 60) #Lambda ~ 0.1667

#En moyenne 11 clients arrivent par heure et 11 sortent par heure
a <- FileMM1(11 / 60, 0.183, 12 * 60) #Lambda ~ 0.1833

#En moyenne 15 clients arrivent par heure et 11 sortent par heure
a <- FileMM1(0.25, 0.183, 100)

evo <- fileEvolution(a[[1]], a[[2]])
plot(b[[1]], b[[2]], type = "s", main = "Évolution d'une file", ylab = "N (nombre de requêtes)", xlab = "Temps (m)")

e <- esperanceFile(a[[1]], a[[2]], evo[[1]], evo[[2]])
#e


filemm1 <- FileMM1(0.1, 0.183, 12 * 60)
evolution <- fileEvolution(filemm1First[[1]], filemm1First[[2]])
e1 <- esperanceFile(filemm1[[1]], filemm1[[2]], evolution[[1]], evolution[[2]])
filemm1 <- FileMM1(5, 7, 12 * 60)
evolution <- fileEvolution(filemm1First[[1]], filemm1First[[2]])
e2 <- esperanceFile(filemm1[[1]], filemm1[[2]], evolution[[1]], evolution[[2]])

e1
e2