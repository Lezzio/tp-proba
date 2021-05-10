source("files.R")
source("filesource.R")
#A = 6 clients par heure = 1 client / 10 m :   10m = 1 / lambda => lambda = 0.1
#D = 11 clients par heure = 5,45m   5.45 = 1 / mu => mu = 0,183
a <- FileMM1(0.1, 0.183, 1000 * 60)
a

c <- evoFileFinal(a[[1]], a[[2]])
plot(c[[1]], c[[2]], type = "s", main = "Évolution d'une file", ylab = "N (nombre de requêtes)", xlab = "Temps (m)")
cat("\nSELF Temps = ", c[[1]])
cat("\nSELF N = ", c[[2]])

e <- esperance(a[[1]], a[[2]], b[[1]], b[[2]], 0.1, 0.183)
e