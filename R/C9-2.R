#Cuestion 2

library(knitr)
library(vcd)
library (PerformanceAnalytics)

#Apartado a

estudiante <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)
algebra <- c(5.7,8.6,3.6,1.5,8.8,5.9,4.9,8.6,7.6,5.0,7.7, 2.6, 8.6, 7.5, 6.2, 9.9,7.1, 5.6,6.2, 7.6,6.5,6.7,4.5, 4.8,  6.9,  8.9, 2.6,5.5, 7.0)
programacion <- c(5.0,7.0,5.2,1.3, 7.2, 6.6, 3.1,8.6, 6.0,6.1,8.0,5.0,9.2,7.3,4.2, 6.6,9.1, 7.6, 4.0,5.1,8.0,8.1, 9.1, 4.5, 3.2,7.6,7.1,4.6,6.0, 5.8)

tabla <- matrix(c(estudiante,algebra,programacion),30,3, byrow = FALSE)
colnames(tabla) <- c("Estudiante", "Algebra" , "Programacion")
datos <- as.table(addmargins(tabla))
kable(datos)

ni <- datos[30,]; ni

nj <- datos[,4]; nj

N<- as.numeric(datos[30,4]); N

pxy <- tabla^2
suma <- 0
for(i in 1:30) { 
  for(j in 1:4) { 
    suma <- suma+as.numeric(pxy[j,i]/(ni[i]*nj[j]))
  }
}
suma

chi2 <- N * (suma-1); chi2

g1 <- (nrow(tabla)-1)*(ncol(tabla)-1); g1

qchisq(0.95,g1)

resultado1 <- chisq.test(tabla, correct = T); resultado1

mean (algebra)
mean (programacion)