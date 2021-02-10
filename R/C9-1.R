#Cuestionario Lectura 9

library(knitr)
library(vcd)
library (PerformanceAnalytics)

#Cuestion 1

#Apartado a:

tabla <- matrix (c(35,47,31,55), 2,2, byrow = TRUE)
colnames(tabla) <- c("hombres", "mujeres")
rownames(tabla) <- c ("Interesados", "No interesados")
datos <- as.table(tabla)
kable (tabla)
summary(tabla)

Teoría.

#Apartado b:

#Pruebas que ya incorpora R.

res1 <- mcnemar.test(tabla, correct = TRUE); 
res1

res2 <- chisq.test(tabla,correct = TRUE); 
res2


