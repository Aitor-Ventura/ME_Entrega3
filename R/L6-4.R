# Lab 6 ej 4
library(knitr)
Tabla <- matrix(c(103,12,18,35),2,2,byrow=T)
colnames(Tabla) <- c("Brazo_Fracturado_Jefe","Brazo_Normal_Jefe")
rownames(Tabla) <- c("Brazo_Fracturado_Interno","Brazo_Normal_Interno")
Tabla <- as.table(Tabla)
kable(Tabla)

#geometria
resultado <- mcnemar.test(Tabla,correct=T)
resultado

#independencia
resultado1 <- chisq.test(Tabla,correct=T)
resultado1

#Son independientes ambos