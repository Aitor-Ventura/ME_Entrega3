# Lab 6 ej 3
library(knitr)
Tabla <- matrix(c(75,73,15,32),2,2,byrow=T)
colnames(Tabla) <- c("Estudiantes II","Otros titulos")
rownames(Tabla) <- c("Mas de 2 horas","Menos de 2 horas")
Tabla <- as.table(Tabla)
kable(Tabla)
Tabla_ampliada <- addmargins(Tabla)
kable(Tabla_ampliada)

ni <- Tabla_ampliada[3,]
nj <- Tabla_ampliada[,3]
N <- as.numeric(Tabla_ampliada[3,3])

Tabla_Esperada <- Tabla
suma <- 0
for (i in 1:2){
  for(j in 1:2){
    Tabla_Esperada[i,j] <- (ni[j]*nj[i])/N
    suma <- suma+((abs(Tabla[i,j]-Tabla_Esperada[i,j])-0.5)^2)/Tabla_Esperada[i,j]
  }
}
kable(Tabla_Esperada)
CHI2 <- suma
CHI2
#grados de libertad y region critica
gl <- (nrow(Tabla)-1)*(ncol(Tabla)-1)
qchisq(0.95,gl)
#Como el valor 4.325 es mayor que el valor limite 3.841, el estadistico está dentro de la RC y
#se rechaza la hipótesis de independencia
resultado <- chisq.test(Tabla,correct=T)
resultado
#El estadistico toma el valor 4.325 y el p-value valor limite 0.03755 es inferior a 0.05,
#luego se rechaza la hipotesis de independencia