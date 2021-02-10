# Lab 6 ej 2
library(knitr)
Tabla <- matrix(c(34,73,63,16,12,12),2,3,byrow=T)
colnames(Tabla) <- c("Fertilizante_A","Fertilizante_B","Fertilizante_C")
rownames(Tabla) <- c("Florecido","No_Florecido")
Tabla <- as.table(Tabla)
kable(Tabla)
Tabla_ampliada <- addmargins(Tabla)
kable(Tabla_ampliada)

ni <- Tabla_ampliada[3,]
nj <- Tabla_ampliada[,4]
N <- as.numeric(Tabla_ampliada[3,4])

pXY <- Tabla^2
suma <- 0
for (i in 1:3){
  for (j in 1:2){
    suma <- suma+as.numeric(pXY[1,1]/(ni[i]*nj[j]))
  }
}
CHI2 <- N*(suma-1)
CHI2
#grados de libertad y region critica
gl <- (nrow(Tabla)-1)*(ncol(Tabla)-1)
qchisq(0.95,gl)
#Como el valor 7.232 es mayor que el valor limite 5.991, el estadistico está dentro de la RC
#y se rechaza la hipótesis de independencia
resultado <- chisq.test(Tabla,correct=T)
resultado
#El estadistico toma el valor 7.232 y el p-value el valor limite 0.0269, inferior a 0.05, luego
#se rechaza la hipótesis de independencia