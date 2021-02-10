# Lab 7 ej 1
Aloe <- read.table("Aloe_Vera.txt", sep=",", dec=".", header=T)
#a)
aggregate(Aloe$Masa~Aloe$Variedad,Aloe,mean)
aggregate(Aloe$Masa_Seca~Aloe$Variedad,Aloe,mean)
#Las que dan más rendimiento son las barbadensis y las arborescens

#b)
Barbadensis <- subset(Aloe,subset=(Aloe$Variedad=="barbadensis"))
plot(Altura~Masa, data=Barbadensis,pch=19,col="blue",xlab="Masa en gr.",ylab="Altura",
     main="Aloe Vera Barbadensis")
grid()

#c)
modelo1 <- lm(Altura~Masa,data=Barbadensis)
abline(modelo1,col="green",lwd=2)

#d)
summary(modelo1)
confint(modelo1)
#En intervalo de confianza está entre 9,42 y 9,48

#e)
x0 <- 5.1
prediccion_masa <- predict(modelo1,list(Masa=x0))
points(x0,prediccion_masa,pch=16,col="black")
lines(c(x0,x0),c(0,prediccion_masa),col="red",lty=3,lwd=3)

inter_prediccion2 <- predict(modelo1,level=0.95,newdata=data.frame(Masa=x0),
                             interval="pred")
inter_prediccion2
lines(c(x0,x0),c(inter_prediccion2[2],inter_prediccion2[3]),col="green")
points(c(x0,x0),c(inter_prediccion2[2],inter_prediccion2[3]),col="green")
#el intervalo es [5.88,10.27]

inter_prediccion3 <- predict(modelo1,level=0.95,newdata=data.frame(Masa=x0),
                             interval="confidence")
inter_prediccion3
lines(c(x0,x0),c(inter_prediccion3[2],inter_prediccion3[3]),col="green")
points(c(x0,x0),c(inter_prediccion3[2],inter_prediccion3[3]),col="green")
#el intervalo de respuesta media es [7.63,8.52]

#f)
summary(modelo1)
#R2 = 0.953

#g)
summary(modelo1)
#el estadistico toma un valor de 1117 y un p-value 2.2e-16 lo que indica que hay una relacion
#muy fuerte, que es una recta

x_factor <- as.factor(Barbadensis$Masa)
n <- length(x_factor)
datos2 <- data.frame(x_factor,Barbadensis$Altura)
Y_M_F <- rep(0,n)
for(i in 1:n){
  Y_M_F[i] <- mean(Barbadensis$Altura[x_factor==Barbadensis$Masa[i]])
}
SCE_Puro <- sum((Barbadensis$Altura-Y_M_F)^2)
SCE_Puro

SC_Falta_Ajuste <- 61.8-SCE_Puro
k <- nlevels(x_factor)
S_2_Puro <- SCE_Puro/(n-k)
S_2_Puro
F_SC_Falta_Ajuste <- SC_Falta_Ajuste/(S_2_Puro*(k-2))
F_SC_Falta_Ajuste

1-pf(F_SC_Falta_Ajuste,1,k-2)
#40%,está bien ajustado, el modelo sigue bien el proceso