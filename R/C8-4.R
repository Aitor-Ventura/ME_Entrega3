#Cuestion 4

#Apartado a

consumo<-c(6.2,6.6,5.8,5.4,5.3,6.15,6.68,7,5.8,5.6,5.85,6.2,6.4,6.75,5.3,6.3)
media_consumo=mean(consumo)
desviacion_consumo=sd(consumo)
z1<-qnorm(0.995,0,1)
show(z1)
z2<-qnorm(0.005,0,1)
show(z2)
z_muestra=(media_consumo-5.3)/(desviacion_consumo/sqrt(16))
show(z_muestra)

PT<-seq(5,6.5,0.0001)
n1<-16
sigma<-0.5
sigma0<-sigma/sqrt(n1)
mu0<-5.3
media_muestra<-media_consumo
DP0<-dnorm(PT, mu0,sigma0)
plot(PT,DP0, type = "l", col="brown",
     ylab = "Densidad de Probabilidad", xlab
     = "Resistencia en Kgs.")
abline(v=mu0, col="green")
abline(v=media_muestra, col="blue")
alfa<-0.01

#Intervalo de decisión

Zona_critica1<-qnorm((1-alfa/2),mu0,sigma0)
Zona_critica2<-qnorm(alfa/2,mu0,sigma0)

Fliminf<-6.5
Flimsup<-Zona_critica2
xv<-PT[PT>=Fliminf & PT<=Flimsup]
yv<-DP0[PT>=Fliminf & PT<=Flimsup]
xv<-c(xv,Flimsup,Fliminf)
yv<-c(yv,DP0[1],DP0[1])
polygon(xv,yv,col = "gray")

Fliminf<-Zona_critica1
Flimsup<-8.4
xv<-PT[PT>=Fliminf & PT<=Flimsup]
yv<-DP0[PT>=Fliminf & PT<=Flimsup]
xv<-c(xv,Flimsup,Fliminf)
yv<-c(yv,DP0[1],DP0[1])
polygon(xv,yv,col = "gray")
