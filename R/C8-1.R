#Cuestiones Lectura 8

#Apartado a

a=0.05
media_0=10
datos <- c(3.58, 10.03, 4.77, 9.71, 10.4, 14.66, 8.45, 5.4, 9.75, 10.1)
U<-mean(datos)
S<-sd(datos)
n=10
t<-(U-media_0)/(S/sqrt(n))
Zona_critica2<-qt(1-a,n-1)*(S/sqrt(n-1))+media_0
Zona_critica2

echo=FALSE
PT<-seq(4,16,0.001)
TPT<-(PT-media_0)/(S/sqrt(n-1))
DP0<-dt(TPT, n-1)
plot(PT,DP0, type = "l", col="black", ylab =
       "Densidad de Probabilidad", xlab =
       "Estadístico t (valores posibles)")
abline(v=media_0, col="green")
abline(v=U, col="blue")

Fliminf<-Zona_critica2
Flimsup<-16
xv<-PT[PT>=Fliminf & PT<=Flimsup]
yv<-DP0[PT>=Fliminf & PT<=Flimsup]
xv<-c(xv,Flimsup,Fliminf)
yv<-c(yv,DP0[1],DP0[1])
polygon(xv,yv,col = "red")

Zona_critica1<-qt(a,n-1)*(S/sqrt(n-1))+media_0
Fliminf2<-4
Flimsup2<-Zona_critica1
xv2<-PT[PT>=Fliminf2 & PT<=Flimsup2]
yv2<-DP0[PT>=Fliminf2 & PT<=Flimsup2]
xv2<-c(xv2,Flimsup2,Fliminf2)
yv2<-c(yv2,DP0[1],DP0[1])
polygon(xv2,yv2,col = "red")

#Apartado b

echo=FALSE
PT<-seq(4,16,0.001)
TPT<-(PT-media_0)/(S/sqrt(n-1))
DP0<-dt(TPT, n-1)
plot(PT,DP0, type = "l", col="black", ylab =
       "Densidad de Probabilidad", xlab =
       "Estadístico t (valores posibles)")
abline(v=media_0, col="green")
abline(v=U, col="blue")

Fliminf<-4
Flimsup<-Zona_critica1
xv<-PT[PT>=Fliminf & PT<=Flimsup]
yv<-DP0[PT>=Fliminf & PT<=Flimsup]
xv<-c(xv,Flimsup,Fliminf)
yv<-c(yv,DP0[1],DP0[1])
polygon(xv,yv,col = "red")

#Apartado c

Teoría
