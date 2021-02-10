# Lab 6 ej 5
library(knitr)
library(ggplot2)
P <- c(15,12,10,8,9,6,10)
A <- c(7,8,9,8,7,10,9,8,7,10)
B <- c(8,9,8,6,7,8,9,8,7,6)
C <- c(10,12,10,8,9,11,10,9,8)
tratamientos_f <- factor(rep(1:4,c(length(P),length(A),length(B),length(C))),
                         labels=c("Placebo","Tratamiento_A","Tratamiento_B","Tratamiento_C"))
tratamientos_v <- c(P,A,B,C)
datos_tratamientos <- as.data.frame(tratamientos_v)
datos_tratamientos[,2] <- tratamientos_f
names(datos_tratamientos) <- c("Tiempo_Recuperacion","Tipo_Tratamiento")
tabla <- table(datos_tratamientos)
tabla

g <- ggplot(data = datos_tratamientos,aes(x=Tipo_Tratamiento,y=Tiempo_Recuperacion,
                                          color=Tipo_Tratamiento))
g+geom_boxplot()+xlab("Tipo de Tratamiento")+ylab("Tiempo de Recuperacion")+geom_point(aes(shape=
                                                                                          Tipo_Tratamiento))

attach(datos_tratamientos)
kruskal.test(Tiempo_Recuperacion,Tipo_Tratamiento,datos_tratamientos)

wilcox.test(Tiempo_Recuperacion[Tipo_Tratamiento=="Placebo"],
            Tiempo_Recuperacion[Tipo_Tratamiento=="Tratamiento_A"],
            alternative="greater")