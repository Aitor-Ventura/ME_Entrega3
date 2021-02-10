#Cuestion 1

library(tseries)
library(ggplot2)

# Cuestión 1
Y<-c(50,56,62,70,80)
X<- c(0,4,5,6,9)
plot(X, Y, ylim = c(0,100), pch = 19, col = "red",
     xlab = "Número de muertes", ylab = "Dosificación", cex = 1.5)
grid()

#Apartado a:

modelo <- lm(Y~X)
summary(modelo)
b0b1<-coefficients(modelo)
confint(modelo)
abline(modelo, col = "blue")

#Apartado b:

e<-residuals(modelo)
plot(e, type = "h", pch = 2, col = "red")
abline(h=0, col = "green")
grid()

shapiro.test(e)
ks.test(e,"pnorm")
jarque.bera.test(e)

hist(e, freq = FALSE, col = "green", density = 25, border = "brown")
valores<- seq(min(e), max(e),0.1)
points(valores,dnorm(valores,mean = mean(e), sd= sd(e)), type = "l",col ="blue")

#Apartado c:

confint(modelo)
coefficients(modelo)

#Apartado d:

prediccion<-predict(modelo,newdata = data.frame(X=5),interval = "pred")
prediccion
plot(X,Y, ylim = c(0,100))
abline(modelo, col = "blue")
lines(c(5,5), c(prediccion[2], prediccion[3]), col="brown")
points(c(5,5),c(prediccion[2], prediccion[3]), col="brown")
grid()

#Apartado d:

datos<-data.frame(X,Y)
b0b1<-coefficients(modelo)
g<-ggplot(datos=datos,aes(x=X,y=Y))
g+geom_point(color = "red")+
  geom_smooth(method = "ln")+
  geom_linerange(aes(x=S,ymin=prediccion[2],ymax=prediccion[3]))


