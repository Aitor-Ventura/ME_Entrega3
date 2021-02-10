#Cuestión 2

library(tseries)
library(ggplot2)

y2<-c(8.1, 7.8, 8.5, 9.8, 9.5, 8.9, 8.6, 10.2, 9.3, 9.2, 10.5)
x2<-c(1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0 )
n<-length(x2)
plot(x2,y2, ylim = c(6,15), pch = 19, col = "red")
grid()

#Apartado a

modelo2<-lm(y2~x2)
b0b1_2<-coefficients(modelo2)
abline(modelo2, col = "blue", lwd =2)

#Apartado b

e2<-residuals(modelo2)
plot(e2, type= "h", lwd=2, col ="red")
abline(h=0, col = "yellow")
grid()

shapiro.test(e2)
ks.test(e2, "pnorm")
jarque.bera.test(e2)
hist(e2, freq = FALSE, col = "green", density = 25, border = "pink")

valores<-seq(min(e2), max(e2), 0.1)
points(valores, dnorm(valores, mean = mean(e2), sd = sd(e2)), type = "l", col ="blue")

#Apartado c

confint(modelo2)
coefficients(modelo2)

#Apartado d

prediccion<-predict(modelo2,newdata = data.frame(x2=1.75), interval = "pred")
prediccion
plot(x2,y2, ylim = c(6,15), pch = 19, col = "red")
grid()

#Apartado e

prediccion_med<-predict(modelo2, newdata = data.frame(x2=1.75, interval = "confidence"))
prediccion_med

#Apartado f

abline (modelo2, col = "blue", lwd = 2)
lines(c(1.75,1.75), c(prediccion_med[2], prediccion_med[3]), col="brown")
points(c(1.75,1.75), c(prediccion_med[2], prediccion_med[3]), col="brown")
