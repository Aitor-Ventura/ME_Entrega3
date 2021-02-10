#Lab 6 ej 1
library(MASS)
library(vcd)
num_dias <- c(41,81,87,54,30,12,3)
num_sol <- c(0,1,2,3,4,5,6)
var_poisson <- data.frame(num_sol,num_dias)
var_p <- c(rep(num_sol,num_dias))

ajuste.poisson <- goodfit(var_p,type="poisson",method="MinChisq")
summary(ajuste.poisson)
ajuste.poisson$par
ajuste.poisson

hist(var_p,breaks=-0.5:6.5,xlab="Num Solicitudes",ylab="Dias",main="Solicitudes de Credito",
     col="gray",border="black",density=50)
grid()
points(0:6,ajuste.poisson$fitted,type="h",lwd=3,col="red")
points(0:6,ajuste.poisson$fitted,pch=19,col="red")
