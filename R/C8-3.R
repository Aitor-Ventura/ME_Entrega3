#Cuestion 3

#Apartado a

rentas <-c(1500.21,880.66,605.22,1210.12,2010.1,701.3,2060.01,810.10,1012.34,1500.08,2500,917.45,890.5,515.01,820.39,1800.3,625.12,1002.2,2015.22,720.25,1102.45,3200,1601.79,1219.7,1005.4,2150.1,623.56)
media_rentas<-mean(rentas)
desviacionsd_retenas<-sd(rentas)
z1<-qt(0.975,26)
z2<-qt(0.025,26)
val_z1=(z1*desviacionsd_retenas)+media_rentas
val_z2=(z2*desviacionsd_retenas)+media_rentas
show(val_z2)
show(val_z1)

#Apartado b

nuevas_rentas<-c(1500.21,880.66,605.22,1210.12,2010.1,701.3,2060.01,810.10,1012.34,1500.08,2500,917.45,890.5,515.01,820.39,1800.3,625.12,1002.2,2015.22,720.25,1102.45,1601.79,1219.7,1005.4,2150.1,623.56)
media_nuevas_rentas<-mean(nuevas_rentas)
desviacionsd_nuevas_retenas<-sd(nuevas_rentas)
show(desviacionsd_nuevas_retenas)
