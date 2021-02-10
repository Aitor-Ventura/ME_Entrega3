#cuestion 4

#Pearson

estudiante<- c(A,B,C,D,E,F,G,H,I,J,L)
test1 <- c(92,89,86,83,77,71,62,2.6,53,40)
test2 <- c(88,85,93,79,70,87,52,84,41,64)
datos <- data.frame(test1, test2)
chart.Correlation(datos)
cor(test1,test2)
cor.test (test1, test2)
shapiro.test(test1)
shapiro.test(test2)

#Spearman

cor(test1, test2, method = "spearman")
cor.test (test1, test2, method = "spearman")