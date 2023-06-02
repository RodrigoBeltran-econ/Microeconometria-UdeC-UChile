#Para realizar el análisis, se necesitará el siguiente paquete: 'lmtest'. En caso de no contar con el paquete, sólo debe quitar el '#' de la línea siguiente
#install.packages('lmtest')

library(lmtest)
library(zoo)

data<-read.csv("C:/Users/Rodrigo/Desktop/Set_practico_06_Omision&Inclusion_Agencia.csv", header=T, sep=",", dec=".")

#Se genera la variable Size <- Activo reescalado en ln
data$Size<-log(data$Activo)
attach(data)
data$Size[!is.finite(data$Size)] <- NA

#Análisis estadístico descriptivo
summary(data)

#Análisis de correlación
cor(data[,c("RATV","Mon_Banc","Holding","Dueño","Socio","Gerente","Exp_gte","Fui.despedido","ROAV","RATV","B.S","Activo","Size")], use="complete")

####Se corre la regresión on sus respectivas variables
#Regresión para estructura de propiedad dueño
reg1<-lm(RATV~B.S+Mon_Banc+Size+Fui.despedido+ROAV+Dueño+Holding, data=data)
summary(reg1)

#Test de Ramsey con test de criterio de información
resettest(reg1, power=2:3, type="regressor", data=data)
AIC(reg1,k=2)
BIC(reg1)

#Regresión para estructura de propiedad socio
reg2<-lm(RATV~B.S+Mon_Banc+Size+Fui.despedido+ROAV+Socio+Holding, data=data)
summary(reg2)

#Test de Ramsey con test de criterio de información
resettest(reg2, power=2:3, type="regressor", data=data)
AIC(reg2,k=2)
BIC(reg2)

#Regresión para estructura de propiedad gerente
reg3<-lm(RATV~B.S+Mon_Banc+Size+Fui.despedido+ROAV+Gerente+Holding, data=data)
summary(reg3)

#Test de Ramsey con test de criterio de información
resettest(reg3, power=2:3, type="regressor", data=data)
AIC(reg3,k=2)
BIC(reg3)


####Se corre la regresión quitandole las variables de estructura de propiedad y endeudamiento
reg4<-lm(RATV~Mon_Banc+Size+Fui.despedido+ROAV+Holding, data=data)
summary(reg4)

#Test de Ramsey con test de criterio de información
resettest(reg4, power=2:3, type="regressor", data=data)
AIC(reg4,k=2)
BIC(reg4)

####Se corre la regresión original agregandole la variable experiencia del gerente
#Regresión para estructura de propiedad dueño
reg5<-lm(RATV~B.S+Mon_Banc+Size+Fui.despedido+ROAV+Dueño+Holding+Exp_gte, data=data)
summary(reg5)

#Test de Ramsey con test de criterio de información
resettest(reg5, power=2:3, type="regressor", data=data)
AIC(reg5,k=2)
BIC(reg5)

#Regresión para estructura de propiedad socio
reg6<-lm(RATV~B.S+Mon_Banc+Size+Fui.despedido+ROAV+Socio+Holding+Exp_gte, data=data)
summary(reg6)

#Test de Ramsey con test de criterio de información
resettest(reg6, power=2:3, type="regressor", data=data)
AIC(reg6,k=2)
BIC(reg6)

#Regresión para estructura de propiedad gerente
reg7<-lm(RATV~B.S+Mon_Banc+Size+Fui.despedido+ROAV+Gerente+Holding+Exp_gte, data=data)
summary(reg7)

#Test de Ramsey
resettest(reg7, power=2:3, type="regressor", data=data)
AIC(reg7,k=2)
BIC(reg7)
