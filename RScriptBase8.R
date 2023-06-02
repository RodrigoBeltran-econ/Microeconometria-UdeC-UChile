####Para el test de heterocedasticidad de Breusch Pagan se utiliza el paquete de "olsrr", a lo que se adjunta el comando de instalación a continuación:
#install.packages("olsrr")

#Se abre la base de datos
data<-read.csv("C:/Users/Rodrigo/Desktop/Set_practico_08_Ventas.csv", header=T, sep=",", dec=".")
attach(data)
data

#Análisis estadístico descriptivo
summary(data)

#Análisis de correlación
cor(data[,c("ventas","precio_medio","precio_comp","Publicidad")], use="complete")

####Regresión en OLS
RegModel.1 <- lm(ventas~precio_medio+precio_comp+Publicidad, data=data)
summary(RegModel.1)

#Residuos
RegModel.1Res<-resid(RegModel.1)
summary(RegModel.1Res)

#Test de Heterocedasticidad (incluye al modelo e individual)
library(olsrr)
ols_bp_test(RegModel.1, rhs = TRUE, multiple = TRUE)

par(mfrow=c(4,2))
#Graficando comportamiento de los residuos
plot(data$date, RegModel.1Res, 
     ylab="Residuals", xlab="Date", 
     main="Comportamiento de los residuos") 
abline(0, 0) 

##Graficando Residuos respecto a variables independientes
#Respecto a Precio Medio
plot(data$precio_medio, RegModel.1Res, 
     ylab="Residuals", xlab="Precio Medio", 
     main="Residuos en Variable Independiente Precio Medio") 
abline(0, 0) 

#Respecto a Precio Competencia
plot(data$precio_comp, RegModel.1Res, 
     ylab="Residuals", xlab="Precio Competencia", 
     main="Residuos en Variable Independiente Precio Competencia") 
abline(0, 0) 

#Respecto a Publicidad
plot(data$Publicidad, RegModel.1Res, 
     ylab="Residuals", xlab="Publicidad", 
     main="Residuos en Variable Independiente Publicidad") 
abline(0, 0) 


####Regresión en ML
x<-cbind(1,data$precio_medio,data$precio_comp,data$Publicidad)
y<-matrix(data$ventas)
y

lll<-function(tetha,y,x){n<-nrow(x)
k<-ncol(x)
beta<-tetha[1:k]
sigma2<-tetha[k+1]
e<-y-x%*%beta
logl<- -.5*n*log(2*pi*sigma2)-((t(e)%*%e)/(2*sigma2))
return(-logl)}
p<-optim(c(1,1,1,1,1),lll,method="BFGS",hessian=T,y=y,x=x)
names(p)
p<-optim(c(10,5,2,3,4),lll,method="BFGS",hessian=T,y=y,x=x)
p<-optim(c(10,5,2,3,90),lll,method="BFGS",hessian=T,y=y,x=x)
names(p)
p







###Calculando OLS con test de White (robusto)

##Planteando test de White
## Heteroskedasticity-robust standard error calculation.
summaryw <- function(model) {
s <- summary(model)
X <- model.matrix(model)
u2 <- residuals(model)^2
XDX <- 0
 
## Here one needs to calculate X'DX. But due to the fact that
## D is huge (NxN), it is better to do it with a cycle.
for(i in 1:nrow(X)) {
XDX <- XDX + u2[i]*X[i,]%*%t(X[i,])
}
 
# inverse(X'X)
XX1 <- solve(t(X)%*%X)
 
# Variance calculation (Bread x meat x Bread)
varcovar <- XX1 %*% XDX %*% XX1
 
# degrees of freedom adjustment
dfc <- sqrt(nrow(X))/sqrt(nrow(X)-ncol(X))
 
# Standard errors of the coefficient estimates are the
# square roots of the diagonal elements
stdh <- dfc*sqrt(diag(varcovar))
 
t <- model$coefficients/stdh
p <- 2*pnorm(-abs(t))
results <- cbind(model$coefficients, stdh, t, p)
dimnames(results) <- dimnames(s$coefficients)
results
}

#Regresión OLS robusta
summaryw(RegModel.1)


####Reescalando las variables en ln
data$lnventas<-log(data$ventas)
data$lnprecio_medio<-log(data$precio_medio)
data$lnprecio_comp<-log(data$precio_comp)
data$lnpublicidad<-log(data$Publicidad)
attach(data)

####Regresión en OLS
RegModel.2 <- lm(lnventas~lnprecio_medio+lnprecio_comp+lnpublicidad, data=data)
summary(RegModel.2)

#Residuos
RegModel.2Res<-resid(RegModel.2)
summary(RegModel.2Res)

#Test de Heterocedasticidad (incluye al modelo e individual)
ols_bp_test(RegModel.2, rhs = TRUE, multiple = TRUE)


#Graficando comportamiento de los residuos
plot(data$date, RegModel.2Res, 
     ylab="Residuals", xlab="Date", 
     main="Comportamiento de los residuos") 
abline(0, 0) 

##Graficando Residuos respecto a variables independientes
#Respecto a Precio Medio
plot(data$lnprecio_medio, RegModel.2Res, 
     ylab="Residuals", xlab="ln(Precio Medio)", 
     main="Residuos en Variable Independiente ln(Precio Medio)") 
abline(0, 0) 

#Respecto a Precio Competencia
plot(data$lnprecio_comp, RegModel.2Res, 
     ylab="Residuals", xlab="ln(Precio Competencia)", 
     main="Residuos en Variable Independiente ln(Precio Competencia)") 
abline(0, 0) 

#Respecto a Publicidad
plot(data$lnpublicidad, RegModel.2Res, 
     ylab="Residuals", xlab="ln(Publicidad)", 
     main="Residuos en Variable Independiente ln(Publicidad)") 
abline(0, 0) 



####Regresión en ML
x<-cbind(1,data$lnprecio_medio,data$lnprecio_comp,data$lnpublicidad)
y<-matrix(data$lnventas)
y

lll<-function(tetha,y,x){n<-nrow(x)
k<-ncol(x)
beta<-tetha[1:k]
sigma2<-tetha[k+1]
e<-y-x%*%beta
logl<- -.5*n*log(2*pi*sigma2)-((t(e)%*%e)/(2*sigma2))
return(-logl)}
pp<-optim(c(1,1,1,1,1),lll,method="BFGS",hessian=T,y=y,x=x)
names(pp)
pp<-optim(c(10,5,2,3,4),lll,method="BFGS",hessian=T,y=y,x=x)
pp<-optim(c(10,5,2,3,90),lll,method="BFGS",hessian=T,y=y,x=x)
names(pp)
pp




###Calculando OLS con test de White (robusto)
summaryw(RegModel.2)
