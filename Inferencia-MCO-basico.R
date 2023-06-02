#Se abre la base de datos:
data1<-read.csv("C:/Users/Rodrigo/Desktop/Set_practico_02_Encuesta_creditos.csv", header=T, sep=",", dec=".")
attach(data1)
#Comprobando la base de datos.
data1
#Haciendo un análisis descriptivo de la base de datos.
summary(data1)
####Se trabajará en la regresión compuesta por las variables macroeconómicas.
#Empezamos verificando un análisis de correlación para Pequeñas Empresas:
cor(data1[,c("PE","Entorno_Eco","Fondeo_Banco","Cap_Banco")], use="complete")
#Ahora un análisis de correlación para Grandes Empresas:
cor(data1[,c("GE","Entorno_Eco","Fondeo_Banco","Cap_Banco")], use="complete")
#Ahora procedemos a realizar la regresión primeramente para pequeñas empresas:
RegModel.1 <- lm(PE~Cap_Banco+Entorno_Eco+Fondeo_Banco, data=data1)
summary(RegModel.1)
#Regresión para Pequeñas Empresas con IFRS.
RegModel.2 <- lm(PE~Cap_Banco+Entorno_Eco+Fondeo_Banco+IFRS_PE, data=data1)
summary(RegModel.2)
#Regresión para Grandes empresas.
RegModel.3 <- lm(GE~Cap_Banco+Entorno_Eco+Fondeo_Banco, data=data1)
summary(RegModel.3)
#Regresión para Grandes Empresas con IFRS.
RegModel.4 <- lm(GE~Cap_Banco+Entorno_Eco+Fondeo_Banco+IFRS_GE, data=data1)
summary(RegModel.4)
####Veremos las condiciones crediticias en relación a las variables del sector industrial.
#Se realiza un análisis de correlación para Pequeñas empresas.
cor(data1[,c("PE","Comp_Bancaria_PE","Riesgo_cartera_PE")], use="complete")
#Análisis de correlación para Grandes Empresas.
cor(data1[,c("GE","Comp_Bancaria_GE","Riesgo_cartera_GE")], use="complete")
#Se realiza la regresión para Pequeñas Empresas.
RegModel.5 <- lm(PE~Comp_Bancaria_PE+Riesgo_cartera_PE, data=data1)
summary(RegModel.5)
#Regresión para Pequeñas Empresas con IFRS.
RegModel.6 <- lm(PE~Comp_Bancaria_PE+Riesgo_cartera_PE+IFRS_PE, data=data1)
summary(RegModel.6)
#Regresión para Grandes Empresas.
RegModel.7 <- lm(GE~Comp_Bancaria_GE+Riesgo_cartera_GE, data=data1)
summary(RegModel.7)
#Regresión para Grandes Empresas con IFRS.
RegModel.8 <- lm(GE~Comp_Bancaria_GE+Riesgo_cartera_GE+IFRS_GE, data=data1)
summary(RegModel.8)
####Se medirá las condiciones crediticias respecto a los factores microeconómicos presentes.
#Se realiza análisis de correlación para Pequeñas Empresas.
cor(data1[,c("PE","Dism_Licred_PE","Mayor_garant_PE","Mayor_Spread_Fond_PE","Menor_plazo_PE")], use="complete")
#Correlación para Empresas Grandes.
cor(data1[,c("GE","Dism_Licred_GE","Mayor_garant_GE","Mayor_Spread_Fond_GE","Menor_plazo_GE")], use="complete")
#Regresión para Pequeñas Empresas.
RegModel.9 <- lm(PE~Dism_Licred_PE+Mayor_garant_PE+Mayor_Spread_Fond_PE+Menor_plazo_PE, data=data1)
summary(RegModel.9)
#Regresión para Pequeñas Empresas con IFRS.
RegModel.10 <- lm(PE~Dism_Licred_PE+Mayor_garant_PE+Mayor_Spread_Fond_PE+Menor_plazo_PE+IFRS_PE, data=data1)
summary(RegModel.10)
#Regresión para Grandes Empresas.
RegModel.11 <- lm(GE~Dism_Licred_GE+Mayor_garant_GE+Mayor_Spread_Fond_GE+Menor_plazo_GE, data=data1)
summary(RegModel.11)
#Regresión para Grandes Empresas con IFRS.
RegModel.12 <- lm(GE~Dism_Licred_GE+Mayor_garant_GE+Mayor_Spread_Fond_GE+Menor_plazo_GE+IFRS_GE, data=data1)
summary(RegModel.12)

