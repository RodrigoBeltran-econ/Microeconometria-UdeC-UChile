library(haven)
Datos<-read_dta("Desktop/Econometra 2/Econometria_II_SP01.dta")
Datos$size <-with(Datos,log(activo))
View(Datos)
attach(Datos)
###############################################################
########################## An谩lisis Descriptivo ###############

summary(Datos)
cor(Datos)

###########################################################################
####################### An谩lisis Econom茅trico por OLS #####################

######## OLS1 con estructura de propiedad "Due帽o" 

size[!is.finite(size)]<-NA
OLS1<- lm(ratv ~ Due駉 + mon_banc + size + fuidespedido + roav + bs + holding)
summary(OLS1)

######## OLS2 con estructura de propiedad "Socio"

OLS2<- lm(ratv ~ socio + mon_banc + size + fuidespedido + roav + bs + holding)
summary(OLS2)

######## OLS3 con estructura de propiedad "Gerente" 

OLS3<- lm(ratv ~ gerente + mon_banc + size + fuidespedido + roav + bs + holding)
summary(OLS3)

##################################################################
##################### Control de Endogeneidad ################

#install.packages("AER")
library(AER)
#install.packages("systemfit")
library(systemfit)
expgte=Datos$exp_gte
edadgte=Datos$edad_gte

######################## Estructura de propiedad Due帽o ######################
####### Instrumentalizaci贸n para OLS1 con 2 instrumentos

ivreg1 <- ivreg( ratv ~ Due駉 + mon_banc + size + fuidespedido + roav + bs + holding | Due駉 + mon_banc + size + fuidespedido + roav + holding + expgte + edadgte)
summary(ivreg1, vcov=sandwich, df=Inf, diagnostics=TRUE) 

######  Instrumentalizaci贸n para OLS1 con instrumento edad_gte

ivreg1.1 <- ivreg( ratv ~ Due駉 + mon_banc + size + fuidespedido + roav + bs + holding | Due駉 + mon_banc + size + fuidespedido + roav + holding + edadgte)
summary(ivreg1.1, vcov=sandwich, df=Inf, diagnostics=TRUE) 

######  Instrumentalizaci贸n para OLS1 con instrumento exp_gte

ivreg1.2 <- ivreg( ratv ~ Due駉 + mon_banc + size + fuidespedido + roav + bs + holding | Due駉 + mon_banc + size + fuidespedido + roav + holding + expgte)
summary(ivreg1.2, vcov=sandwich, df=Inf, diagnostics=TRUE) 

######################## Estructura de propiedad Socio ######################
####### Instrumentalizaci贸n para OLS2 con 2 instrumentos

ivreg2 <- ivreg( ratv ~ socio + mon_banc + size + fuidespedido + roav + bs + holding | socio + mon_banc + size + fuidespedido + roav + holding + expgte + edadgte)
summary(ivreg2, vcov=sandwich, df=Inf, diagnostics=TRUE) 

######  Instrumentalizaci贸n para OLS2 con instrumento edad_gte

ivreg2.1 <- ivreg( ratv ~ socio + mon_banc + size + fuidespedido + roav + bs + holding | socio + mon_banc + size + fuidespedido + roav + holding + edadgte)
summary(ivreg1.1, vcov=sandwich, df=Inf, diagnostics=TRUE) 

######  Instrumentalizaci贸n para OLS2 con instrumento exp_gte

ivreg2.2 <- ivreg( ratv ~ socio + mon_banc + size + fuidespedido + roav + bs + holding | socio + mon_banc + size + fuidespedido + roav + holding + expgte)
summary(ivreg1.2, vcov=sandwich, df=Inf, diagnostics=TRUE) 

######################## Estructura de propiedad Gerente ######################
####### Instrumentalizaci贸n para OLS3 con 2 instrumentos

ivreg3 <- ivreg( ratv ~ gerente + mon_banc + size + fuidespedido + roav + bs + holding | gerente + mon_banc + size + fuidespedido + roav + holding + expgte + edadgte)
summary(ivreg3, vcov=sandwich, df=Inf, diagnostics=TRUE) 

######  Instrumentalizaci贸n para OLS3 con instrumento edad_gte

ivreg3.1 <- ivreg( ratv ~ gerente + mon_banc + size + fuidespedido + roav + bs + holding | gerente + mon_banc + size + fuidespedido + roav + holding + edadgte)
summary(ivreg3.1, vcov=sandwich, df=Inf, diagnostics=TRUE) 

######  Instrumentalizaci贸n para OLS3 con instrumento exp_gte

ivreg3.2 <- ivreg( ratv ~ gerente + mon_banc + size + fuidespedido + roav + bs + holding | gerente + mon_banc + size + fuidespedido + roav + holding + expgte)
summary(ivreg3.2, vcov=sandwich, df=Inf, diagnostics=TRUE) 
