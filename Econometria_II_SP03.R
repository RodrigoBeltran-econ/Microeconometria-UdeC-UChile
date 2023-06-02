
install.packages("Hmisc")
install.packages("mfx")
install.packages("caret")
install.packages("pROC")
install.packages("textreg")
install.packages("dplyr")
install.packages("aod")
library(haven)
library(Hmisc)
library(AER)
library(mfx)
library(caret)
library(pROC)
library(textreg)
library (dplyr)
library(ROCR)
library(aod)

data <- read_dta("/Users/feliperifo/Desktop/Econometra 2/SP03/Econometria_II_SP03.dta")
View(data)
attach(data)


########## Estadística descriptiva y correlaciones
summary(data)
rcorr(as.matrix(data))

########## Prueba de diferencia de medias 
t.test(bid~ans, data=data)             #son iguales
t.test(ingreso~ans, data=data)         #son iguales
t.test(educacion~ans, data=data)       #son iguales
t.test(gru_fam~ans, data=data)         #no son iguales
t.test(automovil~ans, data=data)       #son iguales
t.test(ecivil~ans, data=data)          #no son iguales
t.test(edad~ans, data=data)            #no son iguales
t.test(sexo~ans, data=data)            #no son iguales

########################################### Modelo LPM ####################################################
LPM <- lm(ans~ bid+ingreso+educacion+gru_fam+automovil+ecivil+edad+sexo)
predict_lpm <-predict.lm(LPM)
predict_lpm

################################################## PROBIT #################################################
probit <- glm(ans~bid+ingreso+educacion+gru_fam+automovil+ecivil+edad+sexo, family=binomial(link="probit"))
summary(probit)
probitmfx <-probitmfx(probit, data=data, atmean=TRUE)
probitmfx
predict_probit <-predict.glm(probit, type="response")
predict_probit

                    
roc <- roc( data$ans, predict_probit, auc = TRUE, ci = FALSE  )
print( roc )
plot.roc( roc, 
          add=FALSE,
          legacy.axes = TRUE, 
          print.auc = TRUE,
          auc.polygon = FALSE, 
          max.auc.polygon = TRUE, 
          auc.polygon.col="gainsboro", 
          print.thres=TRUE,
          col = 2, grid = FALSE)

######################## sensitivity and specificity      ######################
threshold=0.5                                                           #######
predicho<-ifelse(predict_probit>=threshold,1,0)                         #######
real<-data$ans                                                          #######
conf_matrix_probit<-table(predicho,real)                                #######
conf_matrix_probit                                                      #######                                                                       #######
sensitivity_probit<- (101/117)*100                                      #######
sensitivity_probit                                                      #######
specificity_probit<-(89/106)*100                                        #######
specificity_probit                                                      #######
######################### Correctly classified                          #######
tabla.clasif <- table(data$ans, predicho)                               #######
tcc_probit <- 100 * sum(diag(tabla.clasif))/sum(tabla.clasif)           #######
tcc_probit                                                              #######
###############################################################################


############################################ LOGIT ######################################################
logit <- glm(ans~bid+ingreso+educacion+gru_fam+automovil+ecivil+edad+sexo, family=binomial(link="logit"))
summary(logit)
logitmfx <-logitmfx(logit, data=data, atmean=TRUE)
logitmfx
predict_logit <-predict.glm(logit, type="response")
predict_logit



roc1 <- roc( data$ans, predict_logit, auc = TRUE, ci = FALSE  )
print( roc1 )
plot.roc( roc1, 
          add=FALSE,
          legacy.axes = TRUE, 
          print.auc = TRUE,
          auc.polygon = FALSE, 
          max.auc.polygon = TRUE, 
          auc.polygon.col="gainsboro", 
          print.thres=TRUE,
          col = 2, grid = FALSE)

######################## sesitivity and specificity      #############################
threshold=0.5                                                           ##############
predict<-ifelse(predict_logit<threshold,0,1)                            ##############
rial<-data$ans                                                          ##############
conf_matrix_logit<-table(predict,rial)                                  ##############
conf_matrix_logit                                                       ##############
sensitivity_logit <- (102/117)*100                                      ##############
sensitivity_logit                                                       ##############
specificity_logit <- (90/106)*100                                       ##############
specificity_logit                                                       ##############
######################### Correctly classified  ######################################
tabla.clasif_logit <- table(data$ans, predict)                           #############
tcc_logit <- 100 * sum(diag(tabla.clasif_logit))/sum(tabla.clasif_logit) #############
tcc_logit                                                                #############
######################################################################################


######################### Modelos Restringidos ##############################################################
######### PROBIT 
probit2 <- glm(ans~ingreso+educacion+gru_fam+automovil+ecivil+edad+sexo, family=binomial(link="probit"))
summary(probit2)
probitmfx2 <-probitmfx(probit2, data=data, atmean=TRUE)
probitmfx2
predict_probit2 <-predict.glm(probit2, type="response")
predict_probit2

probit3 <- glm(ans~bid+educacion+gru_fam+automovil+ecivil+edad+sexo, family=binomial(link="probit"))
summary(probit3)
probitmfx3 <-probitmfx(probit3, data=data, atmean=TRUE)
probitmfx3
predict_probit3 <-predict.glm(probit3, type="response")
predict_probit3

probit4 <- glm(ans~bid+ingreso+gru_fam+automovil+ecivil+edad+sexo, family=binomial(link="probit"))
summary(probit4)
probitmfx4 <-probitmfx(probit4, data=data, atmean=TRUE)
probitmfx4
predict_probit4 <-predict.glm(probit4, type="response")
predict_probit4

######### LOGIT
logit2 <- glm(ans~ingreso+educacion+gru_fam+automovil+ecivil+edad+sexo, family=binomial(link="logit"))
summary(logit2)
logitmfx2 <-logitmfx(logit2, data=data, atmean=TRUE)
logitmfx2
predict_logit2 <-predict.glm(logit2, type="response")
predict_logit2

logit3 <- glm(ans~bid+educacion+gru_fam+automovil+ecivil+edad+sexo, family=binomial(link="logit"))
summary(logit3)
logitmfx3 <-logitmfx(logit3, data=data, atmean=TRUE)
logitmfx3
predict_logit3 <-predict.glm(logit3, type="response")
predict_logit3

logit4 <- glm(ans~bid+ingreso+gru_fam+automovil+ecivil+edad+sexo, family=binomial(link="logit"))
summary(logit4)
logitmfx4 <-logitmfx(logit4, data=data, atmean=TRUE)
logitmfx4
predict_logit4 <-predict.glm(logit4, type="response")
predict_logit4

###############################################################################################################

##########Pruebas lr
lrtest(probit, probit2)
lrtest(probit, probit3)
lrtest(probit, probit4)
lrtest(logit, logit2)
lrtest(logit, logit3)
lrtest(logit, logit4)

########## Prueba de wald
wald.test(b = coef(probit), Sigma = vcov(probit), Terms = 1:3)
wald.test(b = coef(probit), Sigma = vcov(probit), Terms = 1)
wald.test(b = coef(probit), Sigma = vcov(probit), Terms = 2)
wald.test(b = coef(probit), Sigma = vcov(probit), Terms = 3)
wald.test(b = coef(logit), Sigma = vcov(logit), Terms = 1:3)
wald.test(b = coef(logit), Sigma = vcov(logit), Terms = 1)
wald.test(b = coef(logit), Sigma = vcov(logit), Terms = 2)
wald.test(b = coef(logit), Sigma = vcov(logit), Terms = 3)
