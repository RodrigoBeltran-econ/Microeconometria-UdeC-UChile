***Resumen estadistico
summarize ratv dueo socio gerente mon_banc activo fuidespedido roav bs holding
***Corremos las regresiones OLS
generate size=ln( activo)
regress ratv dueo mon_banc size fuidespedido roav bs holding
estimates store req1
regress ratv socio mon_banc size fuidespedido roav bs holding
estimates store req2
regress ratv gerente mon_banc size fuidespedido roav bs holding
estimates store req3
estimates table req1 req2 req3, b(%7.4f) p(%7.4f)
****Analizamos el patron de endogeneidad
*CASO A: DUEÑO
ivregress 2sls ratv dueo mon_banc size fuidespedido roav  holding (bs= edad_gte exp_gte) , first
**Evaluando la relevancia de los instrumentos
ivregress 2sls ratv dueo mon_banc size fuidespedido roav  holding (bs=  exp_gte) , first
ivregress 2sls ratv dueo mon_banc size fuidespedido roav  holding (bs=  edad_gte ) , first
ivregress 2sls ratv dueo mon_banc size fuidespedido roav  holding (bs= edad_gte exp_gte) , first
estimates store ivreg1
estat endogenous  bs
**Prueba de Hausman
estimates table req1 ivreg1, b(%7.4f) p(%7.4f)
*CASO AB SOCIO
ivregress 2sls ratv dueo mon_banc size fuidespedido roav  holding (bs= edad_gte exp_gte) , first
**Evaluando la relevancia de los instrumentos
ivregress 2sls ratv socio mon_banc size fuidespedido roav  holding (bs=  exp_gte) , first
ivregress 2sls ratv socio mon_banc size fuidespedido roav  holding (bs=  edad_gte ) , first
ivregress 2sls ratv socio mon_banc size fuidespedido roav  holding (bs= edad_gte exp_gte) , first
estimates store ivreg2
estat endogenous  bs
**Prueba de Hausman
estimates table req2 ivreg2, b(%7.4f) p(%7.4f)
*CASO C: GERENTE
ivregress 2sls ratv dueo mon_banc size fuidespedido roav  holding (bs= edad_gte exp_gte) , first
**Evaluando la relevancia de los instrumentos
ivregress 2sls ratv gerente mon_banc size fuidespedido roav  holding (bs=  exp_gte) , first
ivregress 2sls ratv gerente mon_banc size fuidespedido roav  holding (bs=  edad_gte ) , first
ivregress 2sls ratv gerente mon_banc size fuidespedido roav  holding (bs= edad_gte exp_gte) , first
estimates store ivreg3
estat endogenous  bs
**Prueba de Hausman
estimates table req3 ivreg3, b(%7.4f) p(%7.4f)
ssc install ranktest 
ivreg2 ratv dueo mon_banc size fuidespedido roav  holding (bs= edad_gte exp_gte) , first
**una forma alternativa de testear endogeneidad por error de medida
**Primero corremos la regresión de la primera etapa
regress bs edad_gte exp_gte dueo mon_banc size fuidespedido roav  holding
predict res, resid
regress ratv  dueo mon_banc size fuidespedido roav  holding res
