use "C:\Users\Eduardo Torres\Desktop\set2\Econometria_II_SP03.dta"
**** resumen estadistico
summarize ans bid ingreso educacion gru_fam automovil ecivil edad sexo
pwcorr ans bid ingreso educacion gru_fam automovil ecivil edad sexo, sig
**Inicialmente aplicaremos LPM
reg ans bid ingreso educacion gru_fam automovil ecivil edad sexo
estimates store OLS
predict p_lpm, xb
g Linear_prediction=_b[_cons]+_b[bid]*bid+_b[ingreso]*22146.19+_b[educacion]*13.38117+_b[gru_fam]*3.071749+_b[automovil]*1+_b[ecivil]*1+_b[edad]*34.49327+_b[sexo]*1 if e(sample)
label var Linear_prediction "Prediccion lineal de ANS por OLS"
*A continuacion estimaremos un modelo probit/logt para cuatificar los factores que inciddiran en la probabilidad de compra de este producto
probit ans bid ingreso educacion gru_fam automovil ecivil edad sexo
mfx
predict p_probit, pr
estimates store PROBIT
estat classification
lsens
g pred_probit=_b[_cons]+_b[bid]*bid+_b[ingreso]*22146.19+_b[educacion]*13.38117+_b[gru_fam]*3.071749+_b[automovil]*1+_b[ecivil]*1+_b[edad]*34.49327+_b[sexo]*1 if e(sample)
g prob_pred_probit=normal(pred_probit)
label var prob_pred_probit "Prediccion por modelo probit"
*comparando el modelo probit con el logit
logit ans bid ingreso educacion gru_fam automovil ecivil edad sexo
mfx
predict p_logt,pr
estimates store LOGIT
estat classification
lsens
g pred_logit=_b[_cons]+_b[bid]*bid+_b[ingreso]*22146.19+_b[educacion]*13.38117+_b[gru_fam]*3.071749+_b[automovil]*1+_b[ecivil]*1+_b[edad]*34.49327+_b[sexo]*1 if e(sample)
g prob_pred_logit=logistic(pred_logit)
label var prob_pred_logit "prediccion del modelo logit"
estimates table OLS PROBIT LOGIT, t stats(N ll) b(%7.3f) p(%7.3f)
twoway (line Linear_prediction bid) (scatter ans bid if Linear_prediction!=.), xtitle("Disponibilidad de pago") ytitle("Probabilidad de comprar") title("Predicción Lineal") legend(off) name(g1, replace)
twoway (line prob_pred_probit bid) (scatter Linear_prediction bid), xtitle("Disponibilidad de Pago") ytitle("Probabilidad de comprar") title("Predicción Probit")  legend(off) name(g2, replace)
twoway (line prob_pred_logit bid) (scatter Linear_prediction bid), xtitle("Disponibilidad de Pago") ytitle("Probabilidad de comprar") title("Predicción Logit")  legend(off) name(g3, replace)
graph combine g1 g2 g3, rows(1) ysize(3)
******************Desventaje de LPM: Sobre estima pro para altos valores de X (control)... Subestima la probabilidad para bajos valores en X
***construccion del perfil
ttest bid, by(ans) unequal
ttest gru_fam, by(ans) unequal
ttest ingreso , by(ans) unequal
ttest educacion , by(ans) unequal
ttest automovil , by(ans) unequal

* Pruebas de especificacion en Razon de verosimilitud y prueba de Wald
*Eliminamos BID
probit ans ingreso educacion gru_fam automovil ecivil edad sexo
estimates store eq2
lrtest (PROBIT) ( eq2)
logit ans ingreso educacion gru_fam automovil ecivil edad sexo
estimates store eq3
lrtest (LOGIT) ( eq3)
probit ans bid ingreso educacion gru_fam automovil ecivil edad sexo
test (bid=0)
logit ans bid ingreso educacion gru_fam automovil ecivil edad sexo
test (bid=0)

**ahora eliminamos la variable ingreso
probit ans bid educacion gru_fam automovil ecivil edad sexo
estimates store eq4
lrtest (PROBIT) (eq4)
logit ans bid educacion gru_fam automovil ecivil edad sexo
estimates store eq5
lrtest (LOGIT) (eq5)
probit ans bid ingreso educacion gru_fam automovil ecivil edad sexo
test (ingreso=0)
logit ans bid ingreso educacion gru_fam automovil ecivil edad sexo
test (ingreso=0)

**ahora eliminamos la variable educacion
probit ans bid ingreso gru_fam automovil ecivil edad sexo
estimates store eq6
lrtest (PROBIT) (eq6)
logit ans bid ingreso gru_fam automovil ecivil edad sexo
estimates store eq7
lrtest (LOGIT) (eq7)
probit ans bid ingreso educacion gru_fam automovil ecivil edad sexo
test (educacion=0)
logit ans bid ingreso educacion gru_fam automovil ecivil edad sexo
test (educacion=0)






