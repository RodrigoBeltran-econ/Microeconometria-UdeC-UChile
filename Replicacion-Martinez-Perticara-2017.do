***********************************
********* Empírico ****************
***********************************
clear all

use "/Users/rodrigo/data_analysis.dta"

***Se busca replicar la tabla 2 según indicaciones del paper

*Para la muestra total
sum LB_edad_post LB_ingpercap_post LB_ing_laboral LB_participa LB_desempleo LB_ocupadas LB_hrs_trabaja LB_mes_trabaja LB_contrato LB_edu_anos LB_edu_max1 LB_edu_max2 LB_edu_max3 LB_edu_max4 LB_asiste_post LB_stress LB_bebe LB_n_kids LB_cuida_infantil est1 est2 est3 est4 if seg==1

*outreg2 LB_edad_post LB_ingpercap_post LB_ing_laboral LB_participa LB_desempleo LB_ocupadas LB_hrs_trabaja LB_mes_trabaja LB_contrato LB_edu_anos LB_edu_max1 LB_edu_max2 LB_edu_max3 LB_edu_max4 LB_asiste_post LB_stress LB_bebe LB_n_kids LB_cuida_infantil est1 est2 est3 est4 if seg==1 using stat.tex, replace sum(log) keep(LB_edad_post LB_ingpercap_post LB_ing_laboral LB_participa LB_desempleo LB_ocupadas LB_hrs_trabaja LB_mes_trabaja LB_contrato LB_edu_anos LB_edu_max1 LB_edu_max2 LB_edu_max3 LB_edu_max4 LB_asiste_post LB_stress LB_bebe LB_n_kids LB_cuida_infantil est1 est2 est3 est4) eqkeep(mean sd N)


*Para la muestra de tratamiento
sum LB_edad_post LB_ingpercap_post LB_ing_laboral LB_participa LB_desempleo LB_ocupadas LB_hrs_trabaja LB_mes_trabaja LB_contrato LB_edu_anos LB_edu_max1 LB_edu_max2 LB_edu_max3 LB_edu_max4 LB_asiste_post LB_stress LB_bebe LB_n_kids LB_cuida_infantil est1 est2 est3 est4 if seg==1 & trat==1

*outreg2 LB_edad_post LB_ingpercap_post LB_ing_laboral LB_participa LB_desempleo LB_ocupadas LB_hrs_trabaja LB_mes_trabaja LB_contrato LB_edu_anos LB_edu_max1 LB_edu_max2 LB_edu_max3 LB_edu_max4 LB_asiste_post LB_stress LB_bebe LB_n_kids LB_cuida_infantil est1 est2 est3 est4 if seg==1 & trat==1 using stattrat.tex, replace sum(log) keep(LB_edad_post LB_ingpercap_post LB_ing_laboral LB_participa LB_desempleo LB_ocupadas LB_hrs_trabaja LB_mes_trabaja LB_contrato LB_edu_anos LB_edu_max1 LB_edu_max2 LB_edu_max3 LB_edu_max4 LB_asiste_post LB_stress LB_bebe LB_n_kids LB_cuida_infantil est1 est2 est3 est4) eqkeep(mean sd N)

*Para la muestra de control 
sum LB_edad_post LB_ingpercap_post LB_ing_laboral LB_participa LB_desempleo LB_ocupadas LB_hrs_trabaja LB_mes_trabaja LB_contrato LB_edu_anos LB_edu_max1 LB_edu_max2 LB_edu_max3 LB_edu_max4 LB_asiste_post LB_stress LB_bebe LB_n_kids LB_cuida_infantil est1 est2 est3 est4 if seg==1 & control==1

*outreg2 LB_edad_post LB_ingpercap_post LB_ing_laboral LB_participa LB_desempleo LB_ocupadas LB_hrs_trabaja LB_mes_trabaja LB_contrato LB_edu_anos LB_edu_max1 LB_edu_max2 LB_edu_max3 LB_edu_max4 LB_asiste_post LB_stress LB_bebe LB_n_kids LB_cuida_infantil est1 est2 est3 est4 if seg==1 & control==1 using statcont.tex, replace sum(log) keep(LB_edad_post LB_ingpercap_post LB_ing_laboral LB_participa LB_desempleo LB_ocupadas LB_hrs_trabaja LB_mes_trabaja LB_contrato LB_edu_anos LB_edu_max1 LB_edu_max2 LB_edu_max3 LB_edu_max4 LB_asiste_post LB_stress LB_bebe LB_n_kids LB_cuida_infantil est1 est2 est3 est4) eqkeep(mean sd N)


*Test de diferencia de medias
ttest LB_edad_post if seg==1, by(treatment)
ttest LB_ingpercap_post if seg==1, by(treatment)
ttest LB_ing_laboral if seg==1, by(treatment)
ttest LB_participa if seg==1, by(treatment)
ttest LB_desempleo if seg==1, by(treatment)
ttest LB_ocupadas  if seg==1, by(treatment)
ttest LB_hrs_trabaja if seg==1, by(treatment)
ttest LB_mes_trabaja if seg==1, by(treatment)
ttest LB_contrato if seg==1, by(treatment)
ttest LB_edu_anos if seg==1, by(treatment)
ttest LB_edu_max1 if seg==1, by(treatment)
ttest LB_edu_max2 if seg==1, by(treatment)
ttest LB_edu_max3 if seg==1, by(treatment)
ttest LB_edu_max4 if seg==1, by(treatment)
ttest LB_asiste_post if seg==1, by(treatment)
ttest LB_stress if seg==1, by(treatment)
ttest LB_bebe if seg==1, by(treatment)
ttest LB_n_kids if seg==1, by(treatment)
ttest LB_cuida_infantil if seg==1, by(treatment)
ttest est1 if seg==1, by(treatment)
ttest est2 if seg==1, by(treatment)
ttest est3 if seg==1, by(treatment)
ttest est4 if seg==1, by(treatment)


***
***


*Se busca replicar la tabla 5   



drop if meses_10!=10

****Primero para la parte (a) de la muestra completa

******************Panel A******************

*Labor Force Participation*
areg participa_unmes_mp treatment LB_jefa_hog LB_edu_anos LB_educ_i LB_cant_nino LB_edad_post if seg==1, cl(comuna_esc) absorb(est_rbd)
sum participa_unmes_mp if treatment==0 & e(sample)

areg participasiempre_mp treatment LB_jefa_hog LB_edu_anos LB_educ_i LB_cant_nino LB_edad_post if seg==1, cl(comuna_esc) absorb(est_rbd)
sum participasiempre_mp if treatment==0 & e(sample)

areg mp_meses_activ treatment LB_jefa_hog LB_edu_anos LB_educ_i LB_cant_nino LB_edad_post if seg==1, cl(comuna_esc) absorb(est_rbd)
sum mp_meses_activ if treatment==0 & e(sample)

**Employment**

areg trabaja_unmes_mp treatment LB_jefa_hog LB_edu_anos LB_educ_i LB_cant_nino LB_edad_post if seg==1, cl(comuna_esc) absorb(est_rbd)
sum trabaja_unmes_mp if treatment==0 & e(sample)

areg trabajasiempre_mp treatment LB_jefa_hog LB_edu_anos LB_educ_i LB_cant_nino LB_edad_post if seg==1, cl(comuna_esc) absorb(est_rbd)
sum trabajasiempre_mp if treatment==0 & e(sample)

areg mes_trabaja treatment LB_jefa_hog LB_edu_anos LB_educ_i LB_cant_nino LB_edad_post if seg==1, cl(comuna_esc) absorb(est_rbd)
sum mes_trabaja if treatment==0 & e(sample)



******************Panel B******************



*********************************************************************************
****************************Labor Force Participation****************************
*********************************************************************************


areg participa_unmes_mp trabybebe notrabybebe trabynobebe notrabynobebe LB_jefa_hog LB_edu_anos LB_educ_i LB_cant_nino LB_edad_post if seg==1, cl(comuna_esc) absorb(est_rbd)
sum participa_unmes_mp if treatment==0 & est1==1 & e(sample)
sum participa_unmes_mp if treatment==0 & est2==1 & e(sample)
sum participa_unmes_mp if treatment==0 & est3==1 & e(sample)
sum participa_unmes_mp if treatment==0 & est4==1 & e(sample)

test trabybebe=notrabybebe=trabynobebe=notrabynobebe


*** Se crea nueva variable para testear, como lo indica la tabla 5
*Se hace vs entre los que tienen bebé (<5 años) vs los que NO tienen bebé (>5 años) 
*Luego se plantea la regresión con la variable nueva + treatment junto con el set de variables de control
*Y se testea, obteniendo los p-value
*Esto se replica para las 6 variables

gen tiene_bebe=trabybebe+notrabybebe
areg participa_unmes_mp tiene_bebe treatment LB_jefa_hog LB_edu_anos LB_educ_i LB_cant_nino LB_edad_post if seg==1, cl(comuna_esc) absorb(est_rbd)
test tiene_bebe==0

******************

areg participasiempre_mp trabybebe notrabybebe trabynobebe notrabynobebe LB_jefa_hog LB_edu_anos LB_educ_i LB_cant_nino LB_edad_post if seg==1, cl(comuna_esc) absorb(est_rbd)
sum participasiempre_mp if treatment==0 & est1==1 & e(sample)
sum participasiempre_mp if treatment==0 & est2==1 & e(sample)
sum participasiempre_mp if treatment==0 & est3==1 & e(sample)
sum participasiempre_mp if treatment==0 & est4==1 & e(sample)

test trabybebe=notrabybebe=trabynobebe=notrabynobebe

areg participasiempre_mp tiene_bebe treatment LB_jefa_hog LB_edu_anos LB_educ_i LB_cant_nino LB_edad_post if seg==1, cl(comuna_esc) absorb(est_rbd)
test tiene_bebe==0

******************

areg mp_meses_activ trabybebe notrabybebe trabynobebe notrabynobebe LB_jefa_hog LB_edu_anos LB_educ_i LB_cant_nino LB_edad_post if seg==1, cl(comuna_esc) absorb(est_rbd)
sum mp_meses_activ if treatment==0 & est1==1 & e(sample)
sum mp_meses_activ if treatment==0 & est2==1 & e(sample)
sum mp_meses_activ if treatment==0 & est3==1 & e(sample)
sum mp_meses_activ if treatment==0 & est4==1 & e(sample)


test trabybebe=notrabybebe=trabynobebe=notrabynobebe

areg mp_meses_activ tiene_bebe treatment LB_jefa_hog LB_edu_anos LB_educ_i LB_cant_nino LB_edad_post if seg==1, cl(comuna_esc) absorb(est_rbd)
test tiene_bebe==0

******************************************
****************Employment****************
******************************************

areg trabaja_unmes_mp trabybebe notrabybebe trabynobebe notrabynobebe LB_jefa_hog LB_edu_anos LB_educ_i LB_cant_nino LB_edad_post if seg==1, cl(comuna_esc) absorb(est_rbd)
sum trabaja_unmes_mp if treatment==0 & est1==1 & e(sample)
sum trabaja_unmes_mp if treatment==0 & est2==1 & e(sample)
sum trabaja_unmes_mp if treatment==0 & est3==1 & e(sample)
sum trabaja_unmes_mp if treatment==0 & est4==1 & e(sample)



test trabybebe=notrabybebe=trabynobebe=notrabynobebe

areg trabaja_unmes_mp tiene_bebe treatment LB_jefa_hog LB_edu_anos LB_educ_i LB_cant_nino LB_edad_post if seg==1, cl(comuna_esc) absorb(est_rbd)
test tiene_bebe==0

*****************


areg trabajasiempre_mp trabybebe notrabybebe trabynobebe notrabynobebe LB_jefa_hog LB_edu_anos LB_educ_i LB_cant_nino LB_edad_post if seg==1, cl(comuna_esc) absorb(est_rbd)
sum trabajasiempre_mp if treatment==0 & est1==1 & e(sample)
sum trabajasiempre_mp if treatment==0 & est2==1 & e(sample)
sum trabajasiempre_mp if treatment==0 & est3==1 & e(sample)
sum trabajasiempre_mp if treatment==0 & est4==1 & e(sample)



test trabybebe=notrabybebe=trabynobebe=notrabynobebe

areg trabajasiempre_mp tiene_bebe treatment LB_jefa_hog LB_edu_anos LB_educ_i LB_cant_nino LB_edad_post if seg==1, cl(comuna_esc) absorb(est_rbd)
test tiene_bebe==0

*****************

areg mes_trabaja trabybebe notrabybebe trabynobebe notrabynobebe LB_jefa_hog LB_edu_anos LB_educ_i LB_cant_nino LB_edad_post if seg==1, cl(comuna_esc) absorb(est_rbd)
sum mes_trabaja if treatment==0 & est1==1 & e(sample)
sum mes_trabaja if treatment==0 & est2==1 & e(sample)
sum mes_trabaja if treatment==0 & est3==1 & e(sample)
sum mes_trabaja if treatment==0 & est4==1 & e(sample)



test trabybebe=notrabybebe=trabynobebe=notrabynobebe

areg mes_trabaja tiene_bebe treatment LB_jefa_hog LB_edu_anos LB_educ_i LB_cant_nino LB_edad_post if seg==1, cl(comuna_esc) absorb(est_rbd)
test tiene_bebe==0





