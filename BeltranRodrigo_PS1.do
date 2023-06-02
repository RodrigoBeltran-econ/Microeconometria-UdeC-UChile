*** PS1 ***
*** Rodrigo Beltrán Moreira ***
*** *** ***


*** Todo análisis se realizará en el informe ***



************************************
*********Programación (Parte 2)**********
************************************
clear all
global main "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS1"
cd "$main"
set seed 2

*Planteamos las 1000 observaciones
set obs 1000

***************************************************************************
***************************************************************************
***************************************************************************
**** (1) ****
***************************************************************************
***************************************************************************
***************************************************************************

*Generamos los parámetros
gen alfa = -3
gen b1 = 1.7
gen b2 = 4
gen b3 = 2

**Creando las variables
gen x1 = runiform(2,5)
gen x3 = rnormal(3,6)

**Generamos una variable aleatoria para hacer la variable x2 aleatoria en los % de distribución
gen random = runiform()
gen x2 = 0
replace x2 = 1 if random<0.15
replace x2 = 2 if inrange(random,0.15,0.4) 
replace x2 = 3 if random>0.4


tab x2

**Generamos los residuos 

gen eps = rnormal(0,1)

*Observamos que los porcentajes de distribucion fueron bastante cercanos
*Procedemos a generar la variable dependiente

gen y = alfa + b1*x1 + b2*x2 + b3*x3 + eps

**Regresión
reg y x1 x2 x3, robust
*outreg2 using myreg.tex, replace ctitle(Modelo 1)
eststo reg1

***************************************************************************
***************************************************************************
***************************************************************************
**** (2) ****
***************************************************************************
***************************************************************************
***************************************************************************

reg y x2 x3, robust
*outreg2 using myreg.tex, append ctitle(Modelo 2)
eststo reg2

**Se elimina x1

***************************************************************************
***************************************************************************
***************************************************************************
*** (3) ***
***************************************************************************
***************************************************************************
***************************************************************************

clear all
set seed 2


**Creando el loop por medio de un MonteCarlo **

**Primero para las 100 simulaciones

postfile buffer beta2 beta2b using reg100, replace

forvalues i=1/100 {
         drop _all
         set obs 1000
		 gen alfa = -3
		 gen b1 = 1.7
		 gen b2 = 4
		 gen b3 = 2
		 gen x1 = runiform(2,5)
		 gen x3 = rnormal(3,6)
		 gen random = runiform()
	     gen x2 = 0
		 replace x2 = 1 if random<0.15
		 replace x2 = 2 if inrange(random,0.15,0.4) 
		 replace x2 = 3 if random>0.4
		 gen eps = rnormal(0,1)
		 gen y = alfa + b1*x1 + b2*x2 + b3*x3 + eps
		 reg y x1 x2 x3, robust

		 scalar beta2 = _b[x2] //se recupera beta 2 en cada loop
		 
		 reg y x2 x3, robust

		 scalar beta2b = _b[x2] //se recupera beta 2 en cada loop

         post buffer (beta2) (beta2b)
 }

postclose buffer
use reg100, clear

histogram beta2, kdensity scheme(s2color) title("Estimación Beta2 usando x1") plotregion(style(none)) note("Source: Own elaboration.",) norm name(graf1)

histogram beta2b, kdensity scheme(s2color) title("Estimación Beta2 sin x1") plotregion(style(none)) note("Source: Own elaboration.",) norm name(graf2)

graph combine graf1 graf2



***************************************************************************
***************************************************************************
***************************************************************************
*** (3 y 4) ***
***************************************************************************
***************************************************************************
***************************************************************************

*Se obtiene la distribución de beta_2 y además la estadística descriptiva para la pregunta 4

clear all
global main "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS1"
cd "$main"
set seed 2


**Creando el loop por medio de un MonteCarlo **

**Para 500 simulaciones

postfile buffer beta1 se_x1 t_x1 pvalue1 beta2 se_x2 t_x2 pvalue2 beta3 se_x3 t_x3 pvalue3 alfa0 se_alfa0 t_alfa0 pvalue4 beta2b se_x2b t_x2b pvalue2b beta3b se_x3b t_x3b pvalue3b  alfa0b se_alfa0b t_alfa0b pvalue4b using reg500, replace

forvalues i=1/500 {
         drop _all
         set obs 1000
		 gen alfa = -3
		 gen b1 = 1.7
		 gen b2 = 4
		 gen b3 = 2
		 gen x1 = runiform(2,5)
		 gen x3 = rnormal(3,6)
		 gen random = runiform()
	     gen x2 = 0
		 replace x2 = 1 if random<0.15
		 replace x2 = 2 if inrange(random,0.15,0.4) 
		 replace x2 = 3 if random>0.4
		 gen eps = rnormal(0,1)
		 gen y = alfa + b1*x1 + b2*x2 + b3*x3 + eps
		 reg y x1 x2 x3, robust

		 scalar beta1 = _b[x1] //se recupera beta 1 en cada loop
		 scalar beta2 = _b[x2] //se recupera beta 2 en cada loop
		 scalar beta3 = _b[x3] //se recupera beta 3 en cada loop
		 scalar alfa0 = _b[_cons] //se recupera alfa
		 matrix sim = r(table) //se extrae matriz para recuperar se, est t, pvalue
		 scalar se_x1 = sim[2,1]
		 scalar se_x2 = sim[2,2]
		 scalar se_x3 = sim[2,3]
		 scalar se_alfa0 = sim[2,4]
		 scalar t_x1 = sim[3,1]
		 scalar t_x2 = sim[3,2]
		 scalar t_x3 = sim[3,3]
		 scalar t_alfa0 = sim[3,4]
		 scalar pvalue1 = sim[4,1]
		 scalar pvalue2 = sim[4,2]
		 scalar pvalue3 = sim[4,3]
		 scalar pvalue4 = sim[4,4]
		 
		 reg y x2 x3, robust

		 scalar beta2b = _b[x2] //se recupera beta 2 en cada loop
		 scalar beta3b = _b[x3] //se recupera beta 3 en cada loop
		 scalar alfa0b = _b[_cons] //se recupera alfa
		 matrix simb = r(table)
		 scalar se_x2b = simb[2,1]
		 scalar se_x3b = simb[2,2]
		 scalar se_alfa0b = simb[2,3]
		 scalar t_x2b = simb[3,1]
		 scalar t_x3b = simb[3,2]
		 scalar t_alfa0b = simb[3,3]
		 scalar pvalue2b = simb[4,1]
		 scalar pvalue3b = simb[4,2]
		 scalar pvalue4b = simb[4,3]
		 
         post buffer (beta1) (se_x1) (t_x1) (pvalue1) (beta2) (se_x2) (t_x2) (pvalue2) (beta3) (se_x3) (t_x3) (pvalue3) (alfa0) (se_alfa0) (t_alfa0) (pvalue4) (beta2b) (se_x2b) (t_x2b) (pvalue2b) (beta3b) (se_x3b) (t_x3b) (pvalue3b) (alfa0b) (se_alfa0b) (t_alfa0b) (pvalue4b)
 }

postclose buffer
use reg500, clear

histogram beta2, kdensity scheme(s2color) title("Estimación Beta2 usando x1") plotregion(style(none)) note("Source: Own elaboration.",) norm name(graf1)

histogram beta2b, kdensity scheme(s2color) title("Estimación Beta2 sin x1") plotregion(style(none)) note("Source: Own elaboration.",) norm name(graf2)

graph combine graf1 graf2


**Obteniendo una estadística descriptiva de todos los valores obtenidos
sum
*outreg2 using desc11.tex, replace sum(log) eqkeep(N mean)


***************************************************************************
***************************************************************************
***************************************************************************
*** (5) ***
***************************************************************************
***************************************************************************
***************************************************************************


***Eliminación del 10% de x1 y 15% de x2

**Creando el loop por medio de un MonteCarlo **
clear all
global main "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS1"
cd "$main"
set seed 2
**Para 500 simulaciones

postfile buffer beta1 se_x1 t_x1 pvalue1 beta2 se_x2 t_x2 pvalue2 beta3 se_x3 t_x3 pvalue3 alfa0 se_alfa0 t_alfa0 pvalue4 beta2b se_x2b t_x2b pvalue2b beta3b se_x3b t_x3b pvalue3b  alfa0b se_alfa0b t_alfa0b pvalue4b using reg500_1, replace

forvalues i=1/500 {
         drop _all
         set obs 1000
		 gen alfa = -3
		 gen b1 = 1.7
		 gen b2 = 4
		 gen b3 = 2
		 gen x1 = runiform(2,5)
		 gen x3 = rnormal(3,6)
		 gen random = runiform()
	     gen x2 = 0
		 replace x2 = 1 if random<0.15
		 replace x2 = 2 if inrange(random,0.15,0.4) 
		 replace x2 = 3 if random>0.4
		 gen eps = rnormal(0,1)
		 *Genero la variable dependiente
		 gen y = alfa + b1*x1 + b2*x2 + b3*x3 + eps

		 gen x1_1 = rbinomial(1, 0.1) //eliminacion aleatoria en cada loop
		 replace x1=. if x1_1==1

		 gen x2_2 = rbinomial(1, 0.15)
		 replace x2=. if x2_2==1
		 
		 
		 
		 reg y x1 x2 x3, robust

		 scalar beta1 = _b[x1] //se recupera beta 1 en cada loop
		 scalar beta2 = _b[x2] //se recupera beta 2 en cada loop
		 scalar beta3 = _b[x3] //se recupera beta 3 en cada loop
		 scalar alfa0 = _b[_cons] //se recupera alfa
		 matrix sim = r(table) //se extrae matriz para recuperar se, est t, pvalue
		 scalar se_x1 = sim[2,1]
		 scalar se_x2 = sim[2,2]
		 scalar se_x3 = sim[2,3]
		 scalar se_alfa0 = sim[2,4]
		 scalar t_x1 = sim[3,1]
		 scalar t_x2 = sim[3,2]
		 scalar t_x3 = sim[3,3]
		 scalar t_alfa0 = sim[3,4]
		 scalar pvalue1 = sim[4,1]
		 scalar pvalue2 = sim[4,2]
		 scalar pvalue3 = sim[4,3]
		 scalar pvalue4 = sim[4,4]
		 
		 reg y x2 x3, robust

		 scalar beta2b = _b[x2] //se recupera beta 2 en cada loop
		 scalar beta3b = _b[x3] //se recupera beta 3 en cada loop
		 scalar alfa0b = _b[_cons] //se recupera alfa
		 matrix simb = r(table)
		 scalar se_x2b = simb[2,1]
		 scalar se_x3b = simb[2,2]
		 scalar se_alfa0b = simb[2,3]
		 scalar t_x2b = simb[3,1]
		 scalar t_x3b = simb[3,2]
		 scalar t_alfa0b = simb[3,3]
		 scalar pvalue2b = simb[4,1]
		 scalar pvalue3b = simb[4,2]
		 scalar pvalue4b = simb[4,3]
		 
         post buffer (beta1) (se_x1) (t_x1) (pvalue1) (beta2) (se_x2) (t_x2) (pvalue2) (beta3) (se_x3) (t_x3) (pvalue3) (alfa0) (se_alfa0) (t_alfa0) (pvalue4) (beta2b) (se_x2b) (t_x2b) (pvalue2b) (beta3b) (se_x3b) (t_x3b) (pvalue3b) (alfa0b) (se_alfa0b) (t_alfa0b) (pvalue4b)
 }

postclose buffer
use reg500_1, clear

**Estadística descriptiva
sum

*outreg2 using desc11.tex, append sum(log) eqkeep(N mean)




***************************************************************************
***************************************************************************
***************************************************************************
*** (6) ***
***************************************************************************
***************************************************************************
***************************************************************************

*Se elimina quintil inferior de x1 y el decil superior de x2


clear all
global main "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS1"
cd "$main"
set seed 2
**Para 500 simulaciones

postfile buffer beta1 se_x1 t_x1 pvalue1 beta2 se_x2 t_x2 pvalue2 beta3 se_x3 t_x3 pvalue3 alfa0 se_alfa0 t_alfa0 pvalue4 beta2b se_x2b t_x2b pvalue2b beta3b se_x3b t_x3b pvalue3b  alfa0b se_alfa0b t_alfa0b pvalue4b using reg500_2, replace

forvalues i=1/500 {
         drop _all
         set obs 1000
		 gen alfa = -3
		 gen b1 = 1.7
		 gen b2 = 4
		 gen b3 = 2
		 gen x1 = runiform(2,5)
		 gen x3 = rnormal(3,6)
		 gen random = runiform()
	     gen x2 = 0
		 replace x2 = 1 if random<0.15
		 replace x2 = 2 if inrange(random,0.15,0.4) 
		 replace x2 = 3 if random>0.4
		 gen eps = rnormal(0,1)
		 
		 *Genero la variable dependiente
		 gen y = alfa + b1*x1 + b2*x2 + b3*x3 + eps
		 
		 **Se tiene que eliminar las observaciones que esten en el quintil inferior de x1 y las que esten en el decil superior de x2

		 gsort x1
		 replace x1=. in 1/200
		 gsort -x2
		 replace x2=. in 1/100
		 
		 
		 
		 
		 reg y x1 x2 x3, robust

		 scalar beta1 = _b[x1] //se recupera beta 1 en cada loop
		 scalar beta2 = _b[x2] //se recupera beta 2 en cada loop
		 scalar beta3 = _b[x3] //se recupera beta 3 en cada loop
		 scalar alfa0 = _b[_cons] //se recupera alfa
		 matrix sim = r(table) //se extrae matriz para recuperar se, est t, pvalue
		 scalar se_x1 = sim[2,1]
		 scalar se_x2 = sim[2,2]
		 scalar se_x3 = sim[2,3]
		 scalar se_alfa0 = sim[2,4]
		 scalar t_x1 = sim[3,1]
		 scalar t_x2 = sim[3,2]
		 scalar t_x3 = sim[3,3]
		 scalar t_alfa0 = sim[3,4]
		 scalar pvalue1 = sim[4,1]
		 scalar pvalue2 = sim[4,2]
		 scalar pvalue3 = sim[4,3]
		 scalar pvalue4 = sim[4,4]
		 
		 reg y x2 x3, robust

		 scalar beta2b = _b[x2] //se recupera beta 2 en cada loop
		 scalar beta3b = _b[x3] //se recupera beta 3 en cada loop
		 scalar alfa0b = _b[_cons] //se recupera alfa
		 matrix simb = r(table)
		 scalar se_x2b = simb[2,1]
		 scalar se_x3b = simb[2,2]
		 scalar se_alfa0b = simb[2,3]
		 scalar t_x2b = simb[3,1]
		 scalar t_x3b = simb[3,2]
		 scalar t_alfa0b = simb[3,3]
		 scalar pvalue2b = simb[4,1]
		 scalar pvalue3b = simb[4,2]
		 scalar pvalue4b = simb[4,3]
		 
         post buffer (beta1) (se_x1) (t_x1) (pvalue1) (beta2) (se_x2) (t_x2) (pvalue2) (beta3) (se_x3) (t_x3) (pvalue3) (alfa0) (se_alfa0) (t_alfa0) (pvalue4) (beta2b) (se_x2b) (t_x2b) (pvalue2b) (beta3b) (se_x3b) (t_x3b) (pvalue3b) (alfa0b) (se_alfa0b) (t_alfa0b) (pvalue4b)
 }

postclose buffer
use reg500_2, clear

**Estadística descriptiva
sum
*outreg2 using desc11.tex, append sum(log) eqkeep(N mean)








***************************************************************************
***************************************************************************
***************************************************************************
*** (7) ***
***************************************************************************
***************************************************************************
***************************************************************************

****IMPUTACION DE DATOS****
***NOTA PARA LOS AYUDANTES: La simulación corre bien, pero toma aprox 10 minutos para correr las 500 simulaciones por si acaso!

**Imputacion tipo MICE, detallando Logistica para variable categorica


clear all
global main "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS1"
cd "$main"
set seed 2
**Para 500 simulaciones

postfile buffer beta1 se_x1 t_x1 pvalue1 beta2 se_x2 t_x2 pvalue2 beta3 se_x3 t_x3 pvalue3 alfa0 se_alfa0 t_alfa0 pvalue4 beta2b se_x2b t_x2b pvalue2b beta3b se_x3b t_x3b pvalue3b  alfa0b se_alfa0b t_alfa0b pvalue4b using reg500_3, replace

forvalues i=1/500 {
         drop _all
         set obs 1000
		 gen alfa = -3
		 gen b1 = 1.7
		 gen b2 = 4
		 gen b3 = 2
		 gen x1 = runiform(2,5)
		 gen x3 = rnormal(3,6)
		 gen random = runiform()
	     gen x2 = 0
		 replace x2 = 1 if random<0.15
		 replace x2 = 2 if inrange(random,0.15,0.4) 
		 replace x2 = 3 if random>0.4
		 gen eps = rnormal(0,1)
		 		 
		 *Genero la variable dependiente
		 gen y = alfa + b1*x1 + b2*x2 + b3*x3 + eps
		 
		 **Se tiene que eliminar las observaciones que esten en el quintil inferior de x1 y las que esten en el decil superior de x2
		 gsort x1
		 replace x1=. in 1/200
		 gsort -x2
		 replace x2=. in 1/100
		 
		 **Realizando imputación tipo MICE
		 mi set mlong
		 mi register imputed x1 x2 
		 mi impute chained (regress) x1 (mlogit) x2, add(5)

		 
		 mi estimate: reg y x1 x2 x3, robust
		 
		 matrix sim = r(table) //se extrae matriz para recuperar betas, se, est t, pvalue
		 scalar beta1 = sim[1,1] //se recupera beta 1 en cada loop
		 scalar beta2 = sim[1,2] //se recupera beta 2 en cada loop
		 scalar beta3 = sim[1,3] //se recupera beta 3 en cada loop
		 scalar alfa0 = sim[1,4] //se recupera alfa
		 scalar se_x1 = sim[2,1]
		 scalar se_x2 = sim[2,2]
		 scalar se_x3 = sim[2,3]
		 scalar se_alfa0 = sim[2,4]
		 scalar t_x1 = sim[3,1]
		 scalar t_x2 = sim[3,2]
		 scalar t_x3 = sim[3,3]
		 scalar t_alfa0 = sim[3,4]
		 scalar pvalue1 = sim[4,1]
		 scalar pvalue2 = sim[4,2]
		 scalar pvalue3 = sim[4,3]
		 scalar pvalue4 = sim[4,4]
		 
		 mi estimate: reg y x2 x3, robust
		 
		 matrix simb = r(table)
		 scalar beta2b = simb[1,1] //se recupera beta 2 en cada loop
		 scalar beta3b = simb[1,2] //se recupera beta 3 en cada loop
		 scalar alfa0b = simb[1,3] //se recupera alfa
		 scalar se_x2b = simb[2,1]
		 scalar se_x3b = simb[2,2]
		 scalar se_alfa0b = simb[2,3]
		 scalar t_x2b = simb[3,1]
		 scalar t_x3b = simb[3,2]
		 scalar t_alfa0b = sim[b3,3]
		 scalar pvalue2b = simb[4,1]
		 scalar pvalue3b = simb[4,2]
		 scalar pvalue4b = simb[4,3]
		 
         post buffer (beta1) (se_x1) (t_x1) (pvalue1) (beta2) (se_x2) (t_x2) (pvalue2) (beta3) (se_x3) (t_x3) (pvalue3) (alfa0) (se_alfa0) (t_alfa0) (pvalue4) (beta2b) (se_x2b) (t_x2b) (pvalue2b) (beta3b) (se_x3b) (t_x3b) (pvalue3b) (alfa0b) (se_alfa0b) (t_alfa0b) (pvalue4b)
 }

postclose buffer
use reg500_3, clear

**Estadística descriptiva
sum
*outreg2 using desc11.tex, replace sum(log) eqkeep(N mean)









**************************************************************************
*********Empírico (Parte 3)***********************************************
**************************************************************************
clear all

use "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS1/data_analysis.dta"

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





