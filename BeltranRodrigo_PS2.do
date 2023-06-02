*** PS2 ***
*** Rodrigo Beltrán Moreira ***
*** *** ***


*** Todo análisis se realizará en el informe ***



**************************************************************************************
***************************Programación (Parte 2)*************************************
**************************************************************************************

**NOTA: EN EL .ZIP INCLUÍ LA BASE DE DATOS nlswork, esto para apurar los loops en las preguntas siguientes!!!!!

******Modelo de Probabilidad Lineal******

clear all
global main "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS2"
cd "$main"

webuse nlswork
gen fulltime=0
replace fulltime=1 if hours>=40


**Pregunta 1

***MCO

regress fulltime age ttl_exp nev_mar

*Efectos marginales de un MCO es dy/dx, con lo que el efecto marginal es el beta estimado directamente
display " Efecto Marginal age = " _b[age]
display " Efecto Marginal ttl_exp = " _b[ttl_exp]
display " Efecto Marginal nev_mar = " _b[nev_mar]

***Probit
probit fulltime age ttl_exp nev_mar

margins, dydx(*) atmeans //se usa margins para ver que tan aproximado es el valor que se estima


*Efecto marginal age
predict xb if e(sample), xb
gen index = normalden(xb)
gen dydage_probit = index * _b[age]

*Efecto marginal exp
gen dydexp_probit = index * _b[ttl_exp]

*Efecto marginal nev_mar
gen dydnevmar_probit = index * _b[nev_mar]

sum dydage_probit dydexp_probit dydnevmar_probit






***Logit
logit fulltime age ttl_exp nev_mar
margins, dydx(*) atmeans 



summarize age
local ag=r(mean)
summarize ttl_exp
local exp=r(mean)
summarize nev_mar
local mar=r(mean)

predict xb1, xb
gen my_dfdxb= exp(xb1)*(1+exp(xb1))^(-2)

display " Efecto Marginal age = " _b[age]*my_dfdxb
display " Efecto Marginal ttl_exp = " _b[ttl_exp]*my_dfdxb
display " Efecto Marginal nev_mar = " _b[nev_mar]*my_dfdxb






**************
**Pregunta 2**
**************

clear all
set seed 1234

global main "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS2"
cd "$main"

postfile buffer betar_age betar_exp betar_marr betap_age betap_exp betap_marr pmarg_age pmarg_exp pmarg_marr betal_age betal_exp betal_marr lmarg_age lmarg_exp lmarg_marr using prob50, replace

forvalues i=1/50 {
	drop _all
	use "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS2/nlswork.dta"
	gen fulltime=0
	replace fulltime=1 if hours>=40
	sample 1000, count
	regress fulltime age ttl_exp nev_mar //los efectos marginales son el mismo beta de la regresión
	scalar betar_age = _b[age]
	scalar betar_exp = _b[ttl_exp]
	scalar betar_marr = _b[nev_mar]
	
	probit fulltime age ttl_exp nev_mar //modelo probit
	scalar betap_age = _b[age]
	scalar betap_exp = _b[ttl_exp]
	scalar betap_marr = _b[nev_mar]
	
	margins, dydx(*) atmeans // efectos marginales
	matrix margprob = r(b)
	scalar pmarg_age = margprob[1,1]
	scalar pmarg_exp = margprob[1,2]
	scalar pmarg_marr = margprob[1,3]
	
	logit fulltime age ttl_exp nev_mar // modelo logit
	scalar betal_age = _b[age]
	scalar betal_exp = _b[ttl_exp]
	scalar betal_marr = _b[nev_mar]
	
	margins, dydx(*) atmeans // efectos marginales
	matrix margprobl = r(b)
	scalar lmarg_age = margprobl[1,1]
	scalar lmarg_exp = margprobl[1,2]
	scalar lmarg_marr = margprobl[1,3]
	
	post buffer (betar_age) (betar_exp) (betar_marr) (betap_age) (betap_exp) (betap_marr) (pmarg_age) (pmarg_exp) (pmarg_marr) (betal_age) (betal_exp) (betal_marr) (lmarg_age) (lmarg_exp) (lmarg_marr)
}

postclose buffer
use prob50, clear


****Grafico de los betas

histogram betar_age, kdensity scheme(s2color) title("MCO ''age''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta age") norm name(graf1bage)

histogram betap_age, kdensity scheme(s2color) title("Probit ''age''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta age") norm name(graf2bage)

histogram betal_age, kdensity scheme(s2color) title("Logit ''age''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta age") norm name(graf3bage)

graph combine graf1bage graf2bage graf3bage, col(3)


histogram betar_exp, kdensity scheme(s2color) title("MCO ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta exp") norm name(graf1bexp)

histogram betap_exp, kdensity scheme(s2color) title("Probit ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta exp") norm name(graf2bexp)

histogram betal_exp, kdensity scheme(s2color) title("Logit ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta exp") norm name(graf3bexp)

graph combine graf1bexp graf2bexp graf3bexp, col(3)


histogram betar_marr, kdensity scheme(s2color) title("MCO ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta marr") norm name(graf1bmarr)

histogram betap_marr, kdensity scheme(s2color) title("Probit ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta marr") norm name(graf2bmarr)

histogram betal_marr, kdensity scheme(s2color) title("Logit ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta marr") norm name(graf3bmarr)

graph combine graf1bmarr graf2bmarr graf3bmarr, col(3)




****Graficos de los efectos marginales

histogram betar_age, kdensity scheme(s2color) title("MCO ''age''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta age") norm name(graf1marage)

histogram pmarg_age, kdensity scheme(s2color) title("Probit ''age''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta age") norm name(graf2marage)

histogram lmarg_age, kdensity scheme(s2color) title("Logit''age''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta age") norm name(graf3marage)

graph combine graf1marage graf2marage graf3marage, col(3)

histogram betar_exp, kdensity scheme(s2color) title("MCO ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta exp") norm name(graf1margexp)

histogram pmarg_exp, kdensity scheme(s2color) title("Probit ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta exp") norm name(graf2margexp)

histogram lmarg_exp, kdensity scheme(s2color) title("Logit ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta exp") norm name(graf3margexp)

graph combine graf1margexp graf2margexp graf3margexp, col(3)

histogram betar_marr, kdensity scheme(s2color) title("MCO ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta marr") norm name(graf1margmarr)

histogram pmarg_marr, kdensity scheme(s2color) title("Probit ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta marr") norm name(graf2margmarr)

histogram lmarg_marr, kdensity scheme(s2color) title("Logit ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta marr") norm name(graf3margmarr)

graph combine graf1margmarr graf2margmarr graf3margmarr, col(3)








**Pregunta 3**

*Se pide 500 repeticiones


clear all
set seed 1234

global main "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS2"
cd "$main"

postfile buffer betar_age betar_exp betar_marr betap_age betap_exp betap_marr pmarg_age pmarg_exp pmarg_marr betal_age betal_exp betal_marr lmarg_age lmarg_exp lmarg_marr using prob500, replace

forvalues i=1/500 {
	drop _all
	use "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS2/nlswork.dta"
	gen fulltime=0
	replace fulltime=1 if hours>=40
	sample 1000, count
	regress fulltime age ttl_exp nev_mar //los efectos marginales son el mismo beta de la regresión
	scalar betar_age = _b[age]
	scalar betar_exp = _b[ttl_exp]
	scalar betar_marr = _b[nev_mar]
	
	probit fulltime age ttl_exp nev_mar //modelo probit
	scalar betap_age = _b[age]
	scalar betap_exp = _b[ttl_exp]
	scalar betap_marr = _b[nev_mar]
	
	margins, dydx(*) atmeans // efectos marginales
	matrix margprob = r(b)
	scalar pmarg_age = margprob[1,1]
	scalar pmarg_exp = margprob[1,2]
	scalar pmarg_marr = margprob[1,3]
	
	logit fulltime age ttl_exp nev_mar // modelo logit
	scalar betal_age = _b[age]
	scalar betal_exp = _b[ttl_exp]
	scalar betal_marr = _b[nev_mar]
	
	margins, dydx(*) atmeans // efectos marginales
	matrix margprobl = r(b)
	scalar lmarg_age = margprobl[1,1]
	scalar lmarg_exp = margprobl[1,2]
	scalar lmarg_marr = margprobl[1,3]
	
	post buffer (betar_age) (betar_exp) (betar_marr) (betap_age) (betap_exp) (betap_marr) (pmarg_age) (pmarg_exp) (pmarg_marr) (betal_age) (betal_exp) (betal_marr) (lmarg_age) (lmarg_exp) (lmarg_marr)
}

postclose buffer
use prob500, clear

****Grafico de los betas

histogram betar_age, kdensity scheme(s2color) title("MCO ''age''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta age") norm name(graf1bage)

histogram betap_age, kdensity scheme(s2color) title("Probit ''age''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta age") norm name(graf2bage)

histogram betal_age, kdensity scheme(s2color) title("Logit ''age''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta age") norm name(graf3bage)

graph combine graf1bage graf2bage graf3bage, col(3)


histogram betar_exp, kdensity scheme(s2color) title("MCO ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta exp") norm name(graf1bexp)

histogram betap_exp, kdensity scheme(s2color) title("Probit ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta exp") norm name(graf2bexp)

histogram betal_exp, kdensity scheme(s2color) title("Logit ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta exp") norm name(graf3bexp)

graph combine graf1bexp graf2bexp graf3bexp, col(3)


histogram betar_marr, kdensity scheme(s2color) title("MCO ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta marr") norm name(graf1bmarr)

histogram betap_marr, kdensity scheme(s2color) title("Probit ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta marr") norm name(graf2bmarr)

histogram betal_marr, kdensity scheme(s2color) title("Logit ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta marr") norm name(graf3bmarr)

graph combine graf1bmarr graf2bmarr graf3bmarr, col(3)




****Graficos de los efectos marginales

histogram betar_age, kdensity scheme(s2color) title("MCO ''age''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta age") norm name(graf1marage)

histogram pmarg_age, kdensity scheme(s2color) title("Probit ''age''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta age") norm name(graf2marage)

histogram lmarg_age, kdensity scheme(s2color) title("Logit''age''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta age") norm name(graf3marage)

graph combine graf1marage graf2marage graf3marage, col(3)

histogram betar_exp, kdensity scheme(s2color) title("MCO ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta exp") norm name(graf1margexp)

histogram pmarg_exp, kdensity scheme(s2color) title("Probit ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta exp") norm name(graf2margexp)

histogram lmarg_exp, kdensity scheme(s2color) title("Logit ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta exp") norm name(graf3margexp)

graph combine graf1margexp graf2margexp graf3margexp, col(3)

histogram betar_marr, kdensity scheme(s2color) title("MCO ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta marr") norm name(graf1margmarr)

histogram pmarg_marr, kdensity scheme(s2color) title("Probit ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta marr") norm name(graf2margmarr)

histogram lmarg_marr, kdensity scheme(s2color) title("Logit ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta marr") norm name(graf3margmarr)

graph combine graf1margmarr graf2margmarr graf3margmarr, col(3)





**Pregunta 4**
**Remuestreo con reemplazo**


clear all
set seed 1234
global main "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS2"
cd "$main"
use "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS2/nlswork.dta"
gen fulltime=0
replace fulltime=1 if hours>=40


postfile buffer betar_age betar_exp betar_marr betap_age betap_exp betap_marr pmarg_age pmarg_exp pmarg_marr betal_age betal_exp betal_marr lmarg_age lmarg_exp lmarg_marr using prob50, replace

forvalues i=1/50 {
	bsample 1000
	regress fulltime age ttl_exp nev_mar //los efectos marginales son el mismo beta de la regresión
	scalar betar_age = _b[age]
	scalar betar_exp = _b[ttl_exp]
	scalar betar_marr = _b[nev_mar]
	
	probit fulltime age ttl_exp nev_mar //modelo probit
	scalar betap_age = _b[age]
	scalar betap_exp = _b[ttl_exp]
	scalar betap_marr = _b[nev_mar]
	
	margins, dydx(*) atmeans // efectos marginales
	matrix margprob = r(b)
	scalar pmarg_age = margprob[1,1]
	scalar pmarg_exp = margprob[1,2]
	scalar pmarg_marr = margprob[1,3]
	
	logit fulltime age ttl_exp nev_mar // modelo logit
	scalar betal_age = _b[age]
	scalar betal_exp = _b[ttl_exp]
	scalar betal_marr = _b[nev_mar]
	
	margins, dydx(*) atmeans // efectos marginales
	matrix margprobl = r(b)
	scalar lmarg_age = margprobl[1,1]
	scalar lmarg_exp = margprobl[1,2]
	scalar lmarg_marr = margprobl[1,3]
	
	post buffer (betar_age) (betar_exp) (betar_marr) (betap_age) (betap_exp) (betap_marr) (pmarg_age) (pmarg_exp) (pmarg_marr) (betal_age) (betal_exp) (betal_marr) (lmarg_age) (lmarg_exp) (lmarg_marr)
}

postclose buffer
use prob50, clear

****Grafico de los betas

histogram betar_age, kdensity scheme(s2color) title("MCO ''age''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta age") norm name(graf1bage)

histogram betap_age, kdensity scheme(s2color) title("Probit ''age''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta age") norm name(graf2bage)

histogram betal_age, kdensity scheme(s2color) title("Logit ''age''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta age") norm name(graf3bage)




histogram betar_exp, kdensity scheme(s2color) title("MCO ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta exp") norm name(graf1bexp)

histogram betap_exp, kdensity scheme(s2color) title("Probit ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta exp") norm name(graf2bexp)

histogram betal_exp, kdensity scheme(s2color) title("Logit ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta exp") norm name(graf3bexp)



histogram betar_marr, kdensity scheme(s2color) title("MCO ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta marr") norm name(graf1bmarr)

histogram betap_marr, kdensity scheme(s2color) title("Probit ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta marr") norm name(graf2bmarr)

histogram betal_marr, kdensity scheme(s2color) title("Logit ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta marr") norm name(graf3bmarr)

graph combine graf1bage graf2bage graf3bage graf1bexp graf2bexp graf3bexp graf1bmarr graf2bmarr graf3bmarr, col(3)




****Graficos de los efectos marginales

histogram betar_age, kdensity scheme(s2color) title("MCO ''age''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta age") norm name(graf1marage)

histogram pmarg_age, kdensity scheme(s2color) title("Probit ''age''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta age") norm name(graf2marage)

histogram lmarg_age, kdensity scheme(s2color) title("Logit''age''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta age") norm name(graf3marage)


histogram betar_exp, kdensity scheme(s2color) title("MCO ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta exp") norm name(graf1margexp)

histogram pmarg_exp, kdensity scheme(s2color) title("Probit ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta exp") norm name(graf2margexp)

histogram lmarg_exp, kdensity scheme(s2color) title("Logit ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta exp") norm name(graf3margexp)



histogram betar_marr, kdensity scheme(s2color) title("MCO ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta marr") norm name(graf1margmarr)

histogram pmarg_marr, kdensity scheme(s2color) title("Probit ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta marr") norm name(graf2margmarr)

histogram lmarg_marr, kdensity scheme(s2color) title("Logit ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("Beta marr") norm name(graf3margmarr)

graph combine graf1marage graf2marage graf3marage graf1margexp graf2margexp graf3margexp graf1margmarr graf2margmarr graf3margmarr, col(3)










**Pregunta 5**
clear all
set seed 1234

global main "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS2"
cd "$main"
use "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS2/nlswork.dta"
gen fulltime=0
replace fulltime=1 if hours>=40
gen region = 0


set level 90

**MCO
regress fulltime age ttl_exp nev_mar 
regress fulltime age ttl_exp nev_mar, r
regress fulltime age ttl_exp nev_mar, vce(cluster ind_code)


*ssc install boottest, replace
regress fulltime age ttl_exp nev_mar, vce(cluster south)
boottest age, nogr
boottest ttl_exp, nogr
boottest nev_mar, nogr

**Probit
probit fulltime age ttl_exp nev_mar 
margins, dydx(*) atmeans
probit fulltime age ttl_exp nev_mar, r
margins, dydx(*) atmeans
probit fulltime age ttl_exp nev_mar, vce(cluster ind_code)
margins, dydx(*) atmeans

probit fulltime age ttl_exp nev_mar, vce(cluster south)
margins, dydx(*) atmeans
boottest age, nogr
boottest ttl_exp, nogr
boottest nev_mar, nogr


**Logit
logit fulltime age ttl_exp nev_mar 
margins, dydx(*) atmeans
logit fulltime age ttl_exp nev_mar, r
margins, dydx(*) atmeans
logit fulltime age ttl_exp nev_mar, vce(cluster ind_code)
margins, dydx(*) atmeans

probit fulltime age ttl_exp nev_mar, vce(cluster south)
boottest age, nogr
boottest ttl_exp, nogr
boottest nev_mar, nogr



***Pregunta 6
**Para MCO

clear all
set seed 1234
set level 90

global main "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS2"
cd "$main"

postfile buffer se_age se_exp se_mar se_ager se_expr se_marr se_agec se_expc se_marc using prob500, replace

forvalues i=1/500 {
	drop _all
	use "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS2/nlswork.dta"
	gen fulltime=0
	replace fulltime=1 if hours>=40
	sample 1000, count
	regress fulltime age ttl_exp nev_mar 
	scalar se_age = _se[age]
	scalar se_exp = _se[ttl_exp]
	scalar se_mar = _se[nev_mar]

	regress fulltime age ttl_exp nev_mar, r
	scalar se_ager = _se[age]
	scalar se_expr = _se[ttl_exp]
	scalar se_marr = _se[nev_mar]
	
	regress fulltime age ttl_exp nev_mar, vce(cluster ind_code)
	scalar se_agec = _se[age]
	scalar se_expc = _se[ttl_exp]
	scalar se_marc = _se[nev_mar]
	

	post buffer (se_age) (se_exp) (se_mar) (se_ager) (se_expr) (se_marr) (se_agec) (se_expc) (se_marc)
}

postclose buffer
use prob500, clear



****Graficos de los errores

histogram se_age, kdensity scheme(s2color) title("MCO ''age''") plotregion(style(none)) ytitle("Densidad") xtitle("SE age") norm name(graf1marage)

histogram se_ager, kdensity scheme(s2color) title("Robusto ''age''") plotregion(style(none)) ytitle("Densidad") xtitle("SE age") norm name(graf2marage)

histogram se_agec, kdensity scheme(s2color) title("Cluster''age''") plotregion(style(none)) ytitle("Densidad") xtitle("SE age") norm name(graf3marage)


histogram se_exp, kdensity scheme(s2color) title("MCO ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("SE exp") norm name(graf1margexp)

histogram se_expr, kdensity scheme(s2color) title("Robusto ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("SE exp") norm name(graf2margexp)

histogram se_expc, kdensity scheme(s2color) title("Cluster ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("SE exp") norm name(graf3margexp)


histogram se_mar, kdensity scheme(s2color) title("MCO ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("SE marr") norm name(graf1margmarr)

histogram se_marr, kdensity scheme(s2color) title("Robusto ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("SE marr") norm name(graf2margmarr)

histogram se_marc, kdensity scheme(s2color) title("Cluster ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("SE marr") norm name(graf3margmarr)

graph combine graf1marage graf2marage graf3marage graf1margexp graf2margexp graf3margexp graf1margmarr graf2margmarr graf3margmarr, col(3)



****Para Probit

clear all
set seed 1234
set level 90

global main "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS2"
cd "$main"

postfile buffer se_age se_exp se_mar se_ager se_expr se_marr se_agec se_expc se_marc using prob500, replace

forvalues i=1/500 {
	drop _all
	use "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS2/nlswork.dta"
	gen fulltime=0
	replace fulltime=1 if hours>=40
	sample 1000, count
	probit fulltime age ttl_exp nev_mar 
	scalar se_age = _se[age]
	scalar se_exp = _se[ttl_exp]
	scalar se_mar = _se[nev_mar]

	probit fulltime age ttl_exp nev_mar, r
	scalar se_ager = _se[age]
	scalar se_expr = _se[ttl_exp]
	scalar se_marr = _se[nev_mar]
	
	probit fulltime age ttl_exp nev_mar, vce(cluster ind_code)
	scalar se_agec = _se[age]
	scalar se_expc = _se[ttl_exp]
	scalar se_marc = _se[nev_mar]
	

	post buffer (se_age) (se_exp) (se_mar) (se_ager) (se_expr) (se_marr) (se_agec) (se_expc) (se_marc)
}

postclose buffer
use prob500, clear


****Graficos de los errores


histogram se_age, kdensity scheme(s2color) title("Probit ''age''") plotregion(style(none)) ytitle("Densidad") xtitle("SE age") norm name(graf1marage)

histogram se_ager, kdensity scheme(s2color) title("Robusto ''age''") plotregion(style(none)) ytitle("Densidad") xtitle("SE age") norm name(graf2marage)

histogram se_agec, kdensity scheme(s2color) title("Cluster''age''") plotregion(style(none)) ytitle("Densidad") xtitle("SE age") norm name(graf3marage)


histogram se_exp, kdensity scheme(s2color) title("Probit ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("SE exp") norm name(graf1margexp)

histogram se_expr, kdensity scheme(s2color) title("Robusto ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("SE exp") norm name(graf2margexp)

histogram se_expc, kdensity scheme(s2color) title("Cluster ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("SE exp") norm name(graf3margexp)


histogram se_mar, kdensity scheme(s2color) title("Probit ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("SE marr") norm name(graf1margmarr)

histogram se_marr, kdensity scheme(s2color) title("Robusto ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("SE marr") norm name(graf2margmarr)

histogram se_marc, kdensity scheme(s2color) title("Cluster ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("SE marr") norm name(graf3margmarr)

graph combine graf1marage graf2marage graf3marage graf1margexp graf2margexp graf3margexp graf1margmarr graf2margmarr graf3margmarr, col(3)


****Para Logit

clear all
set seed 1234
set level 90

global main "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS2"
cd "$main"

postfile buffer se_age se_exp se_mar se_ager se_expr se_marr se_agec se_expc se_marc using prob500, replace

forvalues i=1/500 {
	drop _all
	use "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS2/nlswork.dta"
	gen fulltime=0
	replace fulltime=1 if hours>=40
	sample 1000, count
	logit fulltime age ttl_exp nev_mar 
	scalar se_age = _se[age]
	scalar se_exp = _se[ttl_exp]
	scalar se_mar = _se[nev_mar]

	logit fulltime age ttl_exp nev_mar, r
	scalar se_ager = _se[age]
	scalar se_expr = _se[ttl_exp]
	scalar se_marr = _se[nev_mar]
	
	logit fulltime age ttl_exp nev_mar, vce(cluster ind_code)
	scalar se_agec = _se[age]
	scalar se_expc = _se[ttl_exp]
	scalar se_marc = _se[nev_mar]
	

	post buffer (se_age) (se_exp) (se_mar) (se_ager) (se_expr) (se_marr) (se_agec) (se_expc) (se_marc)
}

postclose buffer
use prob500, clear



****Graficos de los errores


histogram se_age, kdensity scheme(s2color) title("logit ''age''") plotregion(style(none)) ytitle("Densidad") xtitle("SE age") norm name(graf1marage)

histogram se_ager, kdensity scheme(s2color) title("Robusto ''age''") plotregion(style(none)) ytitle("Densidad") xtitle("SE age") norm name(graf2marage)

histogram se_agec, kdensity scheme(s2color) title("Cluster''age''") plotregion(style(none)) ytitle("Densidad") xtitle("SE age") norm name(graf3marage)


histogram se_exp, kdensity scheme(s2color) title("Logit ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("SE exp") norm name(graf1margexp)

histogram se_expr, kdensity scheme(s2color) title("Robusto ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("SE exp") norm name(graf2margexp)

histogram se_expc, kdensity scheme(s2color) title("Cluster ''exp''") plotregion(style(none)) ytitle("Densidad") xtitle("SE exp") norm name(graf3margexp)


histogram se_mar, kdensity scheme(s2color) title("Logit ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("SE marr") norm name(graf1margmarr)

histogram se_marr, kdensity scheme(s2color) title("Robusto ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("SE marr") norm name(graf2margmarr)

histogram se_marc, kdensity scheme(s2color) title("Cluster ''marr''") plotregion(style(none)) ytitle("Densidad") xtitle("SE marr") norm name(graf3margmarr)

graph combine graf1marage graf2marage graf3marage graf1margexp graf2margexp graf3margexp graf1margmarr graf2margmarr graf3margmarr, col(3)




***Para efectos de espacio en el informe, se pasa la gráfica solo de la estimación MCO



*********************************************************************************
********************************Heterocedasticidad*******************************
*********************************************************************************

**Pregunta 1 y 2
clear all
global main "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS2"
cd "$main"
set seed 2

set obs 4000
gen eps = rnormal(0,1)
gen x2 = rnormal(3, 0.5)
gen x3 = exp(10)
gen mu = rnormal(0,0.04)
gen x1 = 1+2.5*x2+0.5*x3+mu

gen y = 2+ 1.5*x1 +2*x2 +eps

reg y x1 x2
*outreg2 using reghe.tex, replace ctitle(Modelo 1)

reg y x1
*outreg2 using reghe.tex, append ctitle(Modelo 2)


**Pregunta 3
clear all
global main "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS2"
cd "$main"
set seed 2

set obs 4000
gen eps=.
replace eps=rnormal(0,0.5) in 1/1000
replace eps=rnormal(0,1.5) in 1001/2000
replace eps=rnormal(0,3) in 2001/3000
replace eps=rnormal(0,4.5) in 3001/4000
gen x2 = rnormal(3, 0.5)
gen x3 = exp(10)
gen mu = rnormal(0,0.04)
gen x1 = 1+2.5*x2+0.5*x3+mu

gen y = 2+1.5*x1+2*x2+eps

gen clusterizado = .
replace clusterizado = 1 in 1/1000
replace clusterizado = 2 in 1001/2000
replace clusterizado = 3 in 2001/3000
replace clusterizado = 4 in 3001/4000

reg y x1 x2, vce(cluster clusterizado)

outreg2 using reghe.tex, append ctitle(Modelo 4)

**Pregunta 4

postfile buffer beta1 beta2 beta1b beta1c beta2c using reg500, replace

forvalues i=1/500 {
         drop _all
         set obs 1000
		 set obs 4000
		 gen eps = rnormal(0,1)
		 gen x2 = rnormal(3, 0.5)
		 gen x3 = exp(10)
		 gen mu = rnormal(0,0.04)
		 gen x1 = 1+2.5*x2+0.5*x3+mu

		 gen y = 2+1.5*x1+2*x2+eps

		 reg y x1 x2
		 scalar beta1 = _b[x1] //se recupera beta 2 en cada loop
		 scalar beta2 = _b[x2] //se recupera beta 2 en cada loop
		 
		 reg y x1
		 scalar beta1b = _b[x1] //se recupera beta 2 en cada loop
	
		 drop eps
		 drop y
		 gen eps=.
		 replace eps=rnormal(0,0.5) in 1/1000
		 replace eps=rnormal(0,1.5) in 1001/2000
		 replace eps=rnormal(0,3) in 2001/3000
		 replace eps=rnormal(0,4.5) in 3001/4000
		 gen y = 2+1.5*x1+2*x2+eps
		 
		 reg y x1 x2, r		 
		 scalar beta1c = _b[x1] //se recupera beta 2 en cada loop
		 scalar beta2c = _b[x2] //se recupera beta 2 en cada loop		 
		 

         post buffer (beta1) (beta2) (beta1b) (beta1c) (beta2c)
 }

postclose buffer
use reg500, clear

histogram beta1, kdensity scheme(s2color) title("Estimación Beta1 usando x2") plotregion(style(none)) note("Source: Own elaboration.",) norm name(graf1)

histogram beta1b, kdensity scheme(s2color) title("Estimación Beta1 sin x2") plotregion(style(none)) note("Source: Own elaboration.",) norm name(graf2)

histogram beta1c, kdensity scheme(s2color) title("Estimación Beta1 robusto usando x2") plotregion(style(none)) note("Source: Own elaboration.",) norm name(graf3)

graph combine graf1 graf2 graf3

histogram beta2, kdensity scheme(s2color) title("Estimación Beta2") plotregion(style(none)) note("Source: Own elaboration.",) norm name(graf1b)

histogram beta2c, kdensity scheme(s2color) title("Estimación Beta2 robusto") plotregion(style(none)) note("Source: Own elaboration.",) norm name(graf2b)

graph combine graf1b graf2b




*************************************************************************************
*************************************************************************************
*************************************************************************************


*************************************************************************************
*********************************Empírico (Parte 3)**********************************
*************************************************************************************

*************************************************************************************
*************************************************************************************
*************************************************************************************


clear all
global main "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS2"
cd "$main"
use "/Users/rodrigobeltranmoreira/Desktop/Econometria 1/PS2/datosp3.dta"

**Nota para los ayudantes: Para las regresiones, dejé las variables de control en un local para que corriera todo el código de una vez, para ir corriendo regresion por regresion hay que correr la regresion requerida junto al local.

**Además, el local sirve como medida temporal para poder manejar el código de la regresión más simple y sin ver todas las variables molestando.

*Se requiere separar los cuartiles para estimar correctamente la regresion**
tab s_group_quart, gen(s_group_quartil)
forvalues i=1(1)16 {
	local s_group_quartil`i'
}


** Correlación del instrumento con las variables dependientes
corr T gpa2008 avggrade2008 gpa2008 earned2008 ptsover702008




************** Panel A *****************
******************Primera etapa regresión********************

*** Para toda la muestra***

**Se definen las variables de control para hacer ver la regresión más simple
*Regresion para Primeros años
local controles "s_hsgrade3 s_mtongue_english s_mothergraddegree s_test1correct s_test2correct s_motherhsdegree s_mothercolldegree s_mothergraddegree s_mothereducmiss s_fatherhsdegree s_fathercolldegree s_fathergraddegree s_fathereducmiss s_group_quartil1 s_group_quartil2 s_group_quartil3 s_group_quartil4 s_group_quartil5 s_group_quartil6 s_group_quartil7 s_group_quartil8 s_group_quartil9 s_group_quartil10 s_group_quartil11 s_group_quartil12 s_group_quartil13 s_group_quartil14 s_group_quartil15 s_group_quartil16" 

reg anycontact T `controles' if s_first_year==1 & earnings2008!=. , r

*Regresion para Segundos años
reg anycontact T `controles' if s_first_year==0 & earnings2008!=. , r

*Regresion para Todos
reg anycontact T `controles' if earnings2008!=. , r



******************Segunda etapa regresión IV********************


**Primeros años**
ivregress 2sls earnings2008 `controles' (anycontact = T) if s_first_year==1 , vce(robust)
ivregress 2sls avggrade2008 `controles' (anycontact = T) if s_first_year==1 , vce(robust)
ivregress 2sls gpa2008 `controles' (anycontact = T) if s_first_year==1 , vce(robust)
ivregress 2sls earned2008 `controles' (anycontact = T) if s_first_year==1 , vce(robust)
ivregress 2sls ptsover702008 `controles' (anycontact = T) if s_first_year==1 , vce(robust)



	
	

**Segundos años**
ivregress 2sls earnings2008 `controles' (anycontact = T) if s_first_year==0 , vce(robust)
ivregress 2sls avggrade2008 `controles' (anycontact = T) if s_first_year==0 , vce(robust)
ivregress 2sls gpa2008 `controles' (anycontact = T) if s_first_year==0 , vce(robust)
ivregress 2sls earned2008 `controles' (anycontact = T) if s_first_year==0 , vce(robust)
ivregress 2sls ptsover702008 `controles' (anycontact = T) if s_first_year==0 , vce(robust)

	
	

**Todos**
ivregress 2sls earnings2008 `controles' (anycontact = T)  , vce(robust)
ivregress 2sls avggrade2008 `controles' (anycontact = T) , vce(robust)
ivregress 2sls gpa2008 `controles' (anycontact = T) , vce(robust)
ivregress 2sls earned2008 `controles' (anycontact = T) , vce(robust)
ivregress 2sls ptsover702008 `controles' (anycontact = T) , vce(robust)

	

************** Panel B *****************
*** Students Who Calculated Awards Correctly***
**Primera etapa regresión**

*Regresion para Primeros años
reg anycontact T `controles' if s_first_year==1 & earnings2008!=. & s_test2correct==1 , r

*Regresion para Segundos años
reg anycontact T `controles' if s_first_year==0 & earnings2008!=. & s_test2correct==1 , r

*Regresion para Todos
reg anycontact T `controles' if earnings2008!=. & s_test2correct==1, r


******* Students Who Calculated Awards Correctly*******

**Primeros años**
ivregress 2sls earnings2008 `controles' (anycontact = T) if s_first_year==1 & s_test2correct==1, vce(robust)
ivregress 2sls avggrade2008 `controles' (anycontact = T) if s_first_year==1 & s_test2correct==1, vce(robust)
ivregress 2sls gpa2008 `controles' (anycontact = T) if s_first_year==1 & s_test2correct==1, vce(robust)
ivregress 2sls earned2008 `controles' (anycontact = T) if s_first_year==1 & s_test2correct==1, vce(robust)
ivregress 2sls ptsover702008 `controles' (anycontact = T) if s_first_year==1 & s_test2correct==1, vce(robust)

	
	

**Segundos años**
ivregress 2sls earnings2008 `controles' (anycontact = T) if s_first_year==0 & s_test2correct==1, vce(robust)
ivregress 2sls avggrade2008 `controles' (anycontact = T) if s_first_year==0 & s_test2correct==1, vce(robust)
ivregress 2sls gpa2008 `controles' (anycontact = T) if s_first_year==0 & s_test2correct==1, vce(robust)
ivregress 2sls earned2008 `controles' (anycontact = T) if s_first_year==0 & s_test2correct==1, vce(robust)
ivregress 2sls ptsover702008 `controles' (anycontact = T) if s_first_year==0 & s_test2correct==1, vce(robust)


	

**Todos**
ivregress 2sls earnings2008 `controles' (anycontact = T) if s_test2correct==1 , vce(robust)
ivregress 2sls avggrade2008 `controles' (anycontact = T) if s_test2correct==1, vce(robust)
ivregress 2sls gpa2008 `controles' (anycontact = T) if  s_test2correct==1, vce(robust)
ivregress 2sls earned2008 `controles' (anycontact = T) if s_test2correct==1, vce(robust)
ivregress 2sls ptsover702008 `controles' (anycontact = T) if s_test2correct==1, vce(robust)

