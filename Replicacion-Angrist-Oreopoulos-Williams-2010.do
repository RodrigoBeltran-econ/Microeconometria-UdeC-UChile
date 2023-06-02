*************************************************************************************
*********************************Empírico (Parte 3)**********************************
*************************************************************************************


clear all
global main "/Users/rodrigo/Desktop/Econometria 1/PS2"
cd "$main"
use "/Users/rodrigo/Desktop/Econometria 1/PS2/datosp3.dta"

**Nota: Para las regresiones, dejé las variables de control en un local para que corriera todo el código de una vez, para ir corriendo regresion por regresion hay que correr la regresion requerida junto al local.
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

