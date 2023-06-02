use "C:\Users\Rodrigo\Desktop\Econometria\basedatos6.dta", clear

****Para iniciar el análisis, primero se debe reescalar la variable "activo" por medio de ln
generate size=ln(activo)

*Se realiza un summarize
summarize ratv mon_banc holding dueo socio gerente exp_gte fuidespedido roav bs size activo

*Matriz de Correlación
pwcorr ratv mon_banc holding dueo socio gerente exp_gte fuidespedido roav bs size activo, sig

****Se corren las regresiones para cada caso de estructura de propiedad
*Regresión para variable dueño
regress ratv bs mon_banc size fuidespedido roav dueo holding 

*Aplicando test de Ramsey y criterio de información
estat ovtest
estat ic

*Regresión para variable socio
regress ratv bs mon_banc size fuidespedido roav socio holding 

*Aplicando test de Ramsey y criterio de información
estat ovtest
estat ic

*Regresión para variable gerente 
regress ratv bs mon_banc size fuidespedido roav gerente holding 

*Aplicando test de Ramsey y criterio de información
estat ovtest
estat ic

****Realizamos la regresión OLS, pero eliminando las variables de estructura de propiedad y de endeudamiento
regress ratv mon_banc size fuidespedido roav holding 

*Aplicando test de Ramsey y criterio de información
estat ovtest
estat ic

****Ahora se estimará la primera regresión OLS, agregándole la variable experiencia del gerente general
*Regresión con estructura de propiedad dueño
regress ratv bs mon_banc size fuidespedido roav holding dueo exp_gte

*Aplicando test de Ramsey y criterio de información
estat ovtest
estat ic

*Regresión con estructura de propiedad socio
regress ratv bs mon_banc size fuidespedido roav holding socio exp_gte

*Aplicando test de Ramsey y criterio de información
estat ovtest
estat ic

*Regresión con estructura de propiedad gerente
regress ratv bs mon_banc size fuidespedido roav holding gerente exp_gte

*Aplicando test de Ramsey y criterio de información
estat ovtest
estat ic



*estat ovtest -> test de Ramseay (omisión de variables) | estat vif -> test de multicolinealidad | estat imtest -> test de heterocedasticidad; curtosis
