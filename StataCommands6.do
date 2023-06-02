use "C:\Users\Rodrigo\Desktop\Econometria\basedatos6.dta", clear

****Para iniciar el an�lisis, primero se debe reescalar la variable "activo" por medio de ln
generate size=ln(activo)

*Se realiza un summarize
summarize ratv mon_banc holding dueo socio gerente exp_gte fuidespedido roav bs size activo

*Matriz de Correlaci�n
pwcorr ratv mon_banc holding dueo socio gerente exp_gte fuidespedido roav bs size activo, sig

****Se corren las regresiones para cada caso de estructura de propiedad
*Regresi�n para variable due�o
regress ratv bs mon_banc size fuidespedido roav dueo holding 

*Aplicando test de Ramsey y criterio de informaci�n
estat ovtest
estat ic

*Regresi�n para variable socio
regress ratv bs mon_banc size fuidespedido roav socio holding 

*Aplicando test de Ramsey y criterio de informaci�n
estat ovtest
estat ic

*Regresi�n para variable gerente 
regress ratv bs mon_banc size fuidespedido roav gerente holding 

*Aplicando test de Ramsey y criterio de informaci�n
estat ovtest
estat ic

****Realizamos la regresi�n OLS, pero eliminando las variables de estructura de propiedad y de endeudamiento
regress ratv mon_banc size fuidespedido roav holding 

*Aplicando test de Ramsey y criterio de informaci�n
estat ovtest
estat ic

****Ahora se estimar� la primera regresi�n OLS, agreg�ndole la variable experiencia del gerente general
*Regresi�n con estructura de propiedad due�o
regress ratv bs mon_banc size fuidespedido roav holding dueo exp_gte

*Aplicando test de Ramsey y criterio de informaci�n
estat ovtest
estat ic

*Regresi�n con estructura de propiedad socio
regress ratv bs mon_banc size fuidespedido roav holding socio exp_gte

*Aplicando test de Ramsey y criterio de informaci�n
estat ovtest
estat ic

*Regresi�n con estructura de propiedad gerente
regress ratv bs mon_banc size fuidespedido roav holding gerente exp_gte

*Aplicando test de Ramsey y criterio de informaci�n
estat ovtest
estat ic



*estat ovtest -> test de Ramseay (omisi�n de variables) | estat vif -> test de multicolinealidad | estat imtest -> test de heterocedasticidad; curtosis
