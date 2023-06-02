use "C:\Users\Rodrigo\Desktop\Econometria\Bases de Datos\BaseDatosEconometria1Set2.dta" 
****Se realiza un estudio estad�stico descriptivo de las variables a trabajar en los tres casos.
summarize
****A continuaci�n, se trabajar� en la regresi�n compuesta por las variables macroecon�micas.
*Se realizar� un an�lisis de correlaci�n entre las variables para ambos casos (Peque�as y Grandes Empresas).
pwcorr pe entorno_eco cap_banco fondeo_banco ifrs_pe, sig
pwcorr ge entorno_eco cap_banco fondeo_banco ifrs_ge, sig
*Ahora, procederemos a correr la regresi�n para peque�as empresas y despu�s para grandes empresas.
regress pe entorno_eco cap_banco fondeo_banco
regress ge entorno_eco cap_banco fondeo_banco
*Ahora se correr� la regresi�n cuando hay IFRS presente.
regress pe entorno_eco cap_banco fondeo_banco ifrs_pe
regress ge entorno_eco cap_banco fondeo_banco ifrs_ge
****Como segundo caso, veremos las condiciones crediticias en relaci�n a las variables del sector industrial.
*A continuaci�n se realizar� un an�lisis de correlaci�n.
pwcorr pe riesgo_cartera_pe comp_bancaria_pe, sig
pwcorr ge riesgo_cartera_ge comp_bancaria_ge, sig
*Ahora se procede a correr la regresi�n.
regress pe riesgo_cartera_pe comp_bancaria_pe
regress ge riesgo_cartera_ge comp_bancaria_ge
*Ahora se correr� la regresi�n cuando hay IFRS presente.
regress pe riesgo_cartera_pe comp_bancaria_pe ifrs_pe
regress ge riesgo_cartera_ge comp_bancaria_ge ifrs_ge
****Como �ltimo caso, se medir� las condiciones crediticias respecto a los factores microecon�micos presentes.
*A continuaci�n se ver� el an�lisis de correlaci�n para peque�as y grandes empresas.
pwcorr pe dism_licred_pe mayor_spread_fond_pe mayor_garant_pe menor_plazo_pe, sig
pwcorr ge dism_licred_ge mayor_spread_fond_ge mayor_garant_ge menor_plazo_ge, sig
*Ahora se procede a correr la regresi�n.
regress pe dism_licred_pe mayor_spread_fond_pe mayor_garant_pe menor_plazo_pe
regress ge dism_licred_ge mayor_spread_fond_ge mayor_garant_ge menor_plazo_ge
*Ahora se correr� la regresi�n cuando hay IFRS presente.
regress pe dism_licred_pe mayor_spread_fond_pe mayor_garant_pe menor_plazo_pe ifrs_pe
regress ge dism_licred_ge mayor_spread_fond_ge mayor_garant_ge menor_plazo_ge ifrs_ge
