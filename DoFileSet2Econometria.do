use "C:\Users\Rodrigo\Desktop\Econometria\Bases de Datos\BaseDatosEconometria1Set2.dta" 
****Se realiza un estudio estadístico descriptivo de las variables a trabajar en los tres casos.
summarize
****A continuación, se trabajará en la regresión compuesta por las variables macroeconómicas.
*Se realizará un análisis de correlación entre las variables para ambos casos (Pequeñas y Grandes Empresas).
pwcorr pe entorno_eco cap_banco fondeo_banco ifrs_pe, sig
pwcorr ge entorno_eco cap_banco fondeo_banco ifrs_ge, sig
*Ahora, procederemos a correr la regresión para pequeñas empresas y después para grandes empresas.
regress pe entorno_eco cap_banco fondeo_banco
regress ge entorno_eco cap_banco fondeo_banco
*Ahora se correrá la regresión cuando hay IFRS presente.
regress pe entorno_eco cap_banco fondeo_banco ifrs_pe
regress ge entorno_eco cap_banco fondeo_banco ifrs_ge
****Como segundo caso, veremos las condiciones crediticias en relación a las variables del sector industrial.
*A continuación se realizará un análisis de correlación.
pwcorr pe riesgo_cartera_pe comp_bancaria_pe, sig
pwcorr ge riesgo_cartera_ge comp_bancaria_ge, sig
*Ahora se procede a correr la regresión.
regress pe riesgo_cartera_pe comp_bancaria_pe
regress ge riesgo_cartera_ge comp_bancaria_ge
*Ahora se correrá la regresión cuando hay IFRS presente.
regress pe riesgo_cartera_pe comp_bancaria_pe ifrs_pe
regress ge riesgo_cartera_ge comp_bancaria_ge ifrs_ge
****Como último caso, se medirá las condiciones crediticias respecto a los factores microeconómicos presentes.
*A continuación se verá el análisis de correlación para pequeñas y grandes empresas.
pwcorr pe dism_licred_pe mayor_spread_fond_pe mayor_garant_pe menor_plazo_pe, sig
pwcorr ge dism_licred_ge mayor_spread_fond_ge mayor_garant_ge menor_plazo_ge, sig
*Ahora se procede a correr la regresión.
regress pe dism_licred_pe mayor_spread_fond_pe mayor_garant_pe menor_plazo_pe
regress ge dism_licred_ge mayor_spread_fond_ge mayor_garant_ge menor_plazo_ge
*Ahora se correrá la regresión cuando hay IFRS presente.
regress pe dism_licred_pe mayor_spread_fond_pe mayor_garant_pe menor_plazo_pe ifrs_pe
regress ge dism_licred_ge mayor_spread_fond_ge mayor_garant_ge menor_plazo_ge ifrs_ge
