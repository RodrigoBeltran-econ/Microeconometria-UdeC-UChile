********** SET 5: LOGIT MULTINOMIAL ************

*Primero que todo, codificaremos las alternativas que aparece en caracter de letras. Utilizaremos el siguiente comando y crearemos una nueva variable que muestre as alternativas.
encode mode, generate(mode2)

*Luego, etiquetaremos cada variable para darle una descripción
label var mode2 "Modo de pesca"
label var price "Precio de la Alternativa Elegida"
label var crate "Tasa de Captura de la Alternativa Elegida"
label var dbeach "1 si se eligio el modo playa"
label var dpier "1 si se eligio el modo muelle"
label var dprivate "1 si se eligio el modo de barco privado"
label var dcharter "1 si se eligio el modo de barco charter"
label var pbeach "precio del modo de pesca playa"
label var ppier "precio del modo de pesca muelle"
label var pprivate "precio del modo de pesca barco privado"
label var pcharter "precio del modo de pesca barco charter"
label var qbeach "tasa de captura del modo de pesca playa"
label var qpier "tasa de captura del modo de pesca muelle"
label var qprivate "tasa de captura del modo de pesca barco privado"
label var qcharter "tasa de captura del modo de pesca barco charter"
label var income  "Ingreso mensual en miles de dolares"
label var sex  "1 si es hombre el que contrata"
label var id "Individuo"
describe
summarize, separator(0)
tabulate mode2
codebook  mode2
*Ahora procederemos a estimar un modelo Logit Multinomial para el modo de pesca que eligen los pescadores.
mlogit mode2 income sex
*Ahora estimaremos un MLOGIT tomando la opcion 1 como base
mlogit mode2 income sex, baseoutcome(1) nolog
*Esta regresion la guardaremos con el siguiente comando
estimates store eq1
*Practicamos pruebas de Wald para evaluar la significancia de los regresores
mlogit mode2 income sex, baseoutcome(1) nolog
test (income sex)
test (income)
test (sex)
*Income tiene incidencia en la elección de una alternativa sobre otra
*Para ello correremos eliminando distintas alternativas y guardando cada regresion MLOGIT
mlogit mode2 income sex if mode2!=2, baseoutcome(1) nolog
estimates store eq2
mlogit mode2 income sex if mode2!=3, baseoutcome(1) nolog
estimates store eq3
mlogit mode2 income sex if mode2!=4, baseoutcome(1) nolog
estimates store eq4
estimates table eq1 eq2 eq3 eq4, b(%7.4f) se(%7.4f) p(%7.4f)
hausman eq3 eq1
hausman eq4 eq1
*Los modelos están bien especificados por su independencia de alternativas -> ver p-value
*******
hausman eq2 eq1
*No corre porque no existe diferencia significativa
********

*Ahora procedemos a estimar los efectos marginales de cada alternativa
mlogit mode2 income sex, baseoutcome(1) nolog
mfx, predict(p outcome(1))
mfx, predict(p outcome(2))
mfx, predict(p outcome(3))
mfx, predict(p outcome(4))

* Ahora compararemos las probabilidades con las respectivas frecuencias relativas
mlogit mode2 income sex, baseoutcome(1) nolog
predict pmlogit1 pmlogit2 pmlogit3 pmlogit4, pr
summarize pmlogit* dbeach dpier dprivate dcharter, separator(4)



******************************************************************************************

********************* SET 6: LOGIT CONDICIONAL **************************


reshape long d p q, i(id) j (fishmode beach pier private charter) string

*Luego estimamos el modelo Logit Condicional de MacFadden con alternativa específica 
asclogit d p q, case(id) alternatives(fishmode) casevars(income sex) basealternative(beach) nolog

*Efectos marginales por precio y cantidad
estat mfx, varlist(p)
estat mfx, varlist(q)

*Test de significancia para p y q sobre la decisión
test (p q)

*Podemos comparar Logit Multinomial con el COndicional. Para ello asumimos que  no hay regresores asociados a atributos de las alternativas.
asclogit d, case(id) alternatives(fishmode) casevars(income sex) basealternative(beach) nolog
estimates store MLOGIT

*Predecimos probabilidades y las comparamos con las frecuencias relativas
asclogit d p q, case(id) alternatives(fishmode) casevars(income sex) basealternative(beach) nolog
predict pasclogit
table fishmode, contents (mean d mean pasclogit sd pasclogit) cellwidth(15)

*Luego evaluamos el test de Haussman para IAI (eliminamos  charter(4))
asclogit d p q, case(id) alternatives(fishmode) casevars(income sex) basealternative(beach) nolog
estimates store eq1
asclogit d p q if mode1!=4, case(id) alternatives(fishmode) casevars(income sex) basealternative(beach) nolog
estimates store eq2
hausman eq2 eq1

*Estimamos modelo Logit Mixto e interpretamos los parámetros
generate dbeach=fishmode=="beach"
generate dprivate=fishmode=="private"
generate dcharter=fishmode=="charter"
generate ybeach=dbeach*income
generate yprivate=dprivate*income
generate ycharter=dcharter*income
generate gbeach=dbeach*sex
generate gprivate=dprivate*sex
generate gcharter=dcharter*sex

mixlogit d q dbeach dprivate dcharter ybeach yprivate ycharter gbeach gprivate gcharter, group(id) rand(p)
mfx


************************************************************************************************





****************** SET 7: PROBIT MULTINOMIAL ***************************

*recodigicamos el modo de pesca
encode(mode), generate(mode2)
*luego estimamos el modelos probit multinomial
mprobit mode2 income sex, baseoutcome(1)
test(income sex)
mfx, predict(p outcome(1))
mfx, predict(p outcome(2))
mfx, predict(p outcome(3))
mfx, predict(p outcome(4))
*
mlogit mode2 income sex, baseoutcome(1)
estimates store MLOGIT
mfx, predict(p outcome(1))
mfx, predict(p outcome(2))
mfx, predict(p outcome(3))
mfx, predict(p outcome(4))
mprobit mode2 income sex, baseoutcome(1)
estimates store MPROBIT
estimates table MPROBIT MLOGIT, b(%7.4f) p(%7.4f)


mprobit mode2 income sex, baseoutcome(1) nolog
predict pmprobit1 pmprobit2 pmprobit3 pmprobit4, pr
summarize pmlogit* pmprobit* dbeach dpier dprivate dcharter, separator(4)


************************************************************************************************



******************* SET 8: LOGIT ANIDADO ******************************

*El proposito es caracterizar las alternativas de pesca y los individuos
reshape long d p q, i(id) j(fishmode beach pier private charter) string

*Luego agrupamos las alternativas en dos categorias, pesca en bote  o pesca en costa/orilla
nlogitgen type = fishmode(shore: pier | beach, boat: private | charter)

*Con el siguiente comando testeamos la estructura del arbol del LOGIT ANIDADO
nlogittree fishmode type, choice(d)

*Luego estimamos el modelo LOGIT ANIDADO
nlogit d p q || type:, base(shore) || fishmode: income, case(id) notree nolog
test (p q)
test (p q income)

*Guardamos el modelo
estimates store Nested

*Predice el valor inclusivo de cada alternativa de pesca y evaluamos su significancia
predict iv, iv
ttest iv == 0
ttest iv == 1

*Comparemos el CLOGIT con NLOGIT
asclogit d p q, case(id) alternatives(fishmode) casevars(income) basealternative(beach) nolog
estimates store CLogit
estimates table CLogit Nested, keep(p q) stats( N ll aic bic) equation(1) b(%7.3f) se(%7.3f) p(%7.3f) stfmt(%7.0f)

*Conclusion -> Condicional no cumple IAI
* Individuos obtienen mayor utilidad a partir de la cuota de captura

by fishmode, sort: summarize p d q

*******
nlogit d p q || type:, base(shore) || fishmode: income, case(id) notree nolog

*Elaboramos una prediccion para la probabilidad de elegir el tipo de pesca
predict plevel1 plevel2, pr

* 0 por el comando predict double p*, pr
*Luego tabulamos los resultados
tabulate fishmode, summarize(plevel1)
tabulate fishmode, summarize(plevel2)

*Analizamos las probabilidades de cada alternativa
list d plevel1 plevel2 in 1/4, clean
estat alternatives

******************TAREA: INTERPRETAR MFX-->NLOGIT

*Calculamos el efecto marginal de un cambio en el precio de pesca en playa sobre la probabilidad de elegir esa y las demás alternativas
preserve
quietly summarize p
generate delta = r(sd)/1000
quietly replace p=p+delta if fishmode == "beach"
predict pnew1 pnew2, pr
generate dpdbeach=(pnew2-plevel2)/delta
tabulate fishmode, summarize(dpdbeach)

display -.00054261/.00048508
*Haciendo display se ve no significancia en ninguna alternativa


*************************************************************************************************














