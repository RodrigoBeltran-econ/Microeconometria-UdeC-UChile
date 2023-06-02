use "C:\Users\Rodrigo\Desktop\Econometria\basedatos8.dta", clear

*Se realiza estadística descriptiva
summarize ventas precio_medio precio_comp publicidad

*Matriz de Correlación
pwcorr ventas precio_medio precio_comp publicidad

****Regresión OLS
regress ventas precio_medio precio_comp publicidad

*Generando residuos
predict resid, residuals

*Graficando residuos
twoway (scatter resid date), name(resid)

*Prueba de Heterocedasticidad
estat hettest
estat hettest precio_medio
estat hettest precio_comp
estat hettest publicidad

*Heterocedasticidad en los predictores
rvpplot precio_medio, recast(scatter) yline(0) name(preciom)
rvpplot precio_comp, recast(scatter) yline(0) name(precioc)
rvpplot publicidad, recast(scatter) yline(0) name(public)


*Regresión por ML
glm ventas precio_medio precio_comp publicidad, family(gaussian) link(identity)

*Regesión OLS robusto
regress ventas precio_medio precio_comp publicidad, vce(robust)


****Reescalando las variables en log
gen logventas=log(ventas)
gen logprecio_medio=log(precio_medio)
gen logprecio_comp=log(precio_comp)
gen logpublicidad=log(publicidad)

*Regresión OLS para log
regress logventas logprecio_medio logprecio_comp logpublicidad

*Generando residuos
predict logresid, residuals

*Graficando residuos
twoway (scatter logresid date), name(logresid)

*Prueba de Heteroccedasticidad
estat hettest
estat hettest logprecio_medio
estat hettest logprecio_comp
estat hettest logpublicidad

*Heterocedasticidad en los predictores
rvpplot logprecio_medio, recast(scatter) yline(0) name(logprecm)
rvpplot logprecio_comp, recast(scatter) yline(0) name(logprecc)
rvpplot logpublicidad, recast(scatter) yline(0) name(logpubl)

*Regresión por ML
glm logventas logprecio_medio logprecio_comp logpublicidad, family(gaussian) link(identity)

*Regesión OLS robusto
regress logventas logprecio_medio logprecio_comp logpublicidad, vce(robust)




