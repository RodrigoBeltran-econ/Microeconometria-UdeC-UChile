use "C:\Users\Rodrigo\Desktop\SetPractico5.dta", clear

*Desarrollamos el analisis estadistico de la muestra
summarize pib_chile capital_chile fl_chile
pwcorr pib_chile capital_chile fl_chile, sig

*Se corre la regresion OLS sobre las variables en nivel
regress pib_chile capital_chile fl_chile
estimates store nivel

*Aplicamos la prueba de variable omitida, la que se aplica una vez corrida la regresion
estat ovtest 
*Dado que hemos corrido la regresion original , ahora calculamos el PIB  estimado
predict yhat 
generate pib_chile2=yhat*yhat
regress pib_chile capital_chile fl_chile pib_chile2

*Desarrollamos el modelo reescalando las variables en log
generate ln_pib_chile=ln(pib_chile)
generate ln_cap_chile=ln(capital_chile)
generate ln_fl_chile=ln(fl_chile)
regress ln_pib_chile ln_cap_chile ln_fl_chile

*Aplicamos la regresion y testeamos las siguientes hipotesis
test (ln_cap_chile+ln_fl_chile=0.75)
test (ln_cap_chile+ln_fl_chile=0.85)
test (ln_cap_chile+ln_fl_chile=1.05)
test (ln_cap_chile+ln_fl_chile=1.25)
test (ln_cap_chile+ln_fl_chile=1.45)
test (ln_cap_chile+ln_fl_chile=1.65)
test (ln_cap_chile+ln_fl_chile=1.85)
test (ln_cap_chile+ln_fl_chile=1.95)
test (ln_cap_chile+ln_fl_chile=2.00)
test (ln_cap_chile+ln_fl_chile=2.25)
test (ln_cap_chile+ln_fl_chile=2.50)
