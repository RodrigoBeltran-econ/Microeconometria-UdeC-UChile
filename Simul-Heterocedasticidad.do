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
