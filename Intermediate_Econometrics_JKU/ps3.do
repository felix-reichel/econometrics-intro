clear
version 11
set more off
set cformat %5.3f
capture log close
set scheme s1mono

cd "/Users/felixreichel/Documents/UNI/WIWI_Bachelor/2023S/IK Econometrics"
cap mkdir ps3

log using "ps3/ps3", replace

use "http://www.econ.jku.at/t3/staff/zweimueller/IntMetrics/ik/data/wage2.dta", clear 

gen exper2 = exper*exper

reg wage educ exper exper2 tenure i.black i.married i.south i.urban

predict yhat, xb
su wage yhat

label var yhat "Fitted values"
twoway (scatter wage yhat, mcolor(dkorange)), ///
	ytitle(Observed values) ylabel(,nogrid) ///
	xtitle(Fitted values) ///
	legend(off) xline(0 1, lcolor(gs10) lpattern(dash))

predict uhat, r
scatter uhat yhat, msymbol(Oh) mcolor(dkorange) yline(0, lpattern(dash) lcolor(black)) ylabel(,nogrid)

qui reg wage educ exper exper2 tenure i.black i.married i.south i.urban
estat hettest, fstat // option fstat drops normality assumption

reg lwage educ exper exper2 tenure i.black i.married i.south i.urban
estat hettest, fstat // option fstat drops normality assumption

reg lwage educ##i.black exper exper2 tenure i.married i.south i.urban

margins black, at(educ=(10(1)18))

marginsplot, ///
	plot1opts(lpattern(solid) msymbol(none)) plot2opts(lpattern(dash) msymbol(none) lcolor(dkorange)) ///
	recastci(rarea) ci1opts(color(gs12)) ci2opts(color(dkorange*0.5)) ///
	legend(order(1 "Non-Black" 2 "Black") rows(1)) yline(0, lcolor(gs8)) ///
	ytitle("Linear prediction of wage") xtitle("Education in years") title("")

use "http://www.econ.jku.at/t3/staff/zweimueller/IntMetrics/ik/data/titanic3.dta", clear 

eststo m1: reg survived i.female age pclass
eststo m2: reg survived i.female age pclass, robust

estout m1 m2, cells(b(star fmt(3)) se(par fmt(4))) stats(N, fmt(0)) ///
	mlabels("usual se" "robust se") starlevel(* 0.05 ** 0.01) ///
	postfoot("sig. at 1%-level (**), 5% level (*)")

reg survived i.female age pclass, robust
predict yhat, xb
su survived yhat

label var yhat "Fitted values"
twoway (scatter survived yhat, mcolor(dkorange)), ///
	ytitle(Observed values) ylabel(,nogrid) ///
	xtitle(Fitted values) ///
	legend(off) xline(0 1, lcolor(gs10) lpattern(dash))

gen pclass1 = 0
gen pclass2 = 0
gen pclass3 = 0

replace pclass1 = 1 if pclass==1
replace pclass2 = 1 if pclass==2
replace pclass3 = 1 if pclass==3

reg survived i.female age i.pclass1 i.pclass2 i.pclass3, robust

eststo m1: reg survived age i.pclass1 i.pclass2 i.pclass3 if female==0, robust
eststo m2: reg survived age i.pclass1 i.pclass2 i.pclass3 if female==1, robust

estout m1 m2, cells(b(star fmt(3))) stats(N, fmt(0)) ///
	mlabels("males" "females") starlevel(* 0.05 ** 0.01) ///
	postfoot("sig. at 1%-level (**), 5% level (*)")
reg survived age i.female##(i.pclass1 i.pclass2 i.pclass3), robust noheader

test 1.female 1.female#1.pclass1 1.female#1.pclass2 1.female#1.pclass3 // F-test

scalar drop _all

* Pooled vs. separate estimation
qui reg survived age i.pclass1 i.pclass2 i.pclass3 if female == 0, robust
scalar define SSRm = e(rss) // SSR males
qui reg survived age i.pclass1 i.pclass2 i.pclass3 if female == 1, robust
scalar def SSRf = e(rss) // SSR females
scalar def k = e(df_m) // k parameters
qui reg survived age i.pclass1 i.pclass2 i.pclass3 if !missing(female), robust
scalar def SSRp = e(rss) // SSR pooled
qui count if e(sample)
scalar def n = r(N) // n observations
scalar list

* Compute the Chow test statistic (= F statistic):
scalar def fstat=((SSRp-(SSRm+SSRf))/(SSRm+SSRf))*(n-2*(k+1))/(k+1)
di "F = " %9.3f fstat
di "critical value =" %9.3f invFtail(k+1,n-2*(k+1),0.05)
di "p-value =" %9.3f Ftail(k+1,n-2*(k+1),fstat)

reg survived age i.pclass1 i.pclass2 i.pclass3 if female == 0, robust
reg survived age i.pclass1 i.pclass2 i.pclass3 if female == 1, robust

summarize age
summarize age if female == 0
summarize age if female == 1


reg survived age i.pclass1 i.pclass2 i.pclass3 if female == 1, robust noheader

margins, at(age=(0.3333(1)80)) atmeans

marginsplot, plotop(msym(none) lcolor(dkorange) lwidth(*2)) ///
	recastci(rarea) ciopts(color(dkorange*0.5)) yline(0) ///
	ytitle("Probability of survival") ylabel(,nogrid) ///
	xtitle("Age in years") note("Females") title("Probability of survival females")
	
	
reg survived age i.pclass1 i.pclass2 i.pclass3 if female == 0, robust noheader

margins, at(age=(0.1667(1)76)) atmeans

marginsplot, plotop(msym(none) lcolor(dkorange) lwidth(*2)) ///
	recastci(rarea) ciopts(color(dkorange*0.5)) yline(0) ///
	ytitle("Probability of survival") ylabel(,nogrid) ///
	xtitle("Age in years") note("Males") title("Probability of survival males")
	
	
	
cap log close
clear
