clear // empty working space
version 11 // version control
set more off // tells Stata not to pause or display the --more-- message
set cformat %5.3f // specifies the output format of coeff., s.e.,... in tables
capture log close // close any open log-file
set scheme s1mono // set scheme for graphs

cd "/Users/felixreichel/Documents/UNI/WIWI_Bachelor/2023S/IK Econometrics"
cap mkdir ps2 // generate a sub-folder "ps2"

log using "ps2/ps2", replace

use "http://www.econ.jku.at/t3/staff/zweimueller/IntMetrics/ik/data/caschool.dta", clear 

describe
summarize

	
corr score clsize
pwcorr score clsize, sig star(0.05)

twoway (scatter score clsize)
twoway (scatter score clsize, jitter(5)) (lfit score clsize)

reg score clsize
reg score clsize, robust

/*
 _cons: 698.933
	The beta_0_hat/constant/intercept is interesting in this model, since it is 
	statistically sig. different from 0.
	The 95%-CI of the intercept is about 40 score points.

If the zero zero conditional mean condition E(ui|xi) = 0 holds, it implies that the
error term ui is 0 given any xi, which implies that ui is independent of xi.

In the given context:
E( ui | clsize = 15 ) =
E( ui | clsize = 20 ) =
E( ui | clsize = 25 ) = 0

Since there might be other variables which are dependent on clsize and
have an effect on score (e.g. confounder), E(ui|clsize) = 0 can not hold. 

=> Therefore it follows that we can not interpret beta_1_hat as causal.

The R^2 of this simple linear model is only 0.0512,
which means only about 5% of the variation is explained by the simple population model.
*/

pwcorr el_pct score, sig star(0.05)
pwcorr el_pct clsize, sig star(0.05)

/*
	Now estimate the augmented model,
	score_i = β0 + β1 * clsize_i + β2 * el_pct_i + u_i
*/

reg score clsize el_pct
reg score c.clsize##c.el_pct, robust

// Calculate τ manually based on our regression results:
di _b[clsize] + _b[c.clsize#c.el_pct] * r(mean)

// The margins command gives the marginal effect τ and its standard error:
margins, dydx(el_pct) at((mean) clsize)

// summarize clsize
// qui margins, dydx(el_pct) at(clsize=(? (?) ?))
// qui marginsplot, plotopts(lwidth(*2) lcolor(green)) recast(line) ciopts(recast(rline) lcolor(green) lpattern(dash)) yline(0, lpattern(solid)) ylabel(,nogrid) title("") ytitle("Marginal effect of el_pct") xtitle("clsize") plotregion(style(none))

// scorei = β0 + β1 * clsize_i + β2 * el_pct_i + β3 * comp_stu_i + β4 * avginc_i + u_i
reg score clsize el_pct comp_stu avginc

ssc install estout, replace

eststo mlr1: reg score clsize el_pct
eststo mlr2: reg score clsize el_pct comp_stu avginc, robust
esttab mlr1 mlr2, cells(b(fmt(3)) se(par star fmt(4))) ///
	r2(%9.4g) ar2(%9.4g) starlevel(* 0.05 ** 0.01) noobs ///
	mlabels("MLR-1" "MLR-2") ///
	postfoot("significant at 1%-level (**), 5% level (*)")

su score clsize el_pct comp_stu avginc
regress score clsize el_pct comp_stu avginc

// beta coefficients:
/*  comp_stu: 13.818 * .0649558 / 19.05335 = 0.471
    avginc: 1.484 * 7.22589 / 19.05335  = 0.563 
	
	
If comp_stu increases by 1 standard deviations, the score increases by 0.471 standard deviations. 
If avginc increases by 1 standard deviation , the score increases by 0.563 standard deviations.
 */	
			
scatter score avginc

twoway(scatter score avginc) (lfit score avginc)
twoway(scatter score avginc) (qfit score avginc)
/* Answer: Looking at the scatter plot, data suggets to fit for a
 quadratic relationship between income and test scores.
*/

gen ln_avginc = ln(avginc)
label variable ln_avginc "Log Average Income (in USD 1,000)"

twoway(scatter score ln_avginc) (lfit score ln_avginc)

eststo mlr3: reg score clsize el_pct comp_stu ln_avginc
esttab mlr2 mlr3, p

cap log close
clear
