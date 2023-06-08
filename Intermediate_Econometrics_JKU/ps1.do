clear // empty working space
version 11 // version control
set more off // tells Stata not to pause or display the --more-- message
set cformat %5.3f // specifies the output format of coeff., s.e.,... in tables
capture log close // close any open log-file
set scheme s1mono // set scheme for graphs

cd "/Users/felixreichel/Documents/UNI/WIWI_Bachelor/2023S/IK Econometrics"
cap mkdir ps1 // generate a sub-folder "ps1"

log using "ps1/ps1", replace

use "http://www.econ.jku.at/t3/staff/zweimueller/IntMetrics/ik/data/share7w5.dta", clear 

	
summarize bmi, detail // displays summary statistics for bmi (Mean, Std. Dev., Percentiles)

histogram bmi, fraction title("BMI distribution") xtitle("Body Mass Index (BMI)")

gen bmicat=1 if bmi<18.5
replace bmicat=2 if bmi>= 18.5 & bmi<25
replace bmicat=3 if bmi>=25 & bmi<30
replace bmicat=4 if bmi>=30
label define bmicat 1 "underweight" 2 "normal weight" 3 "overweight" 4 "obese" // defines value label bmicat
label values bmicat bmicat // assigns value label to variable
label list bmicat // list label bmicat

fre bmicat // Plots a frequency plot of bmicat showing the absolute and the relative distribution of bmicat
display 40.31 + 22.39

ta bmicat, su(bmi) // Displays summary statistics of bmi within each bmicat

su eduyears, detail
su educat, detail
label list educat 

// Plot density plot of eduyears
histogram eduyears, discrete

// Plot density plot of educat
histogram educat, discrete

replace smoking=0 if smoking==5

lab def smoker 0 "No" 1 "Yes", modify
lab val smoking smoker

tab smoking

tab smoking, missing
// For 0.3% of respondents (13 in total) this information is missing

gen emp=1 if lm_stat==2
replace emp=0 if lm_stat!=2

label define emp 0 "unemployed" 1 "employed"
label values emp emp

ttest bmi, by(fborn) // fborn = 1 if not born in AT

/* We test H0: mean(0) - mean(1) = 0 against H1: mean(0) - mean(1) != 0. 
The corresponding two-tailed p-value is 0.0307, which is less than 0.05.
We conclude that the difference of means in bmi between respondents who were born in Austria vs. abroad
is different from 0 -> Reject H0, accept H1/Ha. */ 

tabulate educat bmicat, row

// scatterplot eduyears on x-axis and bmi on y-axis, include a fitted line
twoway (scatter bmi eduyears)
twoway (scatter bmi eduyears, mcolor(gs8) msize(small)) (lfit bmi eduyears, lcolor(dkorange)), xlabel(8(2)20)
			 xtitle("Years of education") ytitle("Body Mass Index") ylabel(,nogrid) 
			 legend(row(1) label(1 "Observed values") label(2 "Fitted line")) plotregion(style(none))

*ssc install binscatter
binscatter bmi eduyears, line() mcolor(gs8) lcolors(dkorange) xlabel(8(2)20) xtitle("Years of education") ytitle("Body Mass Index") ylabel(,nogrid) plotregion(style(none)) title("Binscatter plot")

// Bivariate linear regression model: bmi = alpha0 + alpha1 * eduyears + e 
reg bmi eduyears

mean bmi
display (-0.084/_b[bmi])*100 // -0,31% alpha1 of the sample mean of bmi
display (-0.084/0.074)*100   // -113,51% alpha1 of the sample std. dev. of bmi

reg bmi eduyears
estimates // display last estimation results
ereturn list // display stored results
display "R-squared = " e(r2)

reg bmi eduyears age female fborn emp smoking

rvfplot, yline(0)
estat imtest, white
// prob > chi2 = 0.0714. 
* The H0 of constant variance can NOT be REJECTED at a 5% level of significance.
* The implication of the above finding is that there is NO heteroscedasticity in the residuals.

reg bmi eduyears age female fborn emp smoking, vce(robust)

estat r

sum()
mean()

cap log close
clear
