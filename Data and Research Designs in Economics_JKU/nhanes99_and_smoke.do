// National Health and Nutrition Examination Survey Data
use "http://www.econ.jku.at/t3/staff/ahammer/teaching/data/nhanes99ndi_extr.dta", clear
describe

summarize bmi if inc < 6 | inc == 13 // average bmi = 28.40528
summarize bmi if inc >= 6 & inc <= 9 // average bmi = 28.51541
summarize bmi if inc == 10 | inc == 11 // average bmi = 27.53363

display 28.51541 / 28.40528 * 100 - 100

display 27.53363 / 28.40528 * 100
display 27.53363 / 28.40528 * 100 - 100

reg bloodp bmi female

gen female_bmi = female * bmi // Interaktionsterm
reg bloodp bmi female female_bmi

// II
use "http://www.econ.jku.at/t3/staff/ahammer/teaching/data/SMOKE.dta", clear
describe

gen age2 = age^2
reg cigs age age2

display -_b[age]/(2*_b[age2])

reg cigs income

gen income_educ = income*educ
reg cigs income income_educ

reg cigs restaurn 
reg cigs restaurn income income_educ



