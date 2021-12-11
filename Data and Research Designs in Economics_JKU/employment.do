use "http://www.econ.jku.at/t3/staff/ahammer/teaching/data/employment.dta", clear

describe

count if missing(wage)

rename class occupation

generate byte bluecollar = occupation == 1
label variable bluecollar "= 1 if blue collar worker"

sort id emp_start

generate tenure = mdy(7, 1, 2020) - emp_start

summarize wage if occupation == 1
return list
scalar define mean_wage_blue_collar = r(mean)
display mean_wage_blue_collar

summarize wage if occupation == 2
return list
scalar define mean_wage_white_collar = r(mean)
display mean_wage_white_collar

count if female == 1

count if female == 0
count if missing(female)

summarize female if female == 1
return list
scalar define n_female = r(N)

describe
return list
scalar define n_total = r(N)

scalar define female_perc = (n_female / n_total) * 100
display female_perc

summarize wage
histogram wage, xlabel( , nogrid) ylabel( , nogrid) scheme(s2color)


 
