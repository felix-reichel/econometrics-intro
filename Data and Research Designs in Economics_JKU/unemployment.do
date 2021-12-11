use "http://www.econ.jku.at/t3/staff/ahammer/teaching/data/unemployment.dta", clear

describe

generate ui_dur = ui_end - ui_start + 1

summarize ui_dur, detail
summarize ui_dur if age >= 50 & female == 0, detail 

mean ui_dur if female == 0 // 43.9594   sd=.5762611    .95-conf_lw=42.82961    95-conf_up=45.08919
mean ui_dur if female == 1 //46.93373   sd=.7992998      45.36637     48.5011

ttest ui_dur, by(female) unequal // assuming different variances for the 2 groups

generate exper = exper_d / 365.25

twoway (scatter ui_dur ui_amount, color("49 89 125%25"))

correlate ui_dur ui_amount //  0.0555 

ssc install binscatter

binscatter ui_dur ui_amount

binscatter ui_dur age







