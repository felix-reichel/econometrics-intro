use "http://www.econ.jku.at/t3/staff/ahammer/teaching/data/employment_extended.dta", clear
describe 

summarize(wage), detail 		  // 3.257.743 Beobachtungen, Mittelwert 97.89, Missings sind enkodiert mit .
summarize(age), detail 			  // 4.108.692 Beobachtungen, Mittelwert 41.32
summarize(nkids), detail 		  // 4.108.692 Beobachtungen, Mittelwert 0.6
summarize(healthcare_exp), detail // 630.557 Beobachtungen, Mittelwert 714.79, Missings sind enkodiert mit .

hist nkids, percent bin(16)
hist age, percent bin(16)
kdensity wage, xtitle("Tagessatz in EUR")
kdensity healthcare_exp, xtitle("J�hrliche Gesundheitsausgaben in EUR")

// Aufgabe I 
correlate nkids wage
pwcorr nkids wage, sig
twoway (scatter nkids wage, color(blue))(lfit nkids wage, color(red))

reg wage nkids // wage ~ nkids
reg wage nkids age

gen nkids2 = nkids^2
reg wage nkids nkids2

display -_b[nkids]/(2*_b[nkids2])

qui reg wage nkids##nkids##nkids
margins, at(nkids=(0(1)12)) noatlegend
marginsplot, recast(line) plotopts(lwidth(*2) lcolor(black)) recastci(rarea) ciopt(lwidth(none))

summarize wage if nkids == 0 // mean = 94,26916 EUR
summarize wage if nkids >= 1  // mean = 105,601 EUR
display 105.6011 - 94.26916 


summarize wage if female == 0 // mean = 113,89
summarize wage if female == 1 // mean = 78,664
display 113.8948 - 78.66408
	
summarize wage if educ == 4 // mean = 115,87
summarize wage if educ <= 3 // mean = 102,157
display 115.8568 - 102.1568

reg wage nkids age educ
reg wage nkids age educ female 

// Aufgabe II
twoway (scatter healthcare_exp wage, color(blue))(lfit healthcare_exp wage, color(red))

gen log_wage = log(wage) 
reg healthcare_exp wage
reg healthcare_exp log_wage

/*
Der 10-er Logartithmus ist f�r Zahlen log10(x < 10) < 1 und f�r log10(x < 1) < 0, also negativ
Bei Missings in den Daten wird kein Wert berechnet.
Sieht man sich die Formel f�r den Sch�tzer von beta_1 f�r die OLS-Regression und die Daten an,
wird auch klar warum der Reg.-koeffizient auch negativ sein kann.

Beta_1_dach ist definiert als : Cov(X,Y) / Stdev(X)^2

Wenn wir den Regressionskoeffizienten sinnvoll interpretieren m�chten, k�nnten wir einfach +1 dazuz�hlen, dann liefert
der Logarithmus n�mlich nur noch positive Werte zur�ck und dann passt die Kovarianz die f�r beta_1_dach berechnet wird.
*/

gen log_healthcare_exp_add_one = log(healthcare_exp + 1) 
gen age2 = age^2
reg log_healthcare_exp_add_one log_wage age age2 female

reg log_healthcare_exp_add_one log_wage age age2 female if class == 1		
reg log_healthcare_exp_add_one log_wage age age2 female if class == 2
		
/*
F�r Angestellte sind alle Regressionskoeffizienten (bis auf Age, siehe unten) geringer als f�r Arbeiter.

Die Effekte der einzelnen Variablen auf Gesundheitsausgaben sind nicht mehr so gro�.

Die invers-U-f�rmige Parabel f�r das Alter steigt bei den Angestellten nicht so schnell an, 
weil f�r das 2. Regressionsmodell verglichen mit dem 1. Regressionsmodell:

	0 > beta_3_dach_2 > beta_3_dach_1 und beta_2_dach1 > beta_2_dach2 > 0,
gilt.
*/
	
// (d)
/*
(i) Ist beta_1 kausal zu interpretieren?:
Nein ist nicht kausal zu interpretieren.

(ii) M�gliche unbeobachtbare Drittvariable:
- Schlechte Erfahrungen mit dem Gesundheitssystem
- z.B. "Glaube an Alternative Medizin"
- Confounder: Genetik
Gesundheitsaugaben <- Genetik (Intelligenz, Physis, unbeobachtbares etc.) -> Gehalt

(iii) R�ckw�rtskausalit�t m�glich?:
- Ja ist m�glich, wenn mehr Gesundheitsaugaben zu besserer Gesundheit 
f�hren und die Gesundheit Vorraussetzung f�r ein hohes Tagesgeld ist.
*/
// (e)	
gen emp_dur = emp_end - emp_start
reg log_healthcare_exp_add_one log_wage age age2 female emp_dur

	
// Aufgabe III

pwcorr age wage, sig
twoway (scatter age wage, color(blue))(lfit age wage, color(red))

reg wage age 

reg wage age nkids 

reg wage age nkids educ 

reg wage age nkids educ female 

gen educ_female = female * educ
gen nkids_female = female * nkids

reg wage age nkids educ female educ_female 

reg wage age nkids educ female nkids_female

reg wage age nkids educ female educ_female nkids_female 


// Aufgabe IV
summarize healthcare_exp if educ == 1 	// Mittelwert: 973,80
summarize healthcare_exp if educ == 2 	// Mittelwert: 935,56
summarize healthcare_exp if educ == 3 	// Mittelwert: 750,96
summarize healthcare_exp if educ == 4 	// Mittelwert: 624,84

reg healthcare_exp educ 
reg healthcare_exp educ age age2
reg healthcare_exp educ age2 nkids
reg healthcare_exp educ age age2 nkids female nkids_female 
reg healthcare_exp educ age2 nkids female nkids_female

display -_b[age2]/(2*_b[age2])

/*
Findings:

* Die Gesundheitsausgaben sinken bei besserer Ausbildung
* Die Gesundheitsausgaben in Abh�ngigkeit vom Alter k�nnen am besten quadratisch positiv, 
  wobei der Koeffizient von age dann nicht mehr stat. signifikant w�re, modelliert werden.
* F�r Kinder steigen die Gesundheitsausgaben
* F�r Frauen steigen die Gesundheitsausgaben
* F�r Frauen mit Kind sinken die Gesundheitsaugaben, jedoch pro Kind mehr als sie pro Kind steigen

=> Kann z.B. hei�en, dass Frauen, welche mehr Kinder bekommen tendenziell ges�nder sind
und daher weniger Gesundheitsausgaben haben und nicht weil sie Kinder bekommen

Bsp. f�r Confounder (Unbeobachtbare Drittvariable):
* Intelligenz wirkt auf die Gesundheitsausgaben und Bildung gleichzeitig und kann nicht gemessen werden.

Bsp. f�r R�ckw�rtskausalit�t:
* Frauen die ges�nder sind bekommen tendenziell mehr Kinder und Kinder f�hren daher nicht zu niedrigeren Gesundheitsausgaben bei Frauen.

Das Problem in dem Modell sind m�gliche Confounder und R�ckw�rtskausilit�t.
=> Daher ist der Effekt nicht kausal interpretierbar.

*/

