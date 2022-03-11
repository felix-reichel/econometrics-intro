use "http://www.econ.jku.at/t3/staff/ahammer/teaching/data/employment_extended.dta", clear
describe 

summarize(wage), detail 		  // 3.257.743 Beobachtungen, Mittelwert 97.89, Missings sind enkodiert mit .
summarize(age), detail 			  // 4.108.692 Beobachtungen, Mittelwert 41.32
summarize(nkids), detail 		  // 4.108.692 Beobachtungen, Mittelwert 0.6
summarize(healthcare_exp), detail // 630.557 Beobachtungen, Mittelwert 714.79, Missings sind enkodiert mit .

hist nkids, percent bin(16)
hist age, percent bin(16)
kdensity wage, xtitle("Tagessatz in EUR")
kdensity healthcare_exp, xtitle("Jährliche Gesundheitsausgaben in EUR")

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
Der 10-er Logartithmus ist für Zahlen log10(x < 10) < 1 und für log10(x < 1) < 0, also negativ
Bei Missings in den Daten wird kein Wert berechnet.
Sieht man sich die Formel für den Schätzer von beta_1 für die OLS-Regression und die Daten an,
wird auch klar warum der Reg.-koeffizient auch negativ sein kann.

Beta_1_dach ist definiert als : Cov(X,Y) / Stdev(X)^2

Wenn wir den Regressionskoeffizienten sinnvoll interpretieren möchten, könnten wir einfach +1 dazuzählen, dann liefert
der Logarithmus nämlich nur noch positive Werte zurück und dann passt die Kovarianz die für beta_1_dach berechnet wird.
*/

gen log_healthcare_exp_add_one = log(healthcare_exp + 1) 
gen age2 = age^2
reg log_healthcare_exp_add_one log_wage age age2 female

reg log_healthcare_exp_add_one log_wage age age2 female if class == 1		
reg log_healthcare_exp_add_one log_wage age age2 female if class == 2
		
/*
Für Angestellte sind alle Regressionskoeffizienten (bis auf Age, siehe unten) geringer als für Arbeiter.

Die Effekte der einzelnen Variablen auf Gesundheitsausgaben sind nicht mehr so groß.

Die invers-U-förmige Parabel für das Alter steigt bei den Angestellten nicht so schnell an, 
weil für das 2. Regressionsmodell verglichen mit dem 1. Regressionsmodell:

	0 > beta_3_dach_2 > beta_3_dach_1 und beta_2_dach1 > beta_2_dach2 > 0,
gilt.
*/
	
// (d)
/*
(i) Ist beta_1 kausal zu interpretieren?:
Nein ist nicht kausal zu interpretieren.

(ii) Mögliche unbeobachtbare Drittvariable:
- Schlechte Erfahrungen mit dem Gesundheitssystem
- z.B. "Glaube an Alternative Medizin"
- Confounder: Genetik
Gesundheitsaugaben <- Genetik (Intelligenz, Physis, unbeobachtbares etc.) -> Gehalt

(iii) Rückwärtskausalität möglich?:
- Ja ist möglich, wenn mehr Gesundheitsaugaben zu besserer Gesundheit 
führen und die Gesundheit Vorraussetzung für ein hohes Tagesgeld ist.
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
* Die Gesundheitsausgaben in Abhängigkeit vom Alter können am besten quadratisch positiv, 
  wobei der Koeffizient von age dann nicht mehr stat. signifikant wäre, modelliert werden.
* Für Kinder steigen die Gesundheitsausgaben
* Für Frauen steigen die Gesundheitsausgaben
* Für Frauen mit Kind sinken die Gesundheitsaugaben, jedoch pro Kind mehr als sie pro Kind steigen

=> Kann z.B. heißen, dass Frauen, welche mehr Kinder bekommen tendenziell gesünder sind
und daher weniger Gesundheitsausgaben haben und nicht weil sie Kinder bekommen

Bsp. für Confounder (Unbeobachtbare Drittvariable):
* Intelligenz wirkt auf die Gesundheitsausgaben und Bildung gleichzeitig und kann nicht gemessen werden.

Bsp. für Rückwärtskausalität:
* Frauen die gesünder sind bekommen tendenziell mehr Kinder und Kinder führen daher nicht zu niedrigeren Gesundheitsausgaben bei Frauen.

Das Problem in dem Modell sind mögliche Confounder und Rückwärtskausilität.
=> Daher ist der Effekt nicht kausal interpretierbar.

*/

