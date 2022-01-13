// Aufgabe I
// number of days in unemployment
use "http://www.econ.jku.at/t3/staff/ahammer/teaching/data/unemployment.dta", clear
describe

// (a)
gen ui_dur = ui_end - ui_start + 1

// performing a simple linear regression
reg ui_dur ui_amount  // beta_1 = 0.2532924, beta_0/intercept = 38

/* (i): beta_1 Zirkumflex ist der Steidungsparameter der linearen Regressionsgerade.
	Er sagt aus dass je 1 Einheit von ui_amount, ui_dur um beta_1 = 0.25 Einheiten steigen wird.
	=> Sprich die Arbeitslosendauer steigt um 0.25 Tage wenn der Tagessatz um 1 EUR steigt
	
	beta_0 & beta_1 sind beide auf dem Signifikanzniveau von p = .05 statistisch signifikant.
	da z.B. t-Wert 4.49 > 1.96 bzw. das 95% Konfidenzintervall bei beiden Werten 0 nicht einschließt.
	
	(ii): beta_0 Zirkumflex ist da Interzept.
	Es ist der Wert auf der y-Achse wo ui_amount = 0 ist.
	=> Sprich die geschätzte Arbeitslosendauer, wenn der Tagessatz 0 EUR ist.
*/

// (b)
// performing a multivariate linear regression
reg ui_dur ui_amount age // beta_1 = 0.09, beta_2 = 0.34, beta_0 = 30
/*
	(i): beta_2 = 0.34 ist statistisch signifikant.
		 Der Steigungsparameter beta_2 gibt an wie sich die Arbeitslosendauer
		 durch das Alter erklären lässt. (Je Ein Jahr Alter steigt die Arbeitslosendauer um 0.34 Tage)
	
	(ii): beta_1 = 0.09 ist nun nicht mehr statistisch signifikant auf den 0.05-Niveau.
*/

// (c)
gen ui_amount2 = ui_amount^2
reg ui_dur ui_amount ui_amount2 age // beta_1 = -0.37, beta_2 = 0.01, beta_3 = 0.35, beta_0 = 35.94

display -_b[ui_amount]/(2*_b[ui_amount2]) // 22.1

/*
	(i): Da beta_1 < 0 und beta_2 > 0 ist der marginale Effekt u-formig
	(ii): Das Minumum für UI amount liegt bei 22,1 EUR.
*/

// (d)
gen log_ui_dur = log(ui_dur)
gen log_ui_amount = log(ui_amount)

reg log_ui_dur log_ui_amount age female // beta_1 = 0.085, beta_2 = 0.007, beta_3 = 0.0515, beta_0 = 2.835

/*
   (i): Bis auf beta_3 sind alle geschätzten Koeffizienten statistisch signifikant auf dem p=.05-Niveau.
   
   Interpretation beta_1: Je 1 % Tagessatz steigt die Arbeitslosendauer um (100 * 0.085) %
   Interpretation beta_2: Je 1 EH Alter steigt die Arbeitslosendauer um (100 * 0.007) % 
   Interpretation beta_3: Bei der Merkmalsausprägung 1 für female steigt die Arbeitslosendauer um (100* 0.0515) %
						  Interpretation wie für beta_2, female ist aber statischt insignifikant. (Ist eine Binärvariable)
   Interpretation beta_4: Interzept.
*/
reg ui_dur log_ui_amount age female  // beta_1 =  3.9392 (significant)
/*
   (ii): Interpretation beta_1: Wenn ui_amount um 1% steigt verändert sich ui_dur um (3.9392 / 100) Einheiten.
*/

// Aufgabe II
use "http://www.econ.jku.at/t3/staff/ahammer/teaching/data/employment.dta", clear
describe

reg wage female // delta = -35.23, beta_0 = 113.89 (both significant)

// (a) Der geschätzte Koeffizient (signifikant) delta dach besagt, dass wage für Frauen -35.43 EUR geringer ist.
//     Da delta negativ ist, wird das Interzept beta_0 bei Frauen nach unten verschoben.

// (b)
gen female_age = female*age
reg wage female age female_age // delta_0 = 7.937, beta_1 = 1.803, delta_1 = -1.06, beta_0 = 38.877 (all significant)

/*
   (i): Delta_0 gibt den Gehaltsunterschied zwischen Männer und Frauen an der sich nicht durch das Alter und den Interaktionsterm erklären lässt.
		Delta_0 ist positiv, shiftet also das Interzept beta_0 und delta_0 nach oben.
   (ii): Delta_1 ist statistisch signifinkant. 
	     Delta_1 ist negativ, dh Alter schwächt den Effekt den das Geschlecht auf den Gehaltsunterschied hat.
   (iii): Der Gehaltsunterschied zwischen 40-jährigen Männer und Frauen ist rund 34.5 EUR.
		 ~ 110.99 - ~ 76.5 = ~ 34.5
*/

// wage at age = 40 for female 
display 38.877 + 7.937 * 1 + 1.803 * 40 - 1.06 * (40 * 1)

// wage at age = 40 for male 
display 38.877 + 7.937 * 0 + 1.803 * 40 - 1.06 * (40 * 0)

// diff
display (38.877 + 7.937 * 0 + 1.803 * 40 - 1.06 * (40 * 0)) - (38.877 + 7.937 * 1 + 1.803 * 40 - 1.06 * (40 * 1))
