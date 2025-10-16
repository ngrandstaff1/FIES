/////////////////////////////////////////////////////////////////
/////////////             FIES DO-FILE 3            /////////////
/////////////////////////////////////////////////////////////////

* Carry over data from Do-File 2
clear all
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis1.dta", replace

** Begin Working through FIES Outcome Variables
* Raw Score 1 (RS)
gen RS = 0
replace RS = 1 if rawscore == "1"
replace RS = 2 if rawscore == "2"
replace RS = 3 if rawscore == "3"
replace RS = 4 if rawscore == "4"
replace RS = 5 if rawscore == "5"
replace RS = 6 if rawscore == "6"
replace RS = 7 if rawscore == "7"
replace RS = 8 if rawscore == "8"

* Raw Score 2 (RS_par) - the normalized version
gen RS_par = raw_score_par
replace RS_par = "" if RS_par =="NA"
destring RS_par, replace

* Raw Score 2 Moderate or Severe Binary (RS_msb)
gen RS_msb = 0
replace RS_msb=1 if RS > 3

* Raw Score 2 Severe Binary (RS_sb)
gen RS_sb = 0
replace RS_sb=1 if RS >6

* Probability of Moderate or Severe Food Insecurity
gen PROB_mod_sev = prob_mod_sev
replace PROB_mod_sev = "" if PROB_mod_sev =="NA"
destring PROB_mod_sev, replace

* Probability of Severe Food Insecurity
gen PROB_sev = prob_sev
replace PROB_sev = "" if PROB_sev =="NA"
destring PROB_sev, replace

* Converting adult and child counts to numeric from the FIES data
gen adult_n = n_adults
replace adult_n = "" if adult_n == "NA"
destring adult_n, replace

gen child_n = n_child
replace child_n = "" if child_n == "NA"
destring child_n, replace

* Check if measures of household size are roughly consistent- 98.5% w/o error
gen HHsize_n = child_n + adult_n
gen check = HHsize - HHsize_n
hist check
drop check HHsize_n

** Other corrections
drop WP12509 WP9081 WP9083 WP3675 WP3676 WP3677

save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis2.dta", replace














