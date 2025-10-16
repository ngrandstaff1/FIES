******************************************
/*Food security Experience Scale (FIES)*/
******************************************
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis5.dta", replace


** SOURCES
* (1) https://www.fao.org/in-action/voices-of-the-hungry/using-fies/en/
  (2) https://www.fao.org/in-action/voices-of-the-hungry/analyse-data/en/
  
  
** DATA FORMATTING
drop worried-whlday
* Add descriptions
label variable WORRIED "Worried that there would not be enough food to eat"
label variable HEALTHY "Unable to eat healthy and nutritious food"
label variable FEWFOOD "Ate only a few kinds of foods"
label variable SKIPPED "Had to skip a meal"
label variable ATELESS "Ate less than you thought you should"
label variable RUNOUT "Household ran out of food"
label variable HUNGRY "Hungry but did not eat"
label variable WHLDAY "Went a whole day and night without eating"
* Check for missing data
tab WORRIED, missing
tab HEALTHY, missing
tab FEWFOOD, missing
tab SKIPPED, missing
tab ATELESS, missing
tab RUNOUT, missing
tab HUNGRY, missing
tab WHLDAY, missing


** CALCULATING RAW SCORE
* Generate and label a variable Raw_Score
gen Raw_Score = .
label variable rawScore "FIES Raw Score"
* Calculate the raw score by country based on remaining items via RM.weights, the Word template as well as the Excel template to calculate the prevalence rates

/* IF MORE OR FEWER FIES QUESTIONS ASKED IN ONE COUNTRY
replace rawScore=WORRIED + FEWFOOD + SKIPPED + ATELESS + RUNOUT + HUNGRY + WHLDAY if country==1 & !missing(WORRIED) & !missing(FEWFOOD) & !missing(SKIPPED) & !missing(ATELESS) & !missing(RUNOUT) & !missing(HUNGRY) & !missing(WHLDAY)

replace rawScore = WORRIED + FEWFOOD + SKIPPED + ATELESS + RUNOUT + HUNGRY + WHLDAY if country==2 & !missing(WORRIED) & !missing(FEWFOOD) & !missing(SKIPPED) & !missing(ATELESS) & !missing(RUNOUT) & !missing(HUNGRY) & !missing(WHLDAY)

replace rawScore = WORRIED + HEALTHY + FEWFOOD + SKIPPED + ATELESS + RUNOUT + HUNGRY + WHLDAY if country==3 & !missing(WORRIED) & !missing(HEALTHY) & !missing(FEWFOOD) & !missing(SKIPPED) & !missing(ATELESS) & !missing(RUNOUT) & !missing(HUNGRY) & !missing(WHLDAY)

replace rawScore = WORRIED + SKIPPED + ATELESS + RUNOUT + HUNGRY + WHLDAY if country==4 & !missing(WORRIED) & !missing(SKIPPED) & !missing(ATELESS) & !missing(RUNOUT) & !missing(HUNGRY) & !missing(WHLDAY)

* check distribution of the calculated raw score
tab Raw_Score country, mi
tab Raw_Score country
*/

set trace on
foreach x in WORRIED HEALTHY FEWFOOD SKIPPED ATELESS RUNOUT HUNGRY WHLDAY {
	forvalues i = 1/205 {
		tab `x' if WP5==`i'
		tab WORRIED if WP5 == 1, missing matcell(x)
		matrix list x
		scalar a = x[3,1] if "`x'" == "WORRIED"
		scalar b = x[3,1] if "`x'" == "HEALTHY"
		scalar c = x[3,1] if "`x'" == "FEWFOOD"
		scalar d = x[3,1] if "`x'" == "SKIPPED"
		scalar e = x[3,1] if "`x'" == "ATELESS"
		scalar f = x[3,1] if "`x'" == "RUNOUT"
		scalar g = x[3,1] if "`x'" == "HUNGRY"
		scalar g = x[3,1] if "`x'" == ""
		
		
	}
	
	gen `x'_dif2 = 0
}


tab WORRIED
tab HEALTHY
tab FEWFOOD
tab SKIPPED
tab ATELESS
tab RUNOUT
tab HUNGRY
tab WHLDAY

set trace off





**# Bookmark #1

* categorize the households into food security groups based on the raw score (according to the calculated thresholds by the official Excel template)
gen FIES=.
label variable FIES "FIES: Food insecurity experience scale"

replace FIES=0 if country==1 & inlist(rawScore, 0,1,2)
replace FIES=1 if country==1 & inlist(rawScore, 3,4,5,6)
replace FIES=2 if country==1 & rawScore==7

replace FIES=0 if country==2 & inlist(rawScore, 0,1,2,3)
replace FIES=1 if country==2 & inlist(rawScore, 4,5,6)
replace FIES=2 if country==2 & rawScore==7

replace FIES=0 if country==3 & inlist(rawScore, 0,1,2,3)
replace FIES=1 if country==3 & inlist(rawScore, 4,5,6,7)
replace FIES=2 if country==3 & rawScore==8

replace FIES=0 if country==4 & inlist(rawScore, 0,1,2)
replace FIES=1 if country==4 & inlist(rawScore, 3,4,5)
replace FIES=2 if country==4 & rawScore==6

label define fies 0"food secure + mildly food insecure" 1"moderately food insecure" 2"severely food insecure"

label values FIES fies

* check the distribution of the FIES variable
tab FIES country, mi