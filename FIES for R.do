*************************************
** USING STATA TO MAKE CSV'S FOR R **
*************************************

** Subsets - ALL YEARS, ALL COUNTRIES
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis4.dta", replace
foreach x in worried healthy fewfood skipped ateless runout hungry whlday {
	replace `x' ="" if `x'=="NA"	
}
destring worried healthy fewfood skipped ateless runout hungry whlday, generate(q1 q2 q3 q4 q5 q6 q7 q8)
gen WORRIED = q1
gen HEALTHY = q2 
gen FEWFOOD = q3
gen SKIPPED = q4
gen ATELESS = q5
gen RUNOUT = q6
gen HUNGRY = q7
gen WHLDAY = q8
drop q1 q2 q3 q4 q5 q6 q7 q8
keep WPID_RANDOM wt YEAR_WAVE WP5 RS WORRIED HEALTHY FEWFOOD SKIPPED ATELESS RUNOUT HUNGRY WHLDAY
save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis5.dta", replace
export delimited using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Do Files\Analysis\Subsets for R\testfile.csv", replace
export delimited using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\R Code\testfile.csv", replace
clear


** Subsets - ALL YEARS, COVER 6
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis5.dta", replace
forvalues i = 2014/2019 {
	gen y`i' = 0
	replace y`i' = 1 if YEAR_WAVE == `i'
}
set trace on
forvalues i = 1/205 {
	forvalues j = 2014/2019 {
		sum y`j' if WP5 == `i'
		drop if WP5 == `i' & r(mean) == 0
	}
}
tab WP5              // CHECK IF 74 of 158 (47 percent) -- YES
drop y2014-y2019
export delimited using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Do Files\Analysis\Subsets for R\cover6.csv", replace
export delimited using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\R Code\cover6.csv", replace
clear


** Subsets - ALL YEARS, COVER 5 drop 2014
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis5.dta", replace
forvalues i = 2014/2019 {
	gen y`i' = 0
	replace y`i' = 1 if YEAR_WAVE == `i'
}
set trace on
drop if YEAR_WAVE == 2014
forvalues i = 1/205 {
	forvalues j = 2015/2019 {
		sum y`j' if WP5 == `i'
		drop if WP5 == `i' & r(mean) == 0
	}
}
tab WP5       // CHECK IF 77 of 158 (49 percent) -- YES
drop y2014-y2019
export delimited using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Do Files\Analysis\Subsets for R\cover5drop14.csv", replace
export delimited using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\R Code\cover5drop14.csv", replace
clear


** Subsets - ALL YEARS, COVER 5 drop 2019
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis5.dta", replace
forvalues i = 2014/2019 {
	gen y`i' = 0
	replace y`i' = 1 if YEAR_WAVE == `i'
}
set trace on
drop if YEAR_WAVE == 2019
forvalues i = 1/205 {
	forvalues j = 2014/2018 {
		sum y`j' if WP5 == `i'
		drop if WP5 == `i' & r(mean) == 0
	}
}
tab WP5				// CHECK IF 118 of 158 (75 percent) -- YES
drop y2014-y2019
export delimited using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Do Files\Analysis\Subsets for R\cover5drop19.csv", replace
export delimited using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\R Code\cover5drop19.csv", replace
clear






















