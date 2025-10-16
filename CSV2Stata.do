******************************
** CSV to Stata For Weights **   FIXING STUFF FOR THE MS THESIS
******************************
** SET-UP **
* Obtain FULL Equated Data Set (n = 158)
clear
import delimited "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\R Code\R Output\equated_FULL_1a.csv"
keep iso3 t1 t2 t3 t4 t5 t6 t7 t8 prev_fimod prev_fisev
ren (iso3 t1 t2 t3 t4 t5 t6 t7 t8) (ISO3 T1 T2 T3 T4 T5 T6 T7 T8)
replace T1 = "" if T1 == "NA"
replace T2 = "" if T2 == "NA"
replace T4 = "" if T4 == "NA"
replace T5 = "" if T5 == "NA"
replace T6 = "" if T6 == "NA"
replace T7 = "" if T7 == "NA"
replace prev_fimod = "" if prev_fimod == "NA"
replace prev_fisev = "" if prev_fisev == "NA"
destring T1, replace
destring T2, replace
destring T4, replace
destring T5, replace
destring T6, replace
destring T7, replace
destring prev_fimod, replace
destring prev_fisev, replace
save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\R Code\R Output\equated_FULL.dta", replace

* Merge Scale with Surveyed Data (n = 801,483)
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis7.dta", replace
keep WPID_RANDOM wt ISO3 YEAR_WAVE WP5 countrynew INCOME_4 INCOME_5 REG_GLOBAL REG2_GLOBAL RS RegionA freq age_med age2_med gender_f educ_b WP1223 rural_med HHsize_n_med child2adult_med worried healthy fewfood skipped ateless runout hungry whlday Incomegroup

* Missing Data Issue
replace RS = . if worried == "NA" & healthy == "NA" & fewfood == "NA" & skipped == "NA" & ateless == "NA" & runout == "NA" & hungry == "NA" & whlday == "NA"
gen check = 0
replace check = 1 if worried == "NA" & healthy == "NA" & fewfood == "NA" & skipped == "NA" & ateless == "NA" & runout == "NA" & hungry == "NA" & whlday == "NA"
gen n = 1
sort YEAR_WAVE WP5
bysort YEAR_WAVE WP5: egen count = total(n)
bysort YEAR_WAVE WP5: egen check1 = total(check)
gen tab = count-check1
drop if tab == 0
drop worried-whlday
merge m:1 ISO3 using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\R Code\R Output\equated_FULL.dta"
drop _merge

* Establishing lower bound
egen c_FULL = rowmin(T1 - T8)
gen RS_global_FULL = 123456
replace RS_global_FULL = . if RS == .
replace RS_global_FULL = c_FULL if RS == 0
forvalues i = 1/8 {
	replace RS_global_FULL = T`i' if RS == `i'
}

* Check non-response characteristics
gen RS_check = 0
replace RS_check = 1 if RS ==.
corr RS_check INCOME_5 age_med age2_med gender_f educ_b rural_med HHsize_n_med child2adult_med  // no clear associations between non-resp. and covars
drop RS_check

* Cleaning
drop T1-T8 check1 count n

* Saving 
save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis_THESIS.dta", replace
