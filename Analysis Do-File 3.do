******************************
***   ANALYSIS DO-FILE 3   ***
******************************
* Setting up a continuous dependent variable analysis - all subsets

** Load the data
clear
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis6.dta"

* Establish a globals and locals
global subsets = "FULL cover6 drop14 drop19"
global regionA = "EAPa EuCA LAmC MEAN NAm SAs SSA"
local covars = "age age2 gender_f educ_b i.WP1223 rural HHsize_n child2adult"

* Preliminaries
gen INCOME_42 = INCOME_4^2 // quadratic terms on INCOME_4
gen INCOME_43 = INCOME_4^3 

* Setting up Tobit lower threshold
gen delta = 10^-4
foreach b of global subsets {
    gen c1_`b' = c_`b'
	replace RS_global_`b' = (c1_`b' - delta) if RS_global_`b' == .
	drop c1_`b'
}

* COVARS: Setting up missing values (zeros)
mdesc age age2 gender_f educ_b WP1223 rural HHsize_n child2adult
gen age_mis = age
gen age2_mis = age2

gen educ_b_mis = educ_b
gen WP1223_mis = WP1223
gen rural_mis = rural
gen HHsize_n_mis = HHsize_n
gen child2adult_mis = child2adult
replace age_mis = 0 if age ==.
replace age2_mis = 0 if age2 ==.
replace rural_mis = 1 if WP14 < 3 //Replace with other Urb/rural measure
replace rural_mis = 0 if rural ==. //  Put remaining values as zeros
replace HHsize_n_mis = 0 if HHsize_n ==.
replace child2adult_mis = 0 if child2adult ==.
mdesc age_mis age2_mis educ_b_mis WP1223_mis rural_mis HHsize_n_mis child2adult_mis

* COVARS: Setting up missing values (median OR zeros by country)
bysort WP5: egen age_med = median(age) //problem of full-cou non-report
replace age_med = age 
replace age_med = 0 if age == .

bysort WP5: egen age2_med = median(age2) //problem of full-cou non-report
replace age2_med = age2 
replace age2_med = 0 if age2 == .

bysort WP5: egen rural_med = median(rural)
replace rural_med = rural 
replace rural_med = 0 if rural == .

bysort WP5: egen HHsize_n_med = median(HHsize_n)
replace HHsize_n_med = HHsize_n 
replace HHsize_n_med = 0 if HHsize_n == .

bysort WP5: egen child2adult_med = median(child2adult)
replace child2adult_med = child2adult 
replace child2adult_med = 0 if child2adult == .

mdesc age_med age2_med gender_f educ_b WP1223 rural_med HHsize_n_med child2adult_med

save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis7.dta", replace


** Summary statistics by subset
* Average characteristics by subset
foreach b of global subsets {
    sum RS_global_`b' reliabfl_`b' corr_comm_`b'
}
* Average characteristics by measure (scale, reliability, corr)
sum RS_global*
sum reliabfl*
sum corr_comm*
* Average characteristics by region (tables by subset)
foreach b of global subsets {
	tabstat RS_global_`b' reliabfl_`b' corr_comm_`b', by(Region)
}

* Histograms for Difficulty Parameters
label variable RS_global_FULL "Globally Equated Rasch Scores, Full Sample"
label variable RS_global_cover6 "Globally Equated Rasch Scores, All Years Sample"
label variable RS_global_drop14 "Globally Equated Rasch Scores, Drop 2014 Sample"
label variable RS_global_drop19 "Globally Equated Rasch Scores, Drop 2019 Sample"
foreach b of global subsets {
	hist RS_global_`b' 
}

** Bar Charts for Chacteristics
* Regional Characteristics - food sec
graph bar RS_global_FULL, over(RegionA)
graph bar RS_global_cover6, over(RegionA)
graph bar RS_global_drop14, over(RegionA)
graph bar RS_global_drop19, over(RegionA)

* Income Quintile Characteristics - food sec
foreach r of global regionA {
	graph bar INCOME_4 if RegionA == "`r'", over(INCOME_5)
}





















