**********************
** ESTIMATING COSTS **
**********************
clear

** Data **
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis9.dta", replace

** Model
tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 [pw = wt], ll(c_FULL) vce(robust) iterate(100)

** Extract beta
scalar beta = e(b)[1,1]
scalar beta1 = beta*2.2  // IHST 

** One percent of income 
gen INCOME_perc = INCOME_4/100

** Indicators
gen indicator1 = 0 // for threshold set at ANY (c_FULL)
replace indicator1 = 1 if RS>0
gen indicator2 = 0 // for threshold set at -0.38 (FEWFOODS)
replace indicator2 = 1 if RS_global_FULL_alt > -0.38
gen indicator3 = 0 // for threshold set at 1.18 (WHLDAY)
replace indicator3 = 1 if RS_global_FULL_alt > 1.18

** Differences
gen diff1 = indicator1*(c_FULL - RS_global_FULL_alt)
gen diff2 = indicator2*(-0.38 - RS_global_FULL_alt)
gen diff3 = indicator3*(1.18 - RS_global_FULL_alt)

** Individual costs across the sample
gen sum1 = (diff1/beta1)*INCOME_perc*wt
bysort YEAR_WAVE WP5: egen cost1_cy = total(sum1)
gen sum2 = (diff2/beta1)*INCOME_perc*wt
bysort YEAR_WAVE WP5: egen cost2_cy = total(sum2)
gen sum3 = (diff3/beta1)*INCOME_perc*wt
bysort YEAR_WAVE WP5: egen cost3_cy = total(sum3)

** Sample Size
gen n = 1
bysort YEAR_WAVE WP5: egen samp_size = count(n)
drop n

** Population size
merge m:1 ISO3 YEAR_WAVE using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Referenced Files\population_WB_others.dta"
drop _merge

** Cost per person
gen cost1_j = (cost1_cy/samp_size)*total_pop
gen cost2_j = (cost2_cy/samp_size)*total_pop
gen cost3_j = (cost3_cy/samp_size)*total_pop

** Removing Duplicates
sort WP5 YEAR_WAVE
quietly by WP5 YEAR_WAVE: gen dup = cond(_N==1,0,_n)
drop if dup>1
drop if WPID_RANDOM == .

** Analysis
gen c2_pp = cost2_j/total_pop
gen c3_pp = cost3_j/total_pop


sum cost2_j if RegionA == "EAPa"
sum cost2_j if RegionA == "EuCA"
sum cost2_j if RegionA == "LAmC"

sum cost2_j if RegionA == "MEAN"

sum cost2_j if RegionA == "NAm"

sum cost2_j if RegionA == "SAs"
sum cost2_j if RegionA == "SSA"






















