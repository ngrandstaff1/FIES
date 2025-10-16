**************************************
** MEAN FI RATES BY INCOME QUINTILE **
**************************************

** (1) TESTING DIFFERENCE IN QUINTILES RESPONDENTS *****************************
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis7.dta", replace
gen TOP_INC = 1
replace TOP_INC =. if INCOME_5 ==2
replace TOP_INC =. if INCOME_5 ==3
replace TOP_INC =. if INCOME_5 ==4
replace TOP_INC =0 if INCOME_5==1
tab TOP_INC
bysort WP5 YEAR_WAVE: egen TOP_ct = total(TOP_INC)

sort WP5 YEAR_WAVE
quietly by WP5 YEAR_WAVE:  gen dup = cond(_N==1,0,_n)
drop if dup>1
ttest TOP_ct, by(TOP_INC)
drop dup
clear



********************************************************************************
** (2) POPULATION WEIGHTED THINGS **********************************************
********************************************************************************

* Merge population file to the main dataset
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis7.dta", replace
replace ISO3 = "GMB" if WP5 == 186
replace ISO3 = "ROU" if WP5 == 22
replace ISO3 = "COD" if WP5 == 107
replace Region = "Middle East & North Africa" if WP5 == 39
replace Region = "East Asia & Pacific" if WP5 == 69
replace Region = "Latin America & Caribbean" if WP5 == 167
replace Region = "Europe & Central Asia" if WP5 == 202
merge m:1 YEAR_WAVE ISO3 using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Referenced Files\population_WB_others.dta"
tab _merge
tab WP5 if _merge==1
drop if _merge ==2
drop _merge
save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis8.dta", replace


sort WP5 YEAR_WAVE
quietly by WP5 YEAR_WAVE:  gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup

bysort YEAR_WAVE Region: egen total_region_pop = total(total_pop)
sort YEAR_WAVE Region
quietly by YEAR_WAVE Region:  gen dup = cond(_N==1,0,_n)
drop if dup>1
keep YEAR_WAVE Region total_region_pop
save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Referenced Files\population_WB_regtot.dta", replace
clear
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis8.dta", replace
merge m:1 Region YEAR_WAVE using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Referenced Files\population_WB_regtot.dta"
drop _merge

* need mean outcome by country
bysort WP5 INCOME_5: egen wt1_RS_b = mean(RS_b)
bysort WP5 INCOME_5: egen wt1_RS_msb = mean(RS_msb)
bysort WP5 INCOME_5: egen wt1_RS_sb = mean(RS_sb)
bysort WP5 INCOME_5: egen wt1_INCOME = mean(INCOME_4)

* Create and average weights
gen wt_pop = total_pop/total_region_pop
bysort WP5: egen wt_pop_mean = mean(wt_pop)

* multiply by weights
gen wt_RS_b_reg   = wt1_RS_b   * wt_pop_mean
gen wt_RS_msb_reg = wt1_RS_msb * wt_pop_mean
gen wt_RS_sb_reg  = wt1_RS_sb  * wt_pop_mean
gen wt_INCOME_reg = wt1_INCOME * wt_pop_mean

* drop non-unique country-pooled observations
sort WP5 INCOME_5
quietly by WP5 INCOME_5:  gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup

* sum across regions
bysort Region INCOME_5: egen wtA_RS_b   = total(wt_RS_b_reg)
bysort Region INCOME_5: egen wtA_RS_msb = total(wt_RS_msb_reg)
bysort Region INCOME_5: egen wtA_RS_sb  = total(wt_RS_sb_reg)
bysort Region INCOME_5: egen wtA_INCOME  = total(wt_INCOME_reg)

* drop non-unique country-pooled observations
sort Region INCOME_5
quietly by Region INCOME_5:  gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup

* cut unnecessary stuff
keep Region INCOME_5 wtA_RS_b- wtA_RS_sb wtA_INCOME
drop if INCOME_5 ==.









sort Region total_region_pop 
quietly by Region total_region_pop:  gen dup = cond(_N==1,0,_n)
tab dup


* Multiply weight by measure




** COUNTRY-QUINTILE **
* Get group statistics by country-year
* Food insecurity - Binary, Raw Score
bysort WP5 INCOME_5: egen mean_RS_b = mean(RS_b)
bysort WP5 INCOME_5: egen mean_RS_msb = mean(RS_msb)
bysort WP5 INCOME_5: egen mean_RS_sb = mean(RS_sb)

* Food insecurity - Continuous, Rasch Model
bysort WP5 INCOME_5: egen mean_RS_global_FULL = mean(RS_global_FULL)
bysort WP5 INCOME_5: egen median_RS_global_FULL = median(RS_global_FULL)
bysort WP5 INCOME_5: egen mean_RS_global_cover6 = mean(RS_global_cover6)
bysort WP5 INCOME_5: egen median_RS_global_cover6 = median(RS_global_cover6)
bysort WP5 INCOME_5: egen mean_RS_global_drop19 = mean(RS_global_drop19)
bysort WP5 INCOME_5: egen median_RS_global_drop19 = median(RS_global_drop19)

* Income
bysort WP5 INCOME_5: egen mean_INCOME_4 = mean(INCOME_4)
bysort WP5 INCOME_5: egen median_INCOME_4 = median(INCOME_4)

* Covariates - NOT NEEDED



* Create the region-population weight ******* PROBLEM ********
gen wt_pop = (total_pop/total_region_pop)

* Weight Stage 1, by region-quintile
gen wt1_RS_b = wt_pop*mean_RS_b
gen wt1_RS_msb = wt_pop*mean_RS_msb
gen wt1_RS_sb = wt_pop*mean_RS_sb
gen wt1_INCOME_4 = wt_pop*mean_INCOME_4 

* Weight Stage 2, by region-quintile
bysort Region INCOME_5: egen wt2_RS_b = total(wt1_RS_b)
bysort Region INCOME_5: egen wt2_RS_msb = total(wt1_RS_msb)
bysort Region INCOME_5: egen wt2_RS_sb = total(wt1_RS_sb)
bysort Region INCOME_5: egen wt2_INCOME_4 = total(INCOME_4)

bysort INCOME_5: egen wt3_RS_b = total(wt1_RS_b)
bysort INCOME_5: egen wt3_RS_msb = total(wt1_RS_msb)
bysort INCOME_5: egen wt3_RS_sb = total(wt1_RS_sb)
bysort INCOME_5: egen wt3_INCOME_4 = total(INCOME_4)





* Summarize by quintile and region
forvalues i = 1/5 {
	sum wt1_RS_b if RegionA == "SSA" & INCOME_5 == `i'	
}
sum wt1_RS_b if Region == "EAPa" & 

* Weight Stage 2, by region-quintile
** () CALCULATING RATIOS OF INCOMES ACROSS REGIONS
bysort WP5 YEAR_WAVE INCOME_5: egen av_INC_q1 = mean(INCOME_4)
bysort WP5 YEAR_WAVE INCOME_5: egen av_INC_q2 = mean(INCOME_4) 
bysort WP5 YEAR_WAVE INCOME_5: egen av_INC_q3 = mean(INCOME_4) 
bysort WP5 YEAR_WAVE INCOME_5: egen av_INC_q4 = mean(INCOME_4) 
bysort WP5 YEAR_WAVE INCOME_5: egen av_INC_q5 = mean(INCOME_4) 

sort WP5 YEAR_WAVE
quietly by WP5 YEAR_WAVE:  gen dup = cond(_N==1,0,_n)
drop if dup>1
gen ratio_21 = av_INC_q2/av_INC_q1
gen ratio_31 = av_INC_q3/av_INC_q1
gen ratio_41 = av_INC_q4/av_INC_q1
gen ratio_51 = av_INC_q5/av_INC_q1




