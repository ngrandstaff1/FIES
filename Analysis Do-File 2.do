******************************************************
** EXTRACTING FE COEFFICIENTS FROM TOBIT REGRESSION ** 
******************************************************
clear

** Data **
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis9.dta", replace
* save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis9.dta", replace

**********************
*** Model A - FULL ***
**********************
tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 [pw = wt], ll(c_FULL) vce(robust) iterate(100)
** Extract Intercept
scalar yA_cons = e(b)[1,181]
** Extract Year (base 2014)
scalar yA_2015 = e(b)[1,18]
scalar yA_2016 = e(b)[1,19]
scalar yA_2017 = e(b)[1,20]
scalar yA_2018 = e(b)[1,21]
scalar yA_2019 = e(b)[1,22]
* Numbering countries (base US)
cap drop _merge cA cyA 
merge m:1 WP5 using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Referenced Files\FixWP5.dta"
** Saving Country FE
gen cA = 0
forvalues i = 2/158 {
	scalar cA_temp = e(b)[1,`i'+22]
	replace cA = cA_temp if cnum == `i'
}
** Add Year FE to Country FE
gen cyA = 0
* Country Fixes
replace cyA = yA_cons if WP5 == 1 & YEAR_WAVE == 2014
replace cyA = yA_cons + yA_2015 if WP5 == 1 & YEAR_WAVE == 2015
replace cyA = yA_cons + yA_2016 if WP5 == 1 & YEAR_WAVE == 2016
replace cyA = yA_cons + yA_2017 if WP5 == 1 & YEAR_WAVE == 2017
replace cyA = yA_cons + yA_2018 if WP5 == 1 & YEAR_WAVE == 2018
* Year Fixes
replace cyA = yA_cons + cA  if YEAR_WAVE == 2014
replace cyA = yA_cons + cA + yA_2015 if YEAR_WAVE == 2015
replace cyA = yA_cons + cA + yA_2016 if YEAR_WAVE == 2016
replace cyA = yA_cons + cA + yA_2017 if YEAR_WAVE == 2017
replace cyA = yA_cons + cA + yA_2018 if YEAR_WAVE == 2018
replace cyA = yA_cons + cA + yA_2019 if YEAR_WAVE == 2019
** Drop Duplicates
sort WP5 YEAR_WAVE cyA
quietly by WP5 YEAR_WAVE cyA:  gen dup = cond(_N==1,0,_n)
tab dup
drop if dup > 1
drop WPID_RANDOM wt wpid FIELD_DATE-INCOME_7 WP1219-YEAR_CALENDAR RS-educ_b reliabfl_* corr_comm* v_c6-ihsI_d19_l dup age
** Save New File
save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Do Files\Analysis\CY_ModelA.dta", replace



*************************
*** Model B - Drop 19 ***
*************************
clear
** New Data and Subsetting
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis9.dta", replace
drop if RS_global_drop19_alt == .
drop if YEAR_WAVE == 2019
drop if WP51 == "Mauritania"
** Running Model B
tobit RS_global_drop19_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 [pw = wt], ll(c_drop19) vce(robust) iterate(100)
** Extract Intercept
scalar yB_cons = e(b)[1,138]
** Extract Year (base 2014)
scalar yB_2015 = e(b)[1,18]
scalar yB_2016 = e(b)[1,19]
scalar yB_2017 = e(b)[1,20]
scalar yB_2018 = e(b)[1,21]
** Extract Country FE
sort WP5
egen bin = group(WP5) 
gen cB = 0
set trace on
forvalues i = 2/116 {
	scalar cB_temp = e(b)[1,`i'+21]
	replace cB = cB_temp if bin == `i'
}
set trace off







** Add Year FE to Country FE
gen cyB = 0
 * Country Fixes
replace cyB = yB_cons if WP5 == 1 & YEAR_WAVE == 2014
replace cyB = yB_cons + yB_2015 if WP5 == 1 & YEAR_WAVE == 2015
replace cyB = yB_cons + yB_2016 if WP5 == 1 & YEAR_WAVE == 2016
replace cyB = yB_cons + yB_2017 if WP5 == 1 & YEAR_WAVE == 2017
replace cyB = yB_cons + yB_2018 if WP5 == 1 & YEAR_WAVE == 2018
* Year Fixes
replace cyB = yB_cons + cB  if YEAR_WAVE == 2014
replace cyB = yB_cons + cB + yB_2015 if YEAR_WAVE == 2015
replace cyB = yB_cons + cB + yB_2016 if YEAR_WAVE == 2016
replace cyB = yB_cons + cB + yB_2017 if YEAR_WAVE == 2017
replace cyB = yB_cons + cB + yB_2018 if YEAR_WAVE == 2018
** Drop Duplicates
sort WP5 YEAR_WAVE cyB
quietly by WP5 YEAR_WAVE cyB:  gen dup = cond(_N==1,0,_n)
tab dup
drop if dup > 1
drop WPID_RANDOM wt wpid FIELD_DATE-INCOME_7 WP1219-YEAR_CALENDAR RS-educ_b reliabfl_* corr_comm* v_c6-ihsI_d19_l dup age
** Save New File
save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Do Files\Analysis\CY_ModelB.dta", replace
clear

*******************************************************
** MERGING FE-EXTRACTION FILES WITH OTHER REGRESSORS ** 
*******************************************************
* Data - MODEL A (Full)
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Do Files\Analysis\CY_ModelA.dta", replace
cap drop _merge

* Merge with Covariate Dataset
merge 1:1 ISO3 YEAR_WAVE using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Referenced Files\FE_COVARIATES.dta"
drop if _merge == 1 
drop if _merge == 2
drop _merge
destring Av_ProteinSupplyMA, replace
destring Cereal_Dependency Politic_Stability Cereal_Share, replace


**************
** Analysis **
**************
foreach i in Labor_Q Capital_Q Machinery_Q {
	gen log_`i' = log(`i')
}

* Multiple
reg cyA urbanperc_ GDPpc_lag_ tot_investment_  Av_ProteinSupplyMA Cereal_Dependency Politic_Stability Cereal_Share, vce(cluster ISO3)
estimates store mult

reg cyA TFP_Index Input_Index Labor_Index Capital_Index log_Labor_Q log_Capital_Q log_Machinery_Q, beta

reg cyA urbanperc_ GDPpc_lag_ tot_investment_  Av_ProteinSupplyMA Cereal_Dependency Politic_Stability Cereal_Share TFP_Index Input_Index Labor_Index Capital_Index log_Labor_Q log_Capital_Q log_Machinery_Q, vce(cluster ISO3)

reg cyA urbanperc_ GDPpc_lag_ Av_ProteinSupplyMA Cereal_Dependency TFP_Index Input_Index Labor_Index Capital_Index log_Labor_Q log_Capital_Q log_Machinery_Q i.YEAR_WAVE, vce(cluster ISO3)



* Bivariate
foreach var in urbanperc_ GDPpc_lag_ tot_investment_ emprate_ Av_ProteinSupplyMA Cereal_Dependency Politic_Stability Cereal_Share {
	regress cyA `var' i.YEAR_WAVE
	estimates store `var'
}
coefplot (urbanperc_ GDPpc_lag_ tot_investment_ Av_ProteinSupplyMA Cereal_Dependency Politic_Stability Cereal_Share, label(bivariate)) ///
          (mult)                            ///
          , drop(_cons) xline(0)
foreach var in TFP_Index Input_Index Labor_Index Capital_Index log_Labor_Q log_Capital_Q log_Machinery_Q {
	regress cyA `var' i.YEAR_WAVE
	estimates store `var'
}



* Comparison of shifts
reg cyA urbanperc_ GDPpc_lag_ tot_investment_  Av_ProteinSupplyMA Cereal_Dependency Politic_Stability Cereal_Share, beta

	  


























