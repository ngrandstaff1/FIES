************************
** MS THESIS ANALYSIS **
************************
clear
** Upload Data
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis_THESIS.dta", replace

** Corrections
replace RegionA = "MEAN" if countrynew == "Palestine"
replace RegionA = "EAPa" if countrynew == "Taiwan"
replace RegionA = "EuCA" if countrynew == "Northern Cyprus"
replace ISO3 = "COD" if ISO3 == "ZAR" // Congo (Kinshasa)
replace ISO3 = "ROU" if ISO3 == "ROM" // Romania
replace ISO3 = "GMB" if ISO3 == "GAM" // The Gambia


//////////////////////////////////////////////////////////////////////
// Summary Statistics on Binary Threshold v. Binary Rasch Threshold //   Y Vars
//////////////////////////////////////////////////////////////////////
* Obtain RS values (standard assumptions, no truncation)
	mdesc RS_global_FULL 
	sum RS_global_FULL , detail
* FULL
	gen RScF_msb = 0
	replace RScF_msb = 1 if RS_global_FULL > -0.394 & RS>0
	gen RScF_sb = 0 
	replace RScF_sb = 1 if RS_global_FULL > 1.649 & RS>0
* Regions
	replace RegionA = "MENA" if countrynew == "Palestine"
	replace RegionA = "EAPa" if countrynew == "Taiwan"
	replace RegionA = "LAmC" if countrynew == "Puerto Rico"
	replace RegionA = "EuCA" if countrynew == "Northern Cyprus"
	tabulate RegionA, generate(Reg)
	cap drop RegionN
	gen RegionN = 0
	forvalues i = 1/7 {
		replace RegionN = `i' if Reg`i' == 1	
}
	drop Reg1-Reg8
** Checks on Covariates
	* Pooled
	sum age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med
	mdesc age_med age2_med gender_f educ_b WP1223 rural_med HHsize_n_med child2adult_med
* By Region
	forvalues i = 0/7 {
		display "CUT"
		display "CUT"
		sum age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med if RegionN == `i'	
	}
* By GDP group
	sum age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med if Incomegroup == "High income"
	sum age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med if Incomegroup == "Upper middle income"	
	sum age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med if Incomegroup == "Lower middle income"
	sum age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med if Incomegroup == "Low income"	
* By Region-Year
	forvalues i = 0/7 {
		forvalues j = 2014/2019 {
		display "CUT"
		display "CUT"
		sum age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med if RegionN == `i' & YEAR_WAVE == `j'
		}
	}
	
		
///////////////////////////////////////
// Context Analysis; Broadly Defined //                    bins = 3
///////////////////////////////////////
** CROSS-REGIONS Thresholds **
	* Generate necessary variables
		cap drop log_INCOME_4
		gen log_INCOME_4 = log(1 + INCOME_4)
** Creating Groups - Geography, none work even for tertiles
** Creating Groups - GDP, only works for tertiles
	* Binning
		cap drop quint1
		egen quint1 = xtile(INCOME_4), n(3) by(Incomegroup)
		forvalues i = 2/3 {
			gen q`i' = log_INCOME_4 if quint1 == `i'
			replace q`i' = 0 if quint1 != `i'
			gen Q`i' = 1 if quint1 == `i'
			replace Q`i' = 0 if quint1 != `i'
		}
	
	* MODELLING - Pooled *******************************************************	
		tobit RS_global_FULL log_INCOME_4 Q2 Q3 q2 q3 age_med age2_med gender_f ///
		educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 ///
		[pw = wt], ll(c_FULL) vce(robust)	
		testparm q2 q3 // F-test 
		margins , dydx(log_INCOME_4 q2 q3) predict(ys(c_FULL, 999999))
		forvalues i = 1/3 {
			scalar b`i' = r(table)[1,`i']
			scalar s`i' = r(table)[2,`i']
		}
		scalar B1 = b1 
		scalar B2 = b1 + b2 
		scalar B3 = b1 + b3 
		di B1 B2 B3 // coefficients
		di s1 s2 s3 // s.e. - Note they aren't adjusted for additive transform
		di e(N)     // total number observations in this regression
		di e(N_lc)  // number observations left-censored
	* MODELLING - Low income ***************************************************
		quietly tobit RS_global_FULL log_INCOME_4 Q2 Q3 q2 q3 age_med age2_med gender_f ///
		educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 ///
		if Incomegroup =="Low income" [pw = wt], ll(c_FULL) vce(robust)	
		testparm q2 q3 // F-test 
		margins , dydx(log_INCOME_4 q2 q3) predict(ys(c_FULL, 999999))
		forvalues i = 1/3 {
			scalar b`i' = r(table)[1,`i']
			scalar s`i' = r(table)[2,`i']
		}
		scalar B1 = b1 
		scalar B2 = b1 + b2 
		scalar B3 = b1 + b3 
		di B1 B2 B3 // coefficients
		di s1 s2 s3 // s.e. - Note they aren't adjusted for additive transform
		di e(N)     // total number observations in this regression
		di e(N_lc)  // number observations left-censored
			gen B_low = 0
			replace B_low = B1 if quint1 == 1
			replace B_low = B2 if quint1 == 2
			replace B_low = B3 if quint1 == 3
	* MODELLING - Lower middle income ******************************************
		quietly tobit RS_global_FULL log_INCOME_4 Q2 Q3 q2 q3 age_med age2_med gender_f ///
		educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 ///
		if Incomegroup =="Lower middle income" [pw = wt], ll(c_FULL) vce(robust)
		testparm q2 q3 // F-test 
		margins , dydx(log_INCOME_4 q2 q3) predict(ys(c_FULL, 999999))
		forvalues i = 1/3 {
			scalar b`i' = r(table)[1,`i']
			scalar s`i' = r(table)[2,`i']
		}
		scalar B1 = b1 
		scalar B2 = b1 + b2 
		scalar B3 = b1 + b3 
		di B1 B2 B3 // coefficients
		di s1 s2 s3 // s.e. - Note they aren't adjusted for additive transform
		di e(N)     // total number observations in this regression
		di e(N_lc)  // number observations left-censored
			gen B_lm = 0
			replace B_lm = B1 if quint1 == 1
			replace B_lm = B2 if quint1 == 2
			replace B_lm = B3 if quint1 == 3
	* MODELLING - Upper middle income ******************************************
		quietly tobit RS_global_FULL log_INCOME_4 Q2 Q3 q2 q3 age_med age2_med gender_f ///
		educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 ///
		if Incomegroup =="Upper middle income" [pw = wt], ll(c_FULL) vce(robust)	
		testparm q2 q3 // F-test 
		margins , dydx(log_INCOME_4 q2 q3) predict(ys(c_FULL, 999999))
		forvalues i = 1/3 {
			scalar b`i' = r(table)[1,`i']
			scalar s`i' = r(table)[2,`i']
		}
		scalar B1 = b1 
		scalar B2 = b1 + b2 
		scalar B3 = b1 + b3 
		di B1 B2 B3 // coefficients
		di s1 s2 s3 // s.e. - Note they aren't adjusted for additive transform
		di e(N)     // total number observations in this regression
		di e(N_lc)  // number observations left-censored
			gen B_hm = 0
			replace B_hm = B1 if quint1 == 1
			replace B_hm = B2 if quint1 == 2
			replace B_hm = B3 if quint1 == 3
	* MODELLING - High income **************************************************
		quietly tobit RS_global_FULL log_INCOME_4 Q2 Q3 q2 q3 age_med age2_med gender_f ///
		educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 ///
		if Incomegroup =="High income" [pw = wt], ll(c_FULL) vce(robust)	
		testparm q2 q3 // F-test 
		margins , dydx(log_INCOME_4 q2 q3) predict(ys(c_FULL, 999999))
		forvalues i = 1/3 {
			scalar b`i' = r(table)[1,`i']
			scalar s`i' = r(table)[2,`i']
		}
		scalar B1 = b1 
		scalar B2 = b1 + b2 
		scalar B3 = b1 + b3 
		di B1 B2 B3 // coefficients
		di s1 s2 s3 // s.e. - Note they aren't adjusted for additive transform
		di e(N)     // total number observations in this regression
		di e(N_lc)  // number observations left-censored
			gen B_high = 0
			replace B_high = B1 if quint1 == 1
			replace B_high = B2 if quint1 == 2
			replace B_high = B3 if quint1 == 3
	* Planning ahead
		gen B_wrt_RB = 0
		replace B_wrt_RB = B_low if Incomegroup == "Low income"
		replace B_wrt_RB = B_lm if Incomegroup == "Lower middle income"
		replace B_wrt_RB = B_hm if Incomegroup == "Upper middle income"
		replace B_wrt_RB = B_high if Incomegroup == "High income"
		drop B_low B_lm B_hm B_high
	* Save a copy
save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_THESIS_FE.dta", replace	
	* Characteristics by GDP Group
		forvalues i = 1/3 {
			sum INCOME_4 if Incomegroup == "Low income" & quint1 == `i'
		}
		forvalues i = 1/3 {
			sum INCOME_4 if Incomegroup == "Lower middle income" & quint1 == `i'
		}
		forvalues i = 1/3 {
			sum INCOME_4 if Incomegroup == "Upper middle income" & quint1 == `i'
		}
		forvalues i = 1/3 {
			sum INCOME_4 if Incomegroup == "High income" & quint1 == `i'
		}

	
	
/////////////////////////////////////////////////////
// McDonald & Moffitt 1980 Decomposition on Censor //   
/////////////////////////////////////////////////////
** Investigating On/off censor behavior
/* M&M Decompositions

	cap drop xb 
	cap drop error_F
	tobit RS_global_FULL log_INCOME_4 Q2 Q3 q2 q3 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt] , ll(c_FULL) vce(robust)
	predict xb // column of predicted tobit regression model values
	sum xb
	scalar xb_bar = r(mean)  // obtain XB mean
	gen error_F = xb - RS_global_FULL
		label variable error_F "Tobit Model Error, Full Sample"
	hist error_F, kdensity normal // compare densities; nonparam v. normal
	sum error_F
	scalar sd = r(sd) // error standard deviation
	scalar ratio_F = xb_bar/sd // M&M's z value
	scalar pdf_F = normalden(ratio_F)
	scalar cdf_F = normal(ratio_F)
	scalar MM_FULL = [1 - (xb_bar*pdf_F)/cdf_F - (pdf_F)^2/(cdf_F)^2]
	di MM_FULL
	
	
*/
/* On/Off censor results - full/on/off regressions
* FULL - Full
quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt] , ll(c_FULL) vce(robust)
	scalar b1 = r(table)[1,1] // beta for FULL with 
	scalar s1 = r(table)[2,1] 
* Probit
quietly probit RS_b log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt], vce(robust)
	scalar b2 = r(table)[1,1] // beta for FULL with 
	scalar s2 = r(table)[2,1] 
* Off censor
quietly reg RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RS > 0 [pw = wt] , vce(robust)
	scalar b3 = r(table)[1,1] // beta for FULL with 
	scalar s3 = r(table)[2,1] 
di b1 b2 b3 // full / binary / off-censor --> FULL SAMPLE
di s1 s2 s3
*/
/* J.Hoddinott Tobit Considerations:
* FULL
	tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if log_INCOME_4 != . [pw = wt], ll(c_FULL) 
	
	* (1) Marginal effect for the probability of being uncensored
		margins , dydx(log_INCOME_4) predict(p(c_FULL, 999999)) 
	
	* (2) Marginal effects for the expected value of the dependent variable 
		margins , dydx(log_INCOME_4) predict(e(c_FULL, 999999))

	* (3) Marginal effects for the unconditional expected value of the dept var
	*     where y* = max(a, min(y,b))
		margins , dydx(log_INCOME_4) predict(ys(c_FULL, 999999))
		
	* Compare:
	di e(b)[1,1] "---" e(b)[1,2]

*/

	
	
////////////////////////////////////////////////
// Context Analysis; EXTRACTING FIXED EFFECTS //                    bins = 3
////////////////////////////////////////////////
clear
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_THESIS_FE.dta", replace
* Run the model anew with interaction terms on income by country (pooled)
		tobit RS_global_FULL c.log_INCOME_4##i.WP5 age_med age2_med gender_f ///
		educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 ///
		[pw = wt], ll(c_FULL) vce(robust)
	* Modelling
		merge m:1 WP5 using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Referenced Files\FixWP5.dta"
		cap drop _merge
		cap drop _dup
		sort WP5 YEAR_WAVE
		cap drop INCOME_4_med
		bysort YEAR_WAVE WP5: egen INCOME_4_med = median(log_INCOME_4)	
	* Saving Country FE
		scalar beta_stable = e(b)[1,1]
		cap drop cA_c		
		gen cA_c = 0 
		forvalues i = 2/158 {
			scalar cA_temp = e(b)[1,`i'+1]
			replace cA_c = cA_temp if cnum == `i'
		}		
	* Saving Income Coefficients 
		cap drop cA_i
		gen cA_i = 0
		forvalues i = 2/158 {
			scalar cA_temp = e(b)[1,`i'+159]
			replace cA_i = cA_temp if cnum == `i'
		}	
	cap drop cA_3
	gen cA_3 = cA_c + cA_i*INCOME_4_med + beta_stable*INCOME_4_med               // HERE
	* Save a copy
	save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_THESIS_FE.dta", replace
	* Drop Duplicate Values
		sort WP5 YEAR_WAVE INCOME_4_med
		quietly by WP5 YEAR_WAVE INCOME_4_med:  gen dup = cond(_N==1,0,_n)
		drop if dup > 1
		drop cA_i cA_c
	* Quick summary statistics on Model A
		tabstat cA_3, statistics(mean sd) by(RegionA)
		tabstat cA_3, statistics(mean sd) by(Incomegroup)
		hist cA_3, kdensity normal	
	* Save a copy
		save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_THESIS_baselines.dta", replace
	** Obtain and Merge in Data - MODEL (Full; V3)
	clear
	use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_THESIS_baselines.dta", replace
	* Cleaning
		keep YEAR_WAVE countrynew WP5 REG_GLOBAL REG2_GLOBAL Incomegroup ISO3 RegionA cnum cA_3
	* Merge with Covariate Dataset
		merge 1:1 ISO3 YEAR_WAVE using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Referenced Files\FE_COVARIATES.dta"
		drop if _merge == 1 
		drop if _merge == 2
		drop _merge
		destring Av_ProteinSupplyMA, replace
		destring Cereal_Dependency Politic_Stability Cereal_Share, replace
		drop ThreeYRMA
	foreach i in Labor_Q Capital_Q Machinery_Q {
		gen log_`i' = log(`i')
	}
		drop Labor_Q-Machinery	
/* Analysis: Regressing Stuff on Year
	* Groups with robust SE format
		reg cA_3 urbanperc-log_Machinery i.YEAR_WAVE, beta // pooled
		
		reg cA_3 urban GDPpc tot_investment_ emprate_ Politic_Stability i.YEAR_WAVE, beta // structural
		reg cA_3 Av_ProteinSupplyMA Cereal_Dependency Cereal_Share Input_Index-log_Machinery i.YEAR_WAVE, beta // food system

	* Simple and clustered via standardized coefficients
		reg cA_3 urbanperc-log_Machinery i.YEAR_WAVE, beta
		reg cA_3 urban GDPpc tot_investment_ emprate_ Politic_Stability i.YEAR_WAVE, beta 
		reg cA_3 Av_ProteinSupplyMA Cereal_Dependency Cereal_Share Input_Index-log_Machinery i.YEAR_WAVE, beta

		* Structural
		reg cA_3 urban i.YEAR_WAVE, beta
		reg cA_3 GDPpc i.YEAR_WAVE, beta
		reg cA_3 tot_investment_ i.YEAR_WAVE, beta
		reg cA_3 emprate_ i.YEAR_WAVE, beta
		reg cA_3 Politic_Stability i.YEAR_WAVE, beta

		* Food system
		reg cA_3 Av_ProteinSupplyMA i.YEAR_WAVE, beta
		reg cA_3 Cereal_Dependency i.YEAR_WAVE, beta
		reg cA_3 Cereal_Share i.YEAR_WAVE, beta

	* TFP and logged endowments
		reg cA_3 Input_Index-Capital_Index i.YEAR_WAVE, beta
		reg cA_3 log_Labor_Q-log_Machinery i.YEAR_WAVE, beta

*/
clear
	
	
//////////////////////////////////////////////////////////////////
// Estimating Costs for Eliminating Food Insecurity Experiences //   
//////////////////////////////////////////////////////////////////
clear
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis_THESIS.dta", replace

* Catch up
gen log_INCOME_4 = log(1 + INCOME_4)

* HOW MANY BINS
* global bins "5"
* lobal bins "4"
global bins "3"

* Establish new quintiles
cap drop quint
egen quint1 = xtile(INCOME_4), n($bins) by(Incomegroup)
	forvalues i = 1/$bins {
		gen q`i' = log_INCOME_4*1 if quint1 == `i'
		replace q`i' = 0 if quint1 != `i'
		gen r`i' = 1 if quint1 == `i'
		replace r`i' = 0 if quint1 != `i'
	}
	
* Create places to store estimates by quintile
foreach j in Tob LPM1 LPM2 {	
	forvalues i = 1/$bins {
		gen b`i'_`j' = 0
	}
}

* Population data
replace ISO3 = "COD" if ISO3 == "ZAR" // Congo (Kinshasa)
replace ISO3 = "ROU" if ISO3 == "ROM" // Romania
replace ISO3 = "GMB" if ISO3 == "GAM" // The Gambia
merge m:1 ISO3 YEAR_WAVE using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Referenced Files\population_WB_others.dta"
drop _merge
drop if wt == .

**************************** BEFORE QUINTILE SUM *******************************
** Sample Size
	gen n = 1
	bysort YEAR_WAVE WP5: egen samp_size = count(n)
	drop n

** Thresholds
	scalar T1 = -0.394 // for threshold set at (FEWFOODS)
	scalar T2 = 1.649 // for threshold set at  (WHLDAY)

** Beneficiary Size
	gen t1_prev = 0
	replace t1_prev = 1 if RS_global_FULL > T1
	gen t2_prev = 0
	replace t2_prev = 1 if RS_global_FULL > T2
	bysort YEAR_WAVE WP5: egen ben1_size = total(t1_prev)
	bysort YEAR_WAVE WP5: egen ben2_size = total(t2_prev)

** Indicators
	gen I_tob1 = 0 
	replace I_tob1 = 1 if RS_global_FULL > -0.408
	gen I_tob2 = 0 
	replace I_tob2 = 1 if RS_global_FULL > 1.671

** Differences
	gen diff1 = I_tob1*(T1 - RS_global_FULL)
	gen diff2 = I_tob2*(T2 - RS_global_FULL)

** Income Percents
	gen INCOME_perc = INCOME_4/100

** Summary Statistics
	tabstat t1_prev, statistics(mean sd min max) by(Incomegroup)
	tabstat t1_prev, statistics(mean sd min max) by(RegionA)
	tabstat t2_prev, statistics(mean sd min max) by(Incomegroup)
	tabstat t2_prev, statistics(mean sd min max) by(RegionA)
	drop t1_prev t2_prev

** Save a copy
	save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis7_C1.dta", replace
	clear
********************************************************************************


*************************
** Cost Beta Estimates ** by WB Income Group (bins = 3)
*************************
clear
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis7_C1.dta", replace

/* WB income groups via Rasch with tobit 
* High income
tobit RS_global_FULL log_INCOME_4 r2 r3 q2 q3 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Incomegroup == "High income" [pw = wt], ll(c_FULL) vce(robust)
	margins , dydx(log_INCOME_4 q2 q3) predict(ys(c_FULL, 999999))
	scalar Q1 = r(table)[1,1]
	scalar Q2 = r(table)[1,1] + r(table)[1,2]
	scalar Q3 = r(table)[1,1] + r(table)[1,3]
	forvalues i = 1/3 {
		replace b`i'_Tob = Q`i' if Incomegroup == "High income"
	}	
* Upper middle income
tobit RS_global_FULL log_INCOME_4 r2 r3 q2 q3 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Incomegroup == "Upper middle income" [pw = wt], ll(c_FULL) vce(robust)
	margins , dydx(log_INCOME_4 q2 q3) predict(ys(c_FULL, 999999))	
	scalar Q1 = r(table)[1,1]
	scalar Q2 = r(table)[1,1] + r(table)[1,2]
	scalar Q3 = r(table)[1,1] + r(table)[1,3]
	forvalues i = 1/3 {
		replace b`i'_Tob = Q`i' if Incomegroup == "Upper middle income"
	}
* Lower middle income
tobit RS_global_FULL log_INCOME_4 r2 r3 q2 q3 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Incomegroup == "Lower middle income" [pw = wt], ll(c_FULL) vce(robust)
	margins , dydx(log_INCOME_4 q2 q3) predict(ys(c_FULL, 999999))
		scalar Q1 = r(table)[1,1]
	scalar Q2 = r(table)[1,1] + r(table)[1,2]
	scalar Q3 = r(table)[1,1] + r(table)[1,3]
	forvalues i = 1/3 {
		replace b`i'_Tob = Q`i' if Incomegroup == "Lower middle income"
	}
* Low income
tobit RS_global_FULL log_INCOME_4 r2 r3 q2 q3 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Incomegroup == "Low income" [pw = wt], ll(c_FULL) vce(robust)
	margins , dydx(log_INCOME_4 q2 q3) predict(ys(c_FULL, 999999))
	scalar Q1 = r(table)[1,1]
	scalar Q2 = r(table)[1,1] + r(table)[1,2]
	scalar Q3 = r(table)[1,1] + r(table)[1,3]
	forvalues i = 1/3 {
		replace b`i'_Tob = Q`i' if Incomegroup == "Low income"
	}
* Insurance Policy
	save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis7_C1_SAVE.dta", replace
	clear
*/
/* Costing - Sans Income Bumps
* Preliminaries
	use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis7_C1_SAVE.dta", replace
	drop b1_LPM1-b3_LPM2
	drop if YEAR_WAVE != 2017
	
* Create distance per person moved
	gen d11 = 0
	replace d11 = diff1*(1/b1_Tob) if quint1 == 1
	gen d12 = 0
	replace d12 = diff1*(1/b2_Tob) if quint1 == 2
	gen d13 = 0	
	replace d13 = diff1*(1/b3_Tob) if quint1 == 3
	gen dist1 = d11 + d12 + d13 
	drop d11 d12 d13
	gen d21 = 0
	replace d21 = diff2*(1/b1_Tob) if quint1 == 1
	gen d22 = 0
	replace d22 = diff2*(1/b2_Tob) if quint1 == 2
	gen d23 = 0	
	replace d23 = diff2*(1/b3_Tob) if quint1 == 3
	gen dist2 = d21 + d22 + d23 
	drop d21 d22 d23
	
* Per capita costs per country total pop
	gen c1_pp = (dist1*INCOME_perc)
	sort WP5
	bysort WP5: egen c1_pc = total(c1_pp)
	replace c1_pc = c1_pc/samp_size
	gen c2_pp = (dist2*INCOME_perc)
	sort WP5
	bysort WP5: egen c2_pc = total(c2_pp)
	replace c2_pc = c2_pc/samp_size
	
* Per capita costs per country total beneficiary - Both Thresholds
	gen c1_pp_b = (dist1*INCOME_perc)   // Mod&Sev
	sort WP5
	bysort WP5: egen c1_pc_b = total(c1_pp_b)
	replace c1_pc_b = c1_pc_b/ben1_size
	gen c2_pp_b = (dist2*INCOME_perc)   // Sev
	sort WP5
	bysort WP5: egen c2_pc_b = total(c2_pp_b)
	replace c2_pc_b = c2_pc_b/ben2_size
	
* Drop duplicates of country
	sort WP5
	quietly by WP5: gen dup = cond(_N==1, 0, _n)
	drop if dup > 1
		
* Per capita costs times population (total costs for 142 countries)
	gen tc1 = total_pop*c1_pc
	gen tc2 = total_pop*c2_pc
	
* Summary statistics
	tabstat c1_pc, statistics(mean sd min max) by(Incomegroup) // Per Cap, M&S
	tabstat c2_pc, statistics(mean sd min max) by(Incomegroup) // Per Cap, S 
	tabstat c1_pc, statistics(mean sd min max) by(RegionA)
	tabstat c2_pc, statistics(mean sd min max) by(RegionA)
	tabstat c1_pc_b, statistics(mean sd min max) by(Incomegroup) // Benef., M&S
	tabstat c2_pc_b, statistics(mean sd min max) by(Incomegroup) // Benef., S
	tabstat c1_pc_b, statistics(mean sd min max) by(RegionA)
	tabstat c2_pc_b, statistics(mean sd min max) by(RegionA)
	
* Drop it if ya feel it
* keep YEAR_WAVE Region Incomegroup INCOME_WB ISO3 WP51 RegionA c1_pc c2_pc c1_pc_b c2_pc_b tc1 tc2 samp_size total_pop b1_Tob b2_Tob b3_Tob
*/
/* Costing - 5% Income Bump
	use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis7_C1_SAVE.dta", replace
	drop b1_LPM1-b3_LPM2
	drop if YEAR_WAVE != 2017
	
* Drop Previously Useful Things
	drop samp_size-diff2
	gen n = 1
	bysort YEAR_WAVE WP5: egen samp_size = count(n)
	drop n
	
* Bump Incomes up by 5 percent
	gen Five_Perc_Income = INCOME_4/20
	replace INCOME_perc = (INCOME_4+Five_Perc_Income)/100
	
* Resulting Locational Shift
	replace b1_Tob = 0 if quint1 == 2 | quint1 == 3
	replace b2_Tob = 0 if quint1 == 3 | quint1 == 1
	replace b3_Tob = 0 if quint1 == 1 | quint1 == 2
	replace RS_global_FULL = RS_global_FULL + Five_Perc_Income*(b1_Tob+b2_Tob+b3_Tob)

** Beneficiary Size
	gen t1_prev = 0
	replace t1_prev = 1 if RS_global_FULL > T1
	gen t2_prev = 0
	replace t2_prev = 1 if RS_global_FULL > T2
	bysort YEAR_WAVE WP5: egen ben1_size = total(t1_prev)
	bysort YEAR_WAVE WP5: egen ben2_size = total(t2_prev)	
	
* Replace those things
	scalar T1 = -0.394 // for threshold set at (FEWFOODS)
	scalar T2 = 1.649 // for threshold set at (WHLDAY)	

** Indicators
	gen I_tob1 = 0 
	replace I_tob1 = 1 if RS_global_FULL > T1
	gen I_tob2 = 0 
	replace I_tob2 = 1 if RS_global_FULL > T2	
	
** Differences
	gen diff1 = I_tob1*(T1 - RS_global_FULL)
	gen diff2 = I_tob2*(T2 - RS_global_FULL)
		
* Create distance per person moved
	gen d11 = 0
	replace d11 = diff1*(1/b1_Tob) if quint1 == 1
	gen d12 = 0
	replace d12 = diff1*(1/b2_Tob) if quint1 == 2
	gen d13 = 0	
	replace d13 = diff1*(1/b3_Tob) if quint1 == 3
	gen dist1 = d11 + d12 + d13 
	drop d11 d12 d13
	gen d21 = 0
	replace d21 = diff2*(1/b1_Tob) if quint1 == 1
	gen d22 = 0
	replace d22 = diff2*(1/b2_Tob) if quint1 == 2
	gen d23 = 0	
	replace d23 = diff2*(1/b3_Tob) if quint1 == 3
	gen dist2 = d21 + d22 + d23 
	drop d21 d22 d23		
	
* Per capita costs per country total pop
	gen c1_pp = (dist1*INCOME_perc)
	sort WP5
	bysort WP5: egen c1_pc = total(c1_pp)
	replace c1_pc = c1_pc/samp_size
	gen c2_pp = (dist2*INCOME_perc)
	sort WP5
	bysort WP5: egen c2_pc = total(c2_pp)
	replace c2_pc = c2_pc/samp_size	

* Per capita costs per country total beneficiary - Both Thresholds
	gen c1_pp_b = (dist1*INCOME_perc)   // Mod&Sev
	sort WP5
	bysort WP5: egen c1_pc_b = total(c1_pp_b)
	replace c1_pc_b = c1_pc_b/ben1_size
	gen c2_pp_b = (dist2*INCOME_perc)   // Sev
	sort WP5
	bysort WP5: egen c2_pc_b = total(c2_pp_b)
	replace c2_pc_b = c2_pc_b/ben2_size	
	
* Summary statistics - Prevelence
	tabstat t1_prev, statistics(mean sd min max) by(Incomegroup)
	tabstat t1_prev, statistics(mean sd min max) by(RegionA)
	tabstat t2_prev, statistics(mean sd min max) by(Incomegroup)
	tabstat t2_prev, statistics(mean sd min max) by(RegionA)
	*drop t1_prev t2_prev	

* Drop duplicates of country
	sort WP5
	quietly by WP5: gen dup = cond(_N==1, 0, _n)
	drop if dup > 1
		
* Per capita costs times population (total costs for 142 countries)
	gen tc1 = total_pop*c1_pc
	gen tc2 = total_pop*c2_pc
	

	
* Summary statistics - Costs
	tabstat c1_pc, statistics(mean sd min max) by(Incomegroup) // Per Cap, M&S
	tabstat c2_pc, statistics(mean sd min max) by(Incomegroup) // Per Cap, S 
	tabstat c1_pc, statistics(mean sd min max) by(RegionA)
	tabstat c2_pc, statistics(mean sd min max) by(RegionA)
	tabstat c1_pc_b, statistics(mean sd min max) by(Incomegroup) // Benef., M&S
	tabstat c2_pc_b, statistics(mean sd min max) by(Incomegroup) // Benef., S
	tabstat c1_pc_b, statistics(mean sd min max) by(RegionA)
	tabstat c2_pc_b, statistics(mean sd min max) by(RegionA)
*/
/* Costing - 10% Income Bump
	use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis7_C1_SAVE.dta", replace
	drop b1_LPM1-b3_LPM2
	drop if YEAR_WAVE != 2017
	
* Drop Previously Useful Things
	drop samp_size-diff2
	gen n = 1
	bysort YEAR_WAVE WP5: egen samp_size = count(n)
	drop n
	
* Bump Incomes up by 10 percent
	gen Ten_Perc_Income = INCOME_4/10
	replace INCOME_perc = (INCOME_4+Ten_Perc_Income)/100
	
* Resulting Locational Shift
	replace b1_Tob = 0 if quint1 == 2 | quint1 == 3
	replace b2_Tob = 0 if quint1 == 3 | quint1 == 1
	replace b3_Tob = 0 if quint1 == 1 | quint1 == 2
	replace RS_global_FULL = RS_global_FULL + Ten_Perc_Income*(b1_Tob+b2_Tob+b3_Tob)

** Beneficiary Size
	gen t1_prev = 0
	replace t1_prev = 1 if RS_global_FULL > T1
	gen t2_prev = 0
	replace t2_prev = 1 if RS_global_FULL > T2
	bysort YEAR_WAVE WP5: egen ben1_size = total(t1_prev)
	bysort YEAR_WAVE WP5: egen ben2_size = total(t2_prev)	
	
* Replace those things
	scalar T1 = -0.408 // for threshold set at -0.408 (FEWFOODS)
	scalar T2 = 1.671 // for threshold set at 1.671 (WHLDAY)	

** Indicators
	gen I_tob1 = 0 
	replace I_tob1 = 1 if RS_global_FULL > T1
	gen I_tob2 = 0 
	replace I_tob2 = 1 if RS_global_FULL > T2	
	
** Differences
	gen diff1 = I_tob1*(T1 - RS_global_FULL)
	gen diff2 = I_tob2*(T2 - RS_global_FULL)
		
* Create distance per person moved
	gen d11 = 0
	replace d11 = diff1*(1/b1_Tob) if quint1 == 1
	gen d12 = 0
	replace d12 = diff1*(1/b2_Tob) if quint1 == 2
	gen d13 = 0	
	replace d13 = diff1*(1/b3_Tob) if quint1 == 3
	gen dist1 = d11 + d12 + d13 
	drop d11 d12 d13
	gen d21 = 0
	replace d21 = diff2*(1/b1_Tob) if quint1 == 1
	gen d22 = 0
	replace d22 = diff2*(1/b2_Tob) if quint1 == 2
	gen d23 = 0	
	replace d23 = diff2*(1/b3_Tob) if quint1 == 3
	gen dist2 = d21 + d22 + d23 
	drop d21 d22 d23		
	
* Per capita costs per country total pop
	gen c1_pp = (dist1*INCOME_perc)
	sort WP5
	bysort WP5: egen c1_pc = total(c1_pp)
	replace c1_pc = c1_pc/samp_size
	gen c2_pp = (dist2*INCOME_perc)
	sort WP5
	bysort WP5: egen c2_pc = total(c2_pp)
	replace c2_pc = c2_pc/samp_size	

* Per capita costs per country total beneficiary - Both Thresholds
	gen c1_pp_b = (dist1*INCOME_perc)   // Mod&Sev
	sort WP5
	bysort WP5: egen c1_pc_b = total(c1_pp_b)
	replace c1_pc_b = c1_pc_b/ben1_size
	gen c2_pp_b = (dist2*INCOME_perc)   // Sev
	sort WP5
	bysort WP5: egen c2_pc_b = total(c2_pp_b)
	replace c2_pc_b = c2_pc_b/ben2_size	
	
* Summary statistics - Prevelence
	tabstat t1_prev, statistics(mean sd min max) by(Incomegroup)
	tabstat t1_prev, statistics(mean sd min max) by(RegionA)
	tabstat t2_prev, statistics(mean sd min max) by(Incomegroup)
	tabstat t2_prev, statistics(mean sd min max) by(RegionA)
	*drop t1_prev t2_prev	
	
* Drop duplicates of country
	sort WP5
	quietly by WP5: gen dup = cond(_N==1, 0, _n)
	drop if dup > 1
		
* Per capita costs times population (total costs for 142 countries)
	gen tc1 = total_pop*c1_pc
	gen tc2 = total_pop*c2_pc
	

	
* Summary statistics - Costs
	tabstat c1_pc, statistics(mean sd min max) by(Incomegroup) // Per Cap, M&S
	tabstat c2_pc, statistics(mean sd min max) by(Incomegroup) // Per Cap, S 
	tabstat c1_pc, statistics(mean sd min max) by(RegionA)
	tabstat c2_pc, statistics(mean sd min max) by(RegionA)
	tabstat c1_pc_b, statistics(mean sd min max) by(Incomegroup) // Benef., M&S
	tabstat c2_pc_b, statistics(mean sd min max) by(Incomegroup) // Benef., S
	tabstat c1_pc_b, statistics(mean sd min max) by(RegionA)
	tabstat c2_pc_b, statistics(mean sd min max) by(RegionA)
*/

	
	
	
	
	
	
	
	
	
	
	
	
	