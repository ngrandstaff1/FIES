******************************
***   ANALYSIS DO-FILE 6   ***   Note: Commented out things are largely optional
******************************
clear
** SET-UP **
* Upload the data
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis7.dta", replace

//////////////////////////////////////////////////////////////////////
// Summary Statistics on Binary Threshold v. Binary Rasch Threshold //   Y Vars
//////////////////////////////////////////////////////////////////////
* Obtain RS values (standard assumptions, no truncation)
	mdesc RS_global_FULL RS_global_drop19
	sum RS_global_FULL , detail
	sum RS_global_drop19 if YEAR_WAVE != 2019, detail
* FULL
	gen RScF_msb = 0
	replace RScF_msb = 1 if RS_global_FULL > -0.394 & RS>0
	gen RScF_sb = 0 
	replace RScF_sb = 1 if RS_global_FULL > 1.649 & RS>0
* DROP 19
	gen x = RS_global_drop19 // moderate and severe, drop 19
	replace x = . if YEAR_WAVE == 2019
	sum x if RS_global_drop19 > -0.408
	replace x = 0 if RS_global_drop19 < -0.408
	replace x = 1 if x != 0 & x != .
	gen y = RS_global_drop19 // severe, drop 19
	replace y = . if YEAR_WAVE == 2019
	sum y if RS_global_drop19 > 1.671
	replace y = 0 if RS_global_drop19 < 1.671
	replace y = 1 if y != 0 & y != .
	ren x RScD_msb
	ren y RScD_sb
* Summarize and Compare
	sum RScF_msb RScF_sb 
	sum RScD_msb RScD_sb if YEAR_WAVE != 2019
	sum RS_msb RS_sb
		label variable RS_global_FULL "Histogram of Dependent Variable, Full Sample"
		label variable RS_global_drop19 "Histogram of Dependent Variable, Full Sample"
	twoway (hist RS_global_FULL, legend(off) color(blue)) (hist RS_global_drop19, legend(off) color(red)) 
* Check
	gen RS_8F = 0
	replace RS_8F = 1 if RS > 7
	sum RS_8F
	sum RS_msb RS_sb RS_8F if RScD_msb != . & YEAR_WAVE != 2019
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


/////////////////////////////////////////////////////
// Regress MEDIAN-IMPUTED X's on these sets of Y's //   X Vars
/////////////////////////////////////////////////////
** Covariates
local covars = "age age2 gender_f educ_b i.WP1223 rural HHsize_n child2adult"
local covars_med = "age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med"
sum age age2 gender_f educ_b i.WP1223 rural HHsize_n child2adult
sum age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med
mdesc age age2 gender_f educ_b WP1223 rural HHsize_n child2adult
mdesc age_med age2_med gender_f educ_b WP1223 rural_med HHsize_n_med child2adult_med
** Income Specifications 
	cap drop log_INCOME_4
	gen log_INCOME_4 = log(INCOME_4+1)
	corr RS_global_FULL INCOME_4       // -0.081
	corr RS_global_FULL log_INCOME_4   // -0.446
	gen ihs_INCOME_4 = asinh(INCOME_4)
	corr(ihs_INCOME_4 log_INCOME_4)
	corr(ihs_INCOME_4 RS_global_FULL)

/* Binary Thresholds (LPM; FE; Individual Weights; Medians)
* Full
	quietly reg RS_msb log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt], robust
	scalar b1 = r(table)[1,1]
	scalar s1 = r(table)[2,1]
	quietly reg RS_8F log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt], robust
	scalar b2 = r(table)[1,1]
	scalar s2 = r(table)[2,1]
	quietly reg RS_msb ihs_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt], robust
	scalar b3 = r(table)[1,1]
	scalar s3 = r(table)[2,1]
	quietly reg RS_8F ihs_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt], robust
	scalar b4 = r(table)[1,1]
	scalar s4 = r(table)[2,1]
	di b1 b2 b3 b4 
	di s1 s2 s3 s4
	
* Drop 19 
	quietly reg RS_msb log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RScD_msb != . & YEAR_WAVE != 2019 [pw = wt], robust
	scalar b1 = r(table)[1,1]
	scalar s1 = r(table)[2,1]
	quietly reg RS_8F log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RScD_msb != . & YEAR_WAVE != 2019 [pw = wt], robust
	scalar b2 = r(table)[1,1]
	scalar s2 = r(table)[2,1]
	quietly reg RS_msb ihs_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RScD_msb != . & YEAR_WAVE != 2019 [pw = wt], robust
	scalar b3 = r(table)[1,1]
	scalar s3 = r(table)[2,1]
	quietly reg RS_8F ihs_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RScD_msb != . & YEAR_WAVE != 2019 [pw = wt], robust
	scalar b4 = r(table)[1,1]
	scalar s4 = r(table)[2,1]
	di b1 b2 b3 b4 
	di s1 s2 s3 s4
	*/
/* Binary thresholds: Testing HH Weights
** Binary Thresholds (LPM; FE)
* Full
	quietly reg RS_msb log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = HHWEIGHT2], robust
	scalar b1 = r(table)[1,1]
	scalar s1 = r(table)[2,1]
	quietly reg RS_8F log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = HHWEIGHT2], robust
	scalar b2 = r(table)[1,1]
	scalar s2 = r(table)[2,1]
	quietly reg RS_msb ihs_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = HHWEIGHT2], robust
	scalar b3 = r(table)[1,1]
	scalar s3 = r(table)[2,1]
	quietly reg RS_8F ihs_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = HHWEIGHT2], robust
	scalar b4 = r(table)[1,1]
	scalar s4 = r(table)[2,1]
	di b1 b2 b3 b4 
	di s1 s2 s3 s4
	
* Drop19 
	quietly reg RS_msb log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RScD_msb != . & YEAR_WAVE != 2019 [pw = HHWEIGHT2], robust
	scalar b1 = r(table)[1,1]
	scalar s1 = r(table)[2,1]
	quietly reg RS_8F log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RScD_msb != . & YEAR_WAVE != 2019 [pw = HHWEIGHT2], robust
	scalar b2 = r(table)[1,1]
	scalar s2 = r(table)[2,1]
	quietly reg RS_msb ihs_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RScD_msb != . & YEAR_WAVE != 2019 [pw = HHWEIGHT2], robust
	scalar b3 = r(table)[1,1]
	scalar s3 = r(table)[2,1]
	quietly reg RS_8F ihs_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RScD_msb != . & YEAR_WAVE != 2019 [pw = HHWEIGHT2], robust
	scalar b4 = r(table)[1,1]
	scalar s4 = r(table)[2,1]
	di b1 b2 b3 b4 
	di s1 s2 s3 s4
*/

/* Rasch Thresholds (LPM; FE; Individual Weights; Medians)
* Full
	quietly reg RScF_msb log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt], robust
	scalar b1 = r(table)[1,1]
	scalar s1 = r(table)[2,1]
	quietly reg RScF_sb log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt], robust
	scalar b2 = r(table)[1,1]
	scalar s2 = r(table)[2,1]
	quietly reg RScF_msb ihs_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt], robust
	scalar b3 = r(table)[1,1]
	scalar s3 = r(table)[2,1]
	quietly reg RScF_sb ihs_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt], robust
	scalar b4 = r(table)[1,1]
	scalar s4 = r(table)[2,1]
	di b1 b2 b3 b4 
	di s1 s2 s3 s4
	
* Drop 19 
	quietly reg RScD_msb log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RScD_msb != . & YEAR_WAVE != 2019 [pw = wt], robust
	scalar b1 = r(table)[1,1]
	scalar s1 = r(table)[2,1]
	quietly reg RScD_sb log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RScD_msb != . & YEAR_WAVE != 2019 [pw = wt], robust
	scalar b2 = r(table)[1,1]
	scalar s2 = r(table)[2,1]
	quietly reg RScD_msb ihs_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RScD_msb != . & YEAR_WAVE != 2019 [pw = wt], robust
	scalar b3 = r(table)[1,1]
	scalar s3 = r(table)[2,1]
	quietly reg RScD_sb ihs_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RScD_msb != . & YEAR_WAVE != 2019 [pw = wt], robust
	scalar b4 = r(table)[1,1]
	scalar s4 = r(table)[2,1]
	di b1 b2 b3 b4 
	di s1 s2 s3 s4
	*/
/* Rasch thresholds: Testing HH Weights 
** Binary Thresholds (LPM; FE)
* Full
	quietly reg RScF_msb log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = HHWEIGHT2], robust
	scalar b1 = r(table)[1,1]
	scalar s1 = r(table)[2,1]
	quietly reg RScD_sb log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = HHWEIGHT2], robust
	scalar b2 = r(table)[1,1]
	scalar s2 = r(table)[2,1]
	quietly reg RScF_msb ihs_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = HHWEIGHT2], robust
	scalar b3 = r(table)[1,1]
	scalar s3 = r(table)[2,1]
	quietly reg RScD_sb ihs_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = HHWEIGHT2], robust
	scalar b4 = r(table)[1,1]
	scalar s4 = r(table)[2,1]
	di b1 b2 b3 b4 
	di s1 s2 s3 s4
	
* Drop19 
	quietly reg RScD_msb  log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RScD_msb != . & YEAR_WAVE != 2019 [pw = HHWEIGHT2], robust
	scalar b1 = r(table)[1,1]
	scalar s1 = r(table)[2,1]
	quietly reg RScD_sb log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RScD_msb != . & YEAR_WAVE != 2019 [pw = HHWEIGHT2], robust
	scalar b2 = r(table)[1,1]
	scalar s2 = r(table)[2,1]
	quietly reg RScD_msb  ihs_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RScD_msb != . & YEAR_WAVE != 2019 [pw = HHWEIGHT2], robust
	scalar b3 = r(table)[1,1]
	scalar s3 = r(table)[2,1]
	quietly reg RScD_sb ihs_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RScD_msb != . & YEAR_WAVE != 2019 [pw = HHWEIGHT2], robust
	scalar b4 = r(table)[1,1]
	scalar s4 = r(table)[2,1]
	di b1 b2 b3 b4 
	di s1 s2 s3 s4
*/

** Affine Shift on Rasch Y-samples
	sum RS_global_FULL
	scalar min1 = -r(min)
	gen RS_F = RS_global_FULL + min1
	sum RS_global_drop19
	scalar min2 = -r(min)
	gen RS_D = RS_global_drop19 + min2
** New Affine censors
	gen c_FULL_affine = c_FULL + min1
	gen c_drop19_affine = c_drop19 + min2
** General Formatting
	replace c_drop19 = . if YEAR_WAVE == 2019
** Share above limit
	tab RS
	tab RS if RScD_msb != . & YEAR_WAVE != 2019
/* Rasch Score w/ Affine Comparison (Tobit; FE; Individual Weights; Medians)
* Affine Transform
quietly tobit RS_F log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt] , ll(c_FULL_affine) vce(robust)
scalar b1 = r(table)[1,1]
scalar s1 = r(table)[2,1]
quietly tobit RS_D log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RScD_msb != . & YEAR_WAVE != 2019 [pw = wt] , ll(c_drop19_affine) vce(robust)
scalar b2 = r(table)[1,1]
scalar s2 = r(table)[2,1]

* No Affine Transform (Tobit; )
quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt] , ll(c_FULL) vce(robust)
scalar b3 = r(table)[1,1]
scalar s3 = r(table)[2,1]
quietly tobit RS_global_drop19 log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RScD_msb != . & YEAR_WAVE != 2019 [pw = wt] , ll(c_drop19) vce(robust)
scalar b4 = r(table)[1,1]
scalar s4 = r(table)[2,1]

di b1 b2 b3 b4 
di s1 s2 s3 s4
*/


/////////////////////////////////////////////////////
// McDonald & Moffitt 1980 Decomposition on Censor //   
/////////////////////////////////////////////////////
** Investigating On/off censor behavior
/* M&M Decompositions
* FULL
	cap drop xb 
	cap drop error_F
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt] , ll(c_FULL) vce(robust)
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
	
* DROP 19 
	cap drop xb 
	cap drop error_D
	quietly tobit RS_global_drop19 log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if YEAR_WAVE != 2019 [pw = wt], ll(c_drop19) vce(robust)
	predict xb if YEAR_WAVE != 2019 & RS_global_drop19 != . // column of predicted tobit regression model values
	sum xb if YEAR_WAVE != 2019 & RS_global_drop19 != .
	scalar xb_bar = r(mean)  // obtain XB mean
	gen error_D = xb - RS_global_drop19
		label variable error_D "Tobit Model Error, Drop 19 Sample"
	replace error_D = . if YEAR_WAVE == 2019
	hist error_D, kdensity normal // compare densities; nonparam v. normal
	sum error_D if YEAR_WAVE != 2019
	scalar sd = r(sd) // error s.d.
	scalar ratio_D = xb_bar/sd // M&M's z value
	scalar pdf_D = normalden(ratio_D)
	scalar cdf_D = normal(ratio_D)
	scalar MM_drop19 = [1 - (xb_bar*pdf_D)/cdf_D - (pdf_D)^2/(cdf_D)^2]
	di MM_drop19
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
	
* DROP 19 - FULL
quietly tobit RS_global_drop19 log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if YEAR_WAVE != 2019 [pw = wt], ll(c_drop19) vce(robust)
	scalar b1 = r(table)[1,1]
	scalar s1 = r(table)[2,1]
quietly probit RS_b log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if YEAR_WAVE != 2019 & RS_global_drop19 != . [pw = wt], vce(robust)
	scalar b2 = r(table)[1,1]
	scalar s2 = r(table)[2,1]
quietly reg RS_global_drop19 log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if YEAR_WAVE != 2019 & RS > 0 [pw = wt], vce(robust)
	scalar b3 = r(table)[1,1]
	scalar s3 = r(table)[2,1]
di b1 b2 b3 // full / binary / off-censor --> DROP19 SAMPLE
di s1 s2 s3
*/


///////////////////////////////////////
// Context Analysis; Broadly Defined //   
///////////////////////////////////////
* Groupings
levelsof RegionA, local(Regions)
levelsof Incomegroup, local(Out_Income)
levelsof INCOME_5, local(Country_Thres)

** IN-COUNTRY Thresholds **
* EAPa
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA =="EAPa" & INCOME_5 == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5
* EuCA	
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA =="EuCA" & INCOME_5 == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5
* LAmC
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA =="LAmC" & INCOME_5 == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5		
* MENA (acronym needs fixing)
replace RegionA = "MENA" if RegionA == "MEAN"
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA =="MENA" & INCOME_5 == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5	
* NAm
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA =="NAm" & INCOME_5 == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5	
* SAs
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA =="SAs" & INCOME_5 == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5
* SSA
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA =="SSA" & INCOME_5 == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5

** CROSS-REGIONS Thresholds **
* Regional Income Thresholds *
cap drop quint
egen quint = xtile(INCOME_4), n(5) by(RegionA)
* EAPa
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA =="EAPa" & quint == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5
* EuCA	
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA =="EuCA" & quint == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5
* LAmC
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA =="LAmC" & quint == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5
* MENA
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA =="MENA" & quint == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5
* NAm
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA =="NAm" & quint == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5
* SAs
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA =="SAs" & quint == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5
* SSA
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA =="SSA" & quint == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5
		
** INCOME GROUPS - In-country Quintiles (World Bank)
* Pooled 
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if INCOME_5 == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5
* High income	
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Incomegroup =="High income" & INCOME_5 == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5
* Upper middle income
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Incomegroup =="Upper middle income" & INCOME_5 == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5
* Lower middle income
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Incomegroup =="Lower middle income" & INCOME_5 == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5
* Low income
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Incomegroup =="Low income" & INCOME_5 == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5

** INCOME GROUPS - Cross-country Quintiles (World Bank)	
* Regional Income Thresholds *
cap drop quint
egen quint = xtile(INCOME_4), n(5) by(Incomegroup)
* Pooled - *** Same as above ***
* High income	
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Incomegroup =="High income" & quint == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5
* Upper middle income
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Incomegroup =="Upper middle income" & quint == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5
* Lower middle income
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Incomegroup =="Lower middle income" & quint == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5
* Low income
forval j = 1/5 {
	quietly tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Incomegroup =="Low income" & quint == `j' [pw = wt], ll(c_FULL) vce(robust)
	scalar b`j' = r(table)[1,1] // Betas
	scalar s`j' = r(table)[2,1] 
}
	di b1 b2 b3 b4 b5
	di s1 s2 s3 s4 s5


//////////////////////////////////////
// Regressions on the Fixed Effects //   
//////////////////////////////////////
** Save a copy
save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis7_P2.dta", replace

**********************************
***** Model A - FULL VERSION *****
**********************************
clear
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis7_P2.dta", replace
* Model:
tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt], ll(c_FULL) vce(robust)
* Numbering countries (base US)
	cap drop _merge 
	cap drop cA 
	cap drop cyA 
	merge m:1 WP5 using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Referenced Files\FixWP5.dta"
	cap drop _merge
	cap drop dup
** VERSION 1: Saving Country FE
	gen cA = 0                                               // HERE
	forvalues i = 2/158 {
	scalar cA_temp = e(b)[1,`i'+22]
	replace cA = cA_temp if cnum == `i'
}
	quietly by WP5 :  gen dup = cond(_N==1,0,_n)
	drop if dup > 1
* Descriptive Stats
	tabstat cA, statistics(mean sd) by(RegionA)         
	tabstat cA, statistics(mean sd) by(Incomegroup)
	hist cA, kdensity normal		
** VERSION 2: Median Incomes and Pooled Income Beta
clear
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis7_P2.dta", replace
	tobit RS_global_FULL log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt], ll(c_FULL) vce(robust)
	merge m:1 WP5 using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Referenced Files\FixWP5.dta"
	cap drop _merge
	cap drop _dup
	gen cA = 0                                               // HERE
	forvalues i = 2/158 {
	scalar cA_temp = e(b)[1,`i'+22]
	replace cA = cA_temp if cnum == `i'
}
* Generate median income column by C-Y
	sort WP5 YEAR_WAVE
	bysort YEAR_WAVE WP5: egen INCOME_4_med = median(log_INCOME_4)
	scalar b = e(b)[1,1] 
	gen cA_2 = b*INCOME_4_med + cA                           // HERE
	sort WP5 YEAR_WAVE
	quietly by WP5 YEAR_WAVE:  gen dup = cond(_N==1,0,_n)
	drop if dup > 1
* Descriptive Stats
	tabstat cA_2, statistics(mean sd) by(RegionA)
	tabstat cA_2, statistics(mean sd) by(Incomegroup)
	sum cA_2
** VERSION 3: Allow country-specific semi-elasticities via interaction
clear
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis7_P2.dta", replace
* Run the model anew with interaction terms on income by country (pooled)
tobit RS_global_FULL c.log_INCOME_4##i.WP5 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if log_INCOME_4 != . [pw = wt], ll(c_FULL) vce(robust)
	merge m:1 WP5 using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Referenced Files\FixWP5.dta"
	cap drop _merge
	cap drop _dup
	sort WP5 YEAR_WAVE
	bysort YEAR_WAVE WP5: egen INCOME_4_med = median(log_INCOME_4)
* Saving Country FE
	scalar beta_stable = e(b)[1,1]
	gen cA_c = 0 
	forvalues i = 2/158 {
		scalar cA_temp = e(b)[1,`i'+1]
		replace cA_c = cA_temp if cnum == `i'
	}	
* Saving Income Coefficients
	gen cA_i = 0
	forvalues i = 2/158 {
		scalar cA_temp = e(b)[1,`i'+159]
		replace cA_i = cA_temp if cnum == `i'
	}
	gen cA_3 = cA_c + cA_i*INCOME_4_med + beta_stable*INCOME_4_med               // HERE
** CLEAN UP
* Drop Duplicate Values
	sort WP5 YEAR_WAVE INCOME_4_med
	quietly by WP5 YEAR_WAVE INCOME_4_med:  gen dup = cond(_N==1,0,_n)
	drop if dup > 1
	drop cA_i cA_c
* Quick summary statistics on Model A
	tabstat cA_3, statistics(mean sd) by(RegionA)
	tabstat cA_3, statistics(mean sd) by(Incomegroup)
	hist cA_3, kdensity normal
* Save to use later: 
save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Do Files\Analysis\CY_ModelA_V3.dta", replace
	
************************************
***** Model B - Drop19 VERSION *****
************************************	
clear
** VERSION 1: Only Country Fixed Effect (n = 158)
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis7_P2.dta", replace
	drop if YEAR_WAVE == 2019 
	drop if RS_global_drop19 == .
	drop if WP51 == "Mauritania"
* Numbering countries (base US)
	merge m:1 WP5 using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Referenced Files\FixWP5.dta"
	drop if _merge == 2	
	drop _merge
** Model B:
	tobit RS_global_drop19 log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt], ll(c_drop19) vce(robust)
* Extract Country FE
	cap drop cB
	cap drop bin
	sort WP5
	egen bin = group(WP5) 
	gen cB = 0
	set trace on
	forvalues i = 2/116 {
		scalar cB_temp = e(b)[1,`i'+21]
		replace cB = cB_temp if bin == `i'
	}
	set trace offtats
	tabstat cB, statistics(mean sd) by(RegionA)         
	tabstat cB, statistics(mean sd) by(Incomegroup)
	hist cB, kdensity normal	
** VERSION 2: Median Incomes and Pooled Income Beta (n = 580)
clear
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis7_P2.dta", replace
	drop if YEAR_WAVE == 2019 
	drop if RS_global_drop19 == .
	drop if WP51 == "Mauritania"
* Numbering countries (base US)
	merge m:1 WP5 using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Referenced Files\FixWP5.dta"
	drop if _merge == 2	
	drop _merge
** Model B:
	tobit RS_global_drop19 log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt], ll(c_drop19) vce(robust)
* Extract Country FE
	cap drop cB
	cap drop bin
	sort WP5
	egen bin = group(WP5) 
	gen cB = 0
	set trace on
	forvalues i = 2/116 {
		scalar cB_temp = e(b)[1,`i'+21]
		replace cB = cB_temp if bin == `i'
	}
	set trace off
* Generate median income column by C-Y
	scalar beta = e(b)[1,1] 
	sort WP5 YEAR_WAVE
	bysort YEAR_WAVE WP5: egen INCOME_4_med = median(log_INCOME_4)
	gen cB_2 = beta*INCOME_4_med + cB                         // HERE
	sort WP5 YEAR_WAVE
	quietly by WP5 YEAR_WAVE:  gen dup = cond(_N==1,0,_n)
	drop if dup > 1
* Descriptive Stats
	tabstat cB_2, statistics(mean sd) by(RegionA)
	tabstat cB_2, statistics(mean sd) by(Incomegroup)
	sum cB_2	
	hist cB_2
** VERSION 3: Allow country-specific semi-elasticities via interaction
clear
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis7_P2.dta", replace	
	drop if YEAR_WAVE == 2019 
	drop if RS_global_drop19 == .
	drop if WP51 == "Mauritania"
* Numbering countries (base US)
	merge m:1 WP5 using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Referenced Files\FixWP5.dta"
	drop if _merge == 2	
	drop _merge
	sort WP5
	egen bin = group(WP5) 
** Model B:
	tobit RS_global_drop19 c.log_INCOME_4##i.WP5 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if log_INCOME_4 != . [pw = wt], ll(c_drop19) vce(robust)
	cap drop cB
	cap drop cB_2
* Saving Country FE
	gen cB_c = 0
	set trace on
	forvalues i = 2/116 {
		scalar cB_temp = e(b)[1,`i'+1]
		replace cB_c = cB_temp if bin == `i'
	}	
	set trace off
* Saving Stable Income fixed effect
	scalar beta_stable = e(b)[1,1]
* Saving Interaction Terms	
	gen cB_i = 0
	set trace on
	forvalues i = 2/116 {
		scalar cB_temp = e(b)[1,`i'+117]
		replace cB_i = cB_temp if bin == `i'
	}	
	set trace off
* Final value(s)
	bysort YEAR_WAVE WP5: egen INCOME_4_med = median(log_INCOME_4)
	sort WP5 YEAR_WAVE INCOME_4_med
	quietly by WP5 YEAR_WAVE INCOME_4_med:  gen dup = cond(_N==1,0,_n)
	drop if dup > 1
	gen cB_3 = cB_c + cB_i*INCOME_4_med + beta_stable*INCOME_4_med      // HERE
* Quick summary statistics on Model A
	tabstat cB_3, statistics(mean sd) by(RegionA)
	tabstat cB_3, statistics(mean sd) by(Incomegroup)
	hist cB_3, kdensity normal	


********************************************************************************
**            MERGING FE-EXTRACTION FILES WITH OTHER REGRESSORS               ** 
********************************************************************************
** Data - MODEL A (Full; V3)
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Do Files\Analysis\CY_ModelA_V3.dta", replace
* Cleaning
	drop WPID_RANDOM-whlday n_adults-income FIELD_DATE-EMP_WORK_H HHsize-M30 RS-rural 
	drop freq-drop19 age_mis-RS_8F 
	drop INCOME_42-c_drop19_affine error_F xb error_D-cnum
	drop WP1219-WP9811 log_INCOME_2-INCOME_23 dup
	cap drop _merge
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
** Analysis: Regressing Stuff on Year
* Groups with robust SE format
	reg cA_3 urbanperc-log_Machinery i.YEAR_WAVE, robust // pooled
	reg cA_3 urban GDPpc tot_investment_ emprate_ Politic_Stability i.YEAR_WAVE, robust // structural
	reg cA_3 Av_ProteinSupplyMA Cereal_Dependency Cereal_Share TFP_Index-log_Machinery i.YEAR_WAVE, robust // food system
* Simple and clustered via standardized coefficients
	reg cA_3 urbanperc-log_Machinery i.YEAR_WAVE, beta
	reg cA_3 urban GDPpc tot_investment_ emprate_ Politic_Stability i.YEAR_WAVE, beta 
	reg cA_3 Av_ProteinSupplyMA Cereal_Dependency Cereal_Share TFP_Index-log_Machinery i.YEAR_WAVE, beta
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
	reg cA_3 TFP_Index-Capital_Index i.YEAR_WAVE, beta
	reg cA_3 log_Labor_Q-log_Machinery i.YEAR_WAVE, beta
** Clean up
clear











//////////////////////////////////////////////////////////////////
// Estimating Costs for Eliminating Food Insecurity Experiences //   
//////////////////////////////////////////////////////////////////
clear
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis7_P2.dta", replace
* estabilish new quintiles
cap drop quint
egen quint = xtile(INCOME_4), n(5) by(RegionA)
egen quint1 = xtile(INCOME_4), n(5) by(Incomegroup)
* Create places to store estimates by quintile
foreach j in Tob LPM1 LPM2 {	
	forvalues i = 1/5 {
		gen b`i'_`j' = 0
	}
}
* Just dropping a bunch of stuff
drop worried-whlday rawscore-prob_sev FIELD_DATE-EMP_WORK_HOURS reliabfl_* corr_comm_*
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
scalar T1 = -0.408 // for threshold set at -0.408 (FEWFOODS)
scalar T2 = 1.671 // for threshold set at 1.671 (WHLDAY)

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
********************************************************************************

*************************
** Cost Beta Estimates ** by WB Income Group (USE THIS ONE)
*************************
/* WB income groups via Rasch with tobit (18/20)
* Pooled 
tobit RS_global_FULL c.log_INCOME_4##ib5.quint1 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt], ll(c_FULL) vce(robust)
	testparm quint1#c.log_INCOME_4 log_INCOME_4
	scalar Q5 = e(b)[1,1]              // fifth quintile
	scalar Q1 = e(b)[1,1] + e(b)[1,7]  // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	di Q1 Q2 Q3 Q4 Q5 
	* No extraction step
* High income 5/5
tobit RS_global_FULL c.log_INCOME_4##ib5.quint1 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Incomegroup == "High income" [pw = wt], ll(c_FULL) vce(robust)
	testparm quint1#c.log_INCOME_4 log_INCOME_4
	scalar Q5 = e(b)[1,1]              // fifth quintile
	scalar Q1 = e(b)[1,1] + e(b)[1,7]  // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	di Q1 Q2 Q3 Q4 Q5 
	forvalues i = 1/5 {
		replace b`i'_Tob = Q`i' if Incomegroup == "High income"
	}
* Upper middle income 5/5
tobit RS_global_FULL c.log_INCOME_4##ib5.quint1 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Incomegroup == "Upper middle income" [pw = wt], ll(c_FULL) vce(robust)
	testparm quint1#c.log_INCOME_4 log_INCOME_4
	scalar Q5 = e(b)[1,1]              // fifth quintile
	scalar Q1 = e(b)[1,1] + e(b)[1,7]  // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	di Q1 Q2 Q3 Q4 Q5 	
	forvalues i = 1/5 {
		replace b`i'_Tob = Q`i' if Incomegroup == "Upper middle income"
	}
* Lower middle income 5/5
tobit RS_global_FULL c.log_INCOME_4##i.quint1 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Incomegroup == "Lower middle income" [pw = wt], ll(c_FULL) vce(robust)
	testparm quint1#c.log_INCOME_4 log_INCOME_4
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 
	forvalues i = 1/5 {
		replace b`i'_Tob = Q`i' if Incomegroup == "Lower middle income"
	}
* Low income 5/5
tobit RS_global_FULL c.log_INCOME_4##i.quint1 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Incomegroup == "Low income" [pw = wt], ll(c_FULL) vce(robust)
	testparm quint1#c.log_INCOME_4 log_INCOME_4
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 
	forvalues i = 1/5 {
		replace b`i'_Tob = Q`i' if Incomegroup == "Low income"
	}
*/
/* WB income groups via RScF_msb with LPM (19/20)
* Pooled
reg RScF_msb c.log_INCOME_4##i.quint1 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt], vce(robust)
	testparm quint1#c.log_INCOME_4 log_INCOME_4
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 
	* No extraction
* High income 5/5
reg RScF_msb c.log_INCOME_4##i.quint1 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Incomegroup == "High income" [pw = wt], vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 
	forvalues i = 1/5 {
		replace b`i'_LPM1 = Q`i' if Incomegroup == "High income"
	}
* Upper middle income 4/5 WITH BASE 5
reg RScF_msb c.log_INCOME_4##ib5.quint1 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Incomegroup == "Upper middle income" [pw = wt], vce(robust)
testparm quint1#c.log_INCOME_4 log_INCOME_4
	scalar Q5 = e(b)[1,1]              // fifth quintile
	scalar Q1 = e(b)[1,1] + e(b)[1,7]  // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	di Q1 Q2 Q3 Q4 Q5  
	forvalues i = 1/5 {
		replace b`i'_LPM1 = Q`i' if Incomegroup == "Upper middle income"
	}
* Lower middle income 5/5
reg RScF_msb c.log_INCOME_4##i.quint1 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAV i.WP5 if Incomegroup == "Lower middle income" [pw = wt], vce(robust)
testparm quint1#c.log_INCOME_4 log_INCOME_4
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 
	forvalues i = 1/5 {
		replace b`i'_LPM1 = Q`i' if Incomegroup == "Lower middle income"
	}
* Low income 5/5
reg RScF_msb c.log_INCOME_4##i.quint1 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAV i.WP5 if Incomegroup == "Low income" [pw = wt], vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 
	forvalues i = 1/5 {
		replace b`i'_LPM1 = Q`i' if Incomegroup == "Low income"
	}
*/
/* WB income groups via RScF_msb with LPM (13/20)
* Pooled
reg RScF_sb c.log_INCOME_4##i.quint1 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt], vce(robust)
	testparm quint1#c.log_INCOME_4 log_INCOME_4
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 
* High income (3/5)
reg RScF_sb c.log_INCOME_4##ib5.quint1 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Incomegroup == "High income" [pw = wt], vce(robust)
	testparm quint1#c.log_INCOME_4 log_INCOME_4
	scalar Q5 = e(b)[1,1]              // fifth quintile
	scalar Q1 = e(b)[1,1] + e(b)[1,7]  // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	di Q1 Q2 Q3 Q4 Q5 
	replace b1_LPM2 = . if Incomegroup == "High income" 
	replace b2_LPM2 = Q2 if Incomegroup == "High income"
	replace b3_LPM2 = Q3 if Incomegroup == "High income"
	replace b4_LPM2 = . if Incomegroup == "High income" 
	replace b5_LPM2 = Q5 if Incomegroup == "High income"
* Upper middle income (3/5)
reg RScF_sb c.log_INCOME_4##ib5.quint1 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Incomegroup == "Upper middle income" [pw = wt], vce(robust)
	testparm quint1#c.log_INCOME_4 log_INCOME_4
	scalar Q5 = e(b)[1,1]              // fifth quintile
	scalar Q1 = e(b)[1,1] + e(b)[1,7]  // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	di Q1 Q2 Q3 Q4 Q5 
	forvalues i = 1/5 {
		replace b`i'_LPM2 = Q`i' if Incomegroup == "Upper middle income"
	}
	replace b3_LPM2 = . if Incomegroup == "Upper middle income"
	replace b4_LPM2 = . if Incomegroup == "Upper middle income"
* Lower middle income (4/5)
reg RScF_sb c.log_INCOME_4##i.quint1 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAV i.WP5 if Incomegroup == "Lower middle income" [pw = wt], vce(robust)
	testparm quint1#c.log_INCOME_4 log_INCOME_4
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 
	forvalues i = 1/5 {
		replace b`i'_LPM2 = Q`i' if Incomegroup == "Lower middle income"
	}
	replace b4_LPM2 = . if Incomegroup == "Lower middle income"
* Low income (3/5) with base 5
reg RScF_sb c.log_INCOME_4##ib5.quint1 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAV i.WP5 if Incomegroup == "Low income" [pw = wt], vce(robust)
	testparm quint1#c.log_INCOME_4 log_INCOME_4
	scalar Q5 = e(b)[1,1]              // fifth quintile
	scalar Q1 = e(b)[1,1] + e(b)[1,7]  // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	di Q1 Q2 Q3 Q4 Q5 
	forvalues i = 1/5 {
		replace b`i'_LPM2 = Q`i' if Incomegroup == "Low income"
	}
	replace b2_LPM2 = . if Incomegroup == "Low income"
	replace b4_LPM2 = . if Incomegroup == "Low income"
*/





























************************
****** TO DO LIST ******
************************
** NEED TO OBTAIN BETAS BY COHORT/QUINTILE HERE **
* scalar b_1
** NEED TO DEFINE WHICH COHORT (YEAR) IS APPLICABLE FOR POPULATION **  
* drop (cond)


///////////////////////////////////
// Beta's NEED to be of form: b# //
///////////////////////////////////

** Loop of Costs - RASCH THRESHOLD 1 **
* High Income (W coeff, S for in-quintile cost)
forvalues i = 1/5 {
	gen c_`i' = 0 
	replace c_`i' = I_tob1*(diff1/b`i')*wt*INCOME_perc				
}
bysort WP5 YEAR_WAVE: egen cost_A gen 
di S1 S2 S3 S4 S5
* Upper Middle Income (X coeff, T for in-quintile cost)
forvalues i = 1/5 {
	[BETA_2]
					
}
bysort WP5 YEAR_WAVE: egen cost_B
di T1 T2 T3 T4 T5
* Lower Middle Income (Y coeff, U for in-quintile cost)
forvalues i = 1/5 {
	[BETA_2]
					
}
bysort WP5 YEAR_WAVE: egen cost_C
di U1 U2 U3 U4 U5
* Low Income (Z coeff, V for in-quintile cost)
forvalues i = 1/5 {
	[BETA_2]
					
}
bysort WP5 YEAR_WAVE: egen cost_D
di V1 V2 V3 V4 V5

** Loop of Costs - RASCH THRESHOLD 1 **
gen sum1 = (diff1/beta1)*INCOME_perc*wt
bysort YEAR_WAVE WP5: egen cost1_cy = total(sum1)
gen sum2 = (diff2/beta1)*INCOME_perc*wt
bysort YEAR_WAVE WP5: egen cost2_cy = total(sum2)
gen sum3 = (diff3/beta1)*INCOME_perc*wt
bysort YEAR_WAVE WP5: egen cost3_cy = total(sum3)



















*************************
** Cost Beta Estimates ** by Geographic Region
*************************
/* Geographic Regional groups via Rasch with tobit 
* Total; all countries and quintiles
quietly tobit RS_global_FULL c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt], ll(c_FULL) vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 
* Region 1; EAPa
quietly tobit RS_global_FULL c.log_INCOME_4##ib5.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA == "EAPa" [pw = wt], ll(c_FULL) vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 
* Region 2; EuCA
quietly tobit RS_global_FULL c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA == "EuCA" [pw = wt], ll(c_FULL) vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 

* Region 3; LAmC
quietly tobit RS_global_FULL c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA == "LAmC" [pw = wt], ll(c_FULL) vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 

* Region 4; MENA
quietly tobit RS_global_FULL c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA == "MENA" [pw = wt], ll(c_FULL) vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 

* Region 5; NAm
quietly tobit RS_global_FULL c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA == "NAm" [pw = wt], ll(c_FULL) vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 

* Region 6; SAs
quietly tobit RS_global_FULL c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA == "SAs" [pw = wt], ll(c_FULL) vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5

* Region 7; SSA
quietly tobit RS_global_FULL c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA == "SSA" [pw = wt], ll(c_FULL) vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5
*/
/* Geographic Regional groups via threshold MSB with LPM
* Total; all countries and quintiles
quietly reg RScF_msb c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt], vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 
	
* Region 1; EAPa
quietly reg RScF_msb c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA == "EAPa" [pw = wt], vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 
	
* Region 2; EuCA
quietly reg RScF_msb c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA == "EuCA" [pw = wt], vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 

* Region 3; LAmC
quietly reg RScF_msb c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA == "LAmC" [pw = wt], vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 

* Region 4; MENA
quietly reg RScF_msb c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA == "MENA" [pw = wt], vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 

* Region 5; NAm
quietly reg RScF_msb c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA == "NAm" [pw = wt], vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 

* Region 6; SAs
quietly reg RScF_msb c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA == "SAs" [pw = wt], vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5

* Region 7; SSA
quietly reg RScF_msb c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA == "SSA" [pw = wt], vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5
*/
/* Geographic Regional groups via threshold SB with LPM
* Total; all countries and quintiles
quietly reg RScF_sb c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt], vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 
	
* Region 1; EAPa
quietly reg RScF_sb c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA == "EAPa" [pw = wt], vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 
	
* Region 2; EuCA
quietly reg RScF_sb c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA == "EuCA" [pw = wt], vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 

* Region 3; LAmC
quietly reg RScF_sb c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA == "LAmC" [pw = wt], vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 

* Region 4; MENA
quietly reg RScF_sb c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA == "MENA" [pw = wt], vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 

* Region 5; NAm
quietly reg RScF_sb c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA == "NAm" [pw = wt], vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5 

* Region 6; SAs
quietly reg RScF_sb c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA == "SAs" [pw = wt], vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5

* Region 7; SSA
quietly reg RScF_sb c.log_INCOME_4##i.quint age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if RegionA == "SSA" [pw = wt], vce(robust)
	scalar Q1 = e(b)[1,1]              // first quintile
	scalar Q2 = e(b)[1,1] + e(b)[1,8]  // second quintile
	scalar Q3 = e(b)[1,1] + e(b)[1,9]  // third quintile
	scalar Q4 = e(b)[1,1] + e(b)[1,10] // fourth quintile
	scalar Q5 = e(b)[1,1] + e(b)[1,11] // fifth quintile
	di Q1 Q2 Q3 Q4 Q5
*/
** Note: When including country-quintile interaction v. little is significant
** Note: Base is quintile 1, so associated SE is from coefficient on log_INCOME
* To change base categorical variable use ib#.categorical in regression

*************************************
** Seemingly Unrelated Regressions ** does not work, by construction
*************************************
* Binary LPM Threshold 1
/* Quintile 1
sureg (RScF_msb log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Reg1 == 1 & quint1 == 1) ///
(RScF_msb log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Reg2 == 1 & quint1 == 1) ///
(RScF_msb c.log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Reg3 == 1 & quint1 == 1) ///
(RScF_msb c.log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Reg4 == 1 & quint1 == 1) ///
(RScF_msb c.log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Reg5 == 1 & quint1 == 1) ///
(RScF_msb c.log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Reg6 == 1 & quint1 == 1) ///
(RScF_msb c.log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Reg7 == 1 & quint1 == 1)
*/
/* Quintile 2
sureg (RScF_msb log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Reg1 == 1 & quint1 == 2) ///
(RScF_msb log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Reg2 == 1 & quint1 == 2) ///
(RScF_msb c.log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Reg3 == 1 & quint1 == 2) ///
(RScF_msb c.log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Reg4 == 1 & quint1 == 2) ///
(RScF_msb c.log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Reg5 == 1 & quint1 == 2) ///
(RScF_msb c.log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Reg6 == 1 & quint1 == 2) ///
(RScF_msb c.log_INCOME_4 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Reg7 == 1 & quint1 == 2)
*/
/* Interactions
sureg (RScF_msb c.log_INCOME_4##i.quint1 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Reg1 == 1)(RScF_msb c.log_INCOME_4##i.quint1  age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Reg2 == 1)(RScF_msb c.log_INCOME_4##i.quint1  age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Reg3 == 1)(RScF_msb c.log_INCOME_4##i.quint1  age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Reg4 == 1)(RScF_msb c.log_INCOME_4##i.quint1 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Reg5 == 1)(RScF_msb c.log_INCOME_4##i.quint1 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Reg6 == 1)(RScF_msb c.log_INCOME_4##i.quint1 age_med age2_med gender_f educ_b i.WP1223 rural_med HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 if Reg7 == 1)
*/


































