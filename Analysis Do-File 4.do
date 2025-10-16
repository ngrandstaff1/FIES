******************************
***   ANALYSIS DO-FILE 4   ***     REGRESSIONS & OUTPUT
******************************
clear
** SET-UP
* Upload the data
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis7.dta"

* Remove any observation that isn't relevant
drop n_adults-prob_sev area-countrynew EMP_2010-HHsize INDEX_CA M1 M30 WP1230 WP1230Recoded RS_par PROB_mod_sev PROB_sev log_INCOME_2 

* Establish a globals and locals
global subsets = "FULL cover6 drop14 drop19"
global regionA = "EAPa EuCA LAmC MEAN NAm SAs SSA"
local covars "age age2 gender_f educ_b rural i.WP1223 HHsize_n child2adult"


*****************
** REGRESSIONS **
*****************

** TABLE 1 *******************************************************
*           Binary (_b), FULL, BothCovar, IncSpecs(Lin, Quad, Log)
*           Missing values allowed, FE(CY), non-saturated, robust
* For LaTeX
/*
quietly reg RS_b INCOME_4 i.YEAR_WAVE i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG1.txt, replace tex ctitle(Any) label addtext(Covariates, NO, Country FE, YES, Year FE, YES) keep(INCOME_4)
quietly reg RS_b INCOME_4 INCOME_42 INCOME_43 i.YEAR_WAVE i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG1.txt, append tex ctitle(Any) label addtext(Covariates, NO, Country FE, YES, Year FE, YES) keep(INCOME_4 INCOME_42 INCOME_43)
quietly reg RS_b log_INCOME_4 i.YEAR_WAVE i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG1.txt, append tex ctitle(Any) label addtext(Covariates, NO, Country FE, YES, Year FE, YES) keep(log_INCOME_4)
quietly reg RS_b INCOME_4 age age2 gender_f educ_b rural i.WP1223 HHsize_n child2adult i.YEAR_WAVE i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG1.txt, append tex ctitle(Any) label addtext(Covariates, YES, Country FE, YES, Year FE, YES) keep(INCOME_4)
quietly reg RS_b INCOME_4 INCOME_42 INCOME_43 age age2 gender_f educ_b rural i.WP1223 HHsize_n child2adult i.YEAR_WAVE i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG1.txt, append tex ctitle(Any) label addtext(Covariates, YES, Country FE, YES, Year FE, YES) keep(INCOME_4 INCOME_42 INCOME_43)
quietly reg RS_b log_INCOME_4 age age2 gender_f educ_b rural i.WP1223 HHsize_n child2adult i.YEAR_WAVE i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG1.txt, append tex ctitle(Any) label addtext(Covariates, YES, Country FE, YES, Year FE, YES) keep(log_INCOME_4)
*/

* TABLE 2 *******************************************************
*           Binary (_msb), FULL, BothCovar, IncSpecs(Lin, Quad, Log)
*           Missing values allowed, FE(CY), non-saturated, robust
* For LaTeX
/*
quietly reg RS_msb INCOME_4 i.YEAR_WAVE i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG2.txt, replace tex ctitle(Any) label addtext(Covariates, NO, Country FE, YES, Year FE, YES) keep(INCOME_4)
quietly reg RS_msb INCOME_4 INCOME_42 INCOME_43 i.YEAR_WAVE i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG2.txt, append tex ctitle(Any) label addtext(Covariates, NO, Country FE, YES, Year FE, YES) keep(INCOME_4 INCOME_42 INCOME_43)
quietly reg RS_msb log_INCOME_4 i.YEAR_WAVE i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG2.txt, append tex ctitle(Any) label addtext(Covariates, NO, Country FE, YES, Year FE, YES) keep(log_INCOME_4)
quietly reg RS_msb INCOME_4 age age2 gender_f educ_b rural i.WP1223 HHsize_n child2adult i.YEAR_WAVE i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG2.txt, append tex ctitle(Any) label addtext(Covariates, YES, Country FE, YES, Year FE, YES) keep(INCOME_4)
quietly reg RS_msb INCOME_4 INCOME_42 INCOME_43 age age2 gender_f educ_b rural i.WP1223 HHsize_n child2adult i.YEAR_WAVE i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG2.txt, append tex ctitle(Any) label addtext(Covariates, YES, Country FE, YES, Year FE, YES) keep(INCOME_4 INCOME_42 INCOME_43)
quietly reg RS_msb log_INCOME_4 age age2 gender_f educ_b rural i.WP1223 HHsize_n child2adult i.YEAR_WAVE i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG2.txt, append tex ctitle(Mod\&Sev) label addtext(Covariates, YES, Country FE, YES, Year FE, YES) keep(log_INCOME_4)
*/

* TABLE 3 **********************************************************************
*           Binary (_sb), FULL, BothCovar, IncSpecs(Lin, Quad, Log)
*           Missing values allowed, FE(CY), non-saturated, robust
* For LaTeX
/*
quietly reg RS_sb INCOME_4 i.YEAR_WAVE i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG3.txt, replace tex ctitle(Any) label addtext(Covariates, NO, Country FE, YES, Year FE, YES) keep(INCOME_4)
quietly reg RS_sb INCOME_4 INCOME_42 INCOME_43 i.YEAR_WAVE i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG3.txt, append tex ctitle(Any) label addtext(Covariates, NO, Country FE, YES, Year FE, YES) keep(INCOME_4 INCOME_42 INCOME_43)
quietly reg RS_sb log_INCOME_4 i.YEAR_WAVE i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG3.txt, append tex ctitle(Any) label addtext(Covariates, NO, Country FE, YES, Year FE, YES) keep(log_INCOME_4)
quietly reg RS_sb INCOME_4 age age2 gender_f educ_b rural i.WP1223 HHsize_n child2adult i.YEAR_WAVE i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG3.txt,append tex ctitle(Any) label addtext(Covariates, YES, Country FE, YES, Year FE, YES) keep(INCOME_4)
quietly reg RS_sb INCOME_4 INCOME_42 INCOME_43 age age2 gender_f educ_b rural i.WP1223 HHsize_n child2adult i.YEAR_WAVE i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG3.txt, append tex ctitle(Any) label addtext(Covariates, YES, Country FE, YES, Year FE, YES) keep(INCOME_4 INCOME_42 INCOME_43)
quietly reg RS_sb log_INCOME_4 age age2 gender_f educ_b rural i.WP1223 HHsize_n child2adult i.YEAR_WAVE i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG3.txt, append tex ctitle(Severe) label addtext(Covariates, YES, Country FE, YES, Year FE, YES) keep(log_INCOME_4)
*/

* TABLE 4 **********************************************************************
*           Binaries, FULL, Covars, IncSpecs(Log,IHST)
*           Missing values allowed, FE(CY), non-saturated, robust
* Preliminaries - Means & IHSTs
egen INCOME_mean = mean(INCOME_4)
gen INCOME_IHS = asinh(INCOME_4)
egen RS_b_mean = mean(RS_b)
egen RS_msb_mean = mean(RS_msb)
egen RS_sb_mean = mean(RS_sb)
* Just in case
cap gen RS_global_drop19_alt = RS_global_drop19
cap replace RS_global_drop19_alt = . if YEAR_WAVE == 2019
cap replace RS_global_drop19_alt = c_drop19 if YEAR_WAVE != 2019 & RS == 0
* For LaTeX
/* Need corrected beta's on IHST transformed variables entered manually
quietly reg RS_b log_INCOME_4 age age2 gender_f educ_b rural i.WP1223 HHsize_n child2adult i.YEAR_WAVE i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG4.txt, replace tex ctitle(Any) label addtext(Covariates, YES, Country FE, YES, Year FE, YES) keep(log_INCOME_4)
quietly reg RS_msb log_INCOME_4 age age2 gender_f educ_b rural i.WP1223 HHsize_n child2adult i.YEAR_WAVE i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG4.txt, append tex ctitle(Mod\&Sev) label addtext(Covariates, YES, Country FE, YES, Year FE, YES) keep(log_INCOME_4)
quietly reg RS_sb log_INCOME_4 age age2 gender_f educ_b rural i.WP1223 HHsize_n child2adult i.YEAR_WAVE i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG4.txt, append tex ctitle(Severe) label addtext(Covariates, YES, Country FE, YES, Year FE, YES) keep(log_INCOME_4)
quietly reg RS_b INCOME_IHS age age2 gender_f educ_b rural i.WP1223 HHsize_n child2adult i.YEAR_WAVE i.WP5 [pw = wt], robust
nlcom (_b[INCOME_IHS]*INCOME_mean)/(RS_b_mean*sqrt(INCOME_mean^2+1))
scalar b1 = r(table)[1,1]
scalar s1 = r(table)[1,2]
outreg2 using U:\FIES\Final_Tables\REG4.txt, append tex ctitle(Any) label addtext(Covariates, YES, Country FE, YES, Year FE, YES) keep(INCOME_IHS)
quietly reg RS_msb INCOME_IHS age age2 gender_f educ_b rural i.WP1223 HHsize_n child2adult i.YEAR_WAVE i.WP5 [pw = wt], robust
nlcom (_b[INCOME_IHS]*INCOME_mean)/(RS_msb_mean*sqrt(INCOME_mean^2+1))
scalar b2 = r(table)[1,1]
scalar s2 = r(table)[1,2]
outreg2 using U:\FIES\Final_Tables\REG4.txt, append tex ctitle(Mod\&Sev) label addtext(Covariates, YES, Country FE, YES, Year FE, YES) keep(INCOME_IHS)
quietly reg RS_sb INCOME_IHS age age2 gender_f educ_b rural i.WP1223 HHsize_n child2adult i.YEAR_WAVE i.WP5 [pw = wt], robust
nlcom (_b[INCOME_IHS]*INCOME_mean)/(RS_sb_mean*sqrt(INCOME_mean^2+1))
scalar b3 = r(table)[1,1]
scalar s3 = r(table)[1,2]
outreg2 using U:\FIES\Final_Tables\REG4.txt, append tex ctitle(Severe) label addtext(Covariates, YES, Country FE, YES, Year FE, YES) keep(INCOME_IHS)
display b1 b2 b3
display s1 s2 s3

*/

* TABLE 5 **********************************************************************
*           Binaries, FULL, Covars, IncSpecs(Log,IHST)
*           Missing values allowed, FE(CY), saturated, robust
* For LaTeX
/* Need corrected beta's on IHST transformed variables entered manually
quietly reg RS_b log_INCOME_4 age age2 gender_f educ_b rural i.WP1223 HHsize_n child2adult i.YEAR_WAVE i.WP5 i.YEAR_WAVE#i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG5.txt, replace tex ctitle(Any) label addtext(Saturated, YES, Covariates, YES, Country FE, YES, Year FE, YES) keep(log_INCOME_4)
quietly reg RS_msb log_INCOME_4 age age2 gender_f educ_b rural i.WP1223 HHsize_n child2adult i.YEAR_WAVE i.WP5 i.YEAR_WAVE#i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG5.txt, append tex ctitle(Any) label addtext(Saturated, YES, Covariates, YES, Country FE, YES, Year FE, YES) keep(log_INCOME_4)
quietly reg RS_sb log_INCOME_4 age age2 gender_f educ_b rural i.WP1223 HHsize_n child2adult i.YEAR_WAVE i.WP5 i.YEAR_WAVE#i.WP5 [pw = wt], robust
outreg2 using U:\FIES\Final_Tables\REG5.txt, append tex ctitle(Any) label addtext(Saturated, YES, Covariates, YES, Country FE, YES, Year FE, YES) keep(log_INCOME_4)
quietly reg RS_b INCOME_IHS age age2 gender_f educ_b rural i.WP1223 HHsize_n child2adult i.YEAR_WAVE i.WP5 i.YEAR_WAVE#i.WP5 [pw = wt], robust
nlcom (_b[INCOME_IHS]*INCOME_mean)/(RS_b_mean*sqrt(INCOME_mean^2+1))
scalar b1 = r(table)[1,1]
scalar s1 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG5.txt, append tex ctitle(Any) label addtext(Saturated, YES, Covariates, YES, Country FE, YES, Year FE, YES) keep(INCOME_IHS)
quietly reg RS_msb INCOME_IHS age age2 gender_f educ_b rural i.WP1223 HHsize_n child2adult i.YEAR_WAVE i.WP5 i.YEAR_WAVE#i.WP5 [pw = wt], robust
nlcom (_b[INCOME_IHS]*INCOME_mean)/(RS_msb_mean*sqrt(INCOME_mean^2+1))
scalar b2 = r(table)[1,1]
scalar s2 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG5.txt, append tex ctitle(Any) label addtext(Saturated, YES, Covariates, YES, Country FE, YES, Year FE, YES) keep(INCOME_IHS)
quietly reg RS_sb INCOME_IHS age age2 gender_f educ_b rural i.WP1223 HHsize_n child2adult i.YEAR_WAVE i.WP5 i.YEAR_WAVE#i.WP5 [pw = wt], robust
nlcom (_b[INCOME_IHS]*INCOME_mean)/(RS_sb_mean*sqrt(INCOME_mean^2+1))
scalar b3 = r(table)[1,1]
scalar s3 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG5.txt, append tex ctitle(Any) label addtext(Saturated, YES, Covariates, YES, Country FE, YES, Year FE, YES) keep(INCOME_IHS)
display b1 b2 b3
display s1 s2 s3
*/

* TABLE 6.A **********************************************************************
*           Binaries, FULL, Covars, IncSpecs(Log,IHST)
*           Missing(Mis,Med), FE(CY), not-saturated
* For LaTeX
/* Need corrected beta's on IHST transformed variables entered manually
quietly reg RS_b INCOME_IHS age_mis age2_mis gender_f_mis educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 [pw = wt], robust
nlcom (_b[INCOME_IHS]*INCOME_mean)/(RS_b_mean*sqrt(INCOME_mean^2+1))
scalar b1 = r(table)[1,1]
scalar s1 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG6.txt, replace tex ctitle(Any) label addtext(Impute, Zero, Covariates, YES, Country FE, YES, Year FE, YES) keep(INCOME_IHS)

quietly reg RS_msb INCOME_IHS age_mis age2_mis gender_f_mis educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 [pw = wt], robust
nlcom (_b[INCOME_IHS]*INCOME_mean)/(RS_msb_mean*sqrt(INCOME_mean^2+1))
scalar b2 = r(table)[1,1]
scalar s2 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG6.txt, append tex ctitle(Any) label addtext(Impute, Zero, Covariates, YES, Country FE, YES, Year FE, YES) keep(INCOME_IHS)

quietly reg RS_sb INCOME_IHS age_mis age2_mis gender_f_mis educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 [pw = wt], robust
nlcom (_b[INCOME_IHS]*INCOME_mean)/(RS_sb_mean*sqrt(INCOME_mean^2+1))
scalar b3 = r(table)[1,1]
scalar s3 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG6.txt, append tex ctitle(Any) label addtext(Impute, Zero, Covariates, YES, Country FE, YES, Year FE, YES) keep(INCOME_IHS)

quietly reg RS_b INCOME_IHS age_med age2_med gender_f_mis educ_b_mis rural_med i.WP1223 HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt], robust
nlcom (_b[INCOME_IHS]*INCOME_mean)/(RS_b_mean*sqrt(INCOME_mean^2+1))
scalar b4 = r(table)[1,1]
scalar s4 = r(table)[2,1]

outreg2 using U:\FIES\Final_Tables\REG6.txt, append tex ctitle(Any) label addtext(Impute, Median, Covariates, YES, Country FE, YES, Year FE, YES) keep(INCOME_IHS)

quietly reg RS_msb INCOME_IHS age_med age2_med gender_f_mis educ_b_mis rural_med i.WP1223 HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt], robust
nlcom (_b[INCOME_IHS]*INCOME_mean)/(RS_msb_mean*sqrt(INCOME_mean^2+1))
scalar b5 = r(table)[1,1]
scalar s5 = r(table)[2,1]

outreg2 using U:\FIES\Final_Tables\REG6.txt, append tex ctitle(Any) label addtext(Impute, Median, Covariates, YES, Country FE, YES, Year FE, YES) keep(INCOME_IHS)

quietly reg RS_sb INCOME_IHS age_med age2_med gender_f_mis educ_b_mis rural_med i.WP1223 HHsize_n_med child2adult_med i.YEAR_WAVE i.WP5 [pw = wt], robust
nlcom (_b[INCOME_IHS]*INCOME_mean)/(RS_sb_mean*sqrt(INCOME_mean^2+1))
scalar b6 = r(table)[1,1]
scalar s6 = r(table)[2,1]

outreg2 using U:\FIES\Final_Tables\REG6.txt, append tex ctitle(Any) label addtext(Impute, Median, Covariates, YES, Country FE, YES, Year FE, YES) keep(INCOME_IHS)
display b1 b2 b3 b4 b5 b6
display s1 s2 s3 s4 s5 s6
*/

* TABLE 6.B **********************************************************************
*           Binaries, OLS, Covars, IncSpecs(Log,IHST)
*           Missing(Mis), FE(CY), not-saturated, two samples (D19, Full)
* For LaTeX
cap ren gender_f gender_f_mis
/* Need corrected beta's on IHST transformed variables entered manually
quietly reg RS_b INCOME_IHS age_mis age2_mis gender_f_mis educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 [pw = wt], robust
nlcom (_b[INCOME_IHS]*INCOME_mean)/(RS_b_mean*sqrt(INCOME_mean^2+1))
scalar b1 = r(table)[1,1]
scalar s1 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG6.txt, replace tex ctitle(Any) label addtext(Country FE, YES, Year FE, YES, Sample, Full) keep(INCOME_IHS)
quietly reg RS_msb INCOME_IHS age_mis age2_mis gender_f_mis educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 [pw = wt], robust
nlcom (_b[INCOME_IHS]*INCOME_mean)/(RS_msb_mean*sqrt(INCOME_mean^2+1))
scalar b2 = r(table)[1,1]
scalar s2 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG6.txt, append tex ctitle(Any) label addtext(Country FE, YES, Year FE, YES, Sample, Full) keep(INCOME_IHS)
quietly reg RS_sb INCOME_IHS age_mis age2_mis gender_f_mis educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 [pw = wt], robust
nlcom (_b[INCOME_IHS]*INCOME_mean)/(RS_sb_mean*sqrt(INCOME_mean^2+1))
scalar b3 = r(table)[1,1]
scalar s3 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG6.txt, append tex ctitle(Any) label addtext(Country FE, YES, Year FE, YES, Sample, Full) keep(INCOME_IHS)

quietly reg RS_b INCOME_IHS age_mis age2_mis gender_f_mis educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if RS_global_drop19_alt != . & YEAR_WAVE != 2019 [pw = wt], robust
nlcom (_b[INCOME_IHS]*INCOME_mean)/(RS_b_mean*sqrt(INCOME_mean^2+1))
scalar b4 = r(table)[1,1]
scalar s4 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG6.txt, append tex ctitle(Any) label addtext(Country FE, YES, Year FE, YES, Sample, Drop 19) keep(INCOME_IHS)
quietly reg RS_msb INCOME_IHS age_mis age2_mis gender_f_mis educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if RS_global_drop19_alt != . & YEAR_WAVE != 2019  [pw = wt], robust
nlcom (_b[INCOME_IHS]*INCOME_mean)/(RS_msb_mean*sqrt(INCOME_mean^2+1))
scalar b5 = r(table)[1,1]
scalar s5 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG6.txt, append tex ctitle(Any) label addtext(Country FE, YES, Year FE, YES, Sample, Drop 19) keep(INCOME_IHS)
quietly reg RS_sb INCOME_IHS age_mis age2_mis gender_f_mis educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if RS_global_drop19_alt != . & YEAR_WAVE != 2019  [pw = wt], robust
nlcom (_b[INCOME_IHS]*INCOME_mean)/(RS_sb_mean*sqrt(INCOME_mean^2+1))
scalar b6 = r(table)[1,1]
scalar s6 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG6.txt, append tex ctitle(Any) label addtext(Country FE, YES, Year FE, YES, Sample, Drop 19) keep(INCOME_IHS)
display b1 b2 b3 b4 b5 b6
display s1 s2 s3 s4 s5 s6
*/


* TABLE 7 **********************************************************
*           Rasch continuous, FULL/drop#/cover6, Covars, IncSpecs(IHST)
*           Missing(Mis), FE(CY), not-saturated, OLS, local (no RS<1)
* Rasch score by subset ****************
gen v_c6 = RS_global_cover6
gen v_d14 = RS_global_drop14
gen v_d19 = RS_global_drop19
* Conditional binary on RS and Rasch score
gen b_c6 = 0
replace b_c6 = 1 if RS>0 & v_c6 != .
gen b_d14 = 0
replace b_d14 = 1 if RS>0 & v_d14 != .
gen b_d19 = 0
replace b_d19 = 1 if RS>0 & v_d19 != .
** Y-Variables *************************
* Full 
egen mean_full_f = mean(RS_global_FULL)
egen mean_c6_f = mean(v_c6) 
egen mean_d14_f = mean(v_d14) 
egen mean_d19_f = mean(v_d19) 
gen ihs_c6_f = asinh(v_c6)
gen ihs_d14_f = asinh(v_d14)
gen ihs_d19_f = asinh(v_d19)
* Local (all, including full)
egen mean_full_l = mean(RS_global_FULL) if RS>0
egen mean_c6_l = mean(v_c6) if b_c6 ==1
egen mean_d14_l = mean(v_d14) if b_d14 ==1
egen mean_d19_l = mean(v_d19) if b_d19 ==1
gen ihs_full_l = asinh(RS_global_FULL) if RS>0
gen ihs_c6_l = asinh(v_c6) if b_c6==1
gen ihs_d14_l = asinh(v_d14) if b_d14==1
gen ihs_d19_l = asinh(v_d19) if b_d19==1
** X-Variables *************************
** Income by RS and Rasch score
* Full (all)
egen meanI_full_f = mean(INCOME_4)
egen meanI_c6_f = mean(INCOME_4) if c_cover6 != .
egen meanI_d14_f = mean(INCOME_4) if c_drop14 != .
egen meanI_d19_f = mean(INCOME_4) if c_drop19 != . 
gen ihsI_full_f = asinh(INCOME_4)
gen ihsI_c6_f = asinh(INCOME_4) if c_cover6 !=.
gen ihsI_d14_f = asinh(INCOME_4) if c_drop14 !=.
gen ihsI_d19_f = asinh(INCOME_4) if c_drop19 !=.
* Local (all, including full)
egen meanI_full_l = mean(INCOME_4) if RS>1
egen meanI_c6_l = mean(INCOME_4) if c_cover6 != . & RS>1
egen meanI_d14_l = mean(INCOME_4) if c_drop14 != . & RS>1
egen meanI_d19_l = mean(INCOME_4) if c_drop19 != . & RS>1
gen ihsI_full_l = asinh(INCOME_4) if RS>1
gen ihsI_c6_l = asinh(INCOME_4) if c_cover6 !=. & RS>1
gen ihsI_d14_l = asinh(INCOME_4) if c_drop14 !=. & RS>1
gen ihsI_d19_l = asinh(INCOME_4) if c_drop19 !=. & RS>1
* Scalars
sum meanI_full_f
scalar m1 = r(mean)
sum meanI_c6_f 
scalar m2 = r(mean)
sum meanI_d14_f 
scalar m3 = r(mean)
sum meanI_d19_f
scalar m4 =r(mean)
* For LaTeX
/*
quietly reg RS_global_FULL INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 [pw = wt], robust
nlcom (_b[INCOME_IHS]*m1)/(mean_full_f*sqrt(m1^2+1))
scalar b1 = r(table)[1,1]
scalar s1 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG7.txt, replace tex ctitle(Full) label addtext(Impute, Zero, Covariates, YES, Country FE, YES, Year FE, YES) keep(INCOME_IHS)

quietly reg RS_global_cover6 INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 [pw = wt], robust
nlcom (_b[INCOME_IHS]*m2)/(mean_c6_f*sqrt(m2^2+1))
scalar b2 = r(table)[1,1]
scalar s2 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG7.txt, append tex ctitle(Cover 6) label addtext(Impute, Zero, Covariates, YES, Country FE, YES, Year FE, YES) keep(INCOME_IHS)

quietly reg RS_global_drop14 INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 [pw = wt], robust
nlcom (_b[INCOME_IHS]*m3)/(mean_d14_f*sqrt(m3^2+1))
scalar b3 = r(table)[1,1]
scalar s3 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG7.txt, append tex ctitle(Drop 14) label addtext(Impute, Zero, Covariates, YES, Country FE, YES, Year FE, YES) keep(INCOME_IHS)

quietly reg RS_global_drop19 INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 [pw = wt], robust
nlcom (_b[INCOME_IHS]*m4)/(mean_d19_f*sqrt(m4^2+1))
scalar b4 = r(table)[1,1]
scalar s4 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG7.txt, append tex ctitle(Drop19) label addtext(Impute, Zero, Covariates, YES, Country FE, YES, Year FE, YES) keep(INCOME_IHS)

display b1 b2 b3 b4
display s1 s2 s3 s4
*/

* TABLE 8 *******************************************************
*           Rasch continuous, FULL/drop19, Covars, IncSpecs(IHST)
*           Missing(Mis), FE(CY), not-saturated, Tobit
* Tobit Y-Variables
gen RS_global_FULL_alt = RS_global_FULL
replace RS_global_FULL_alt = c_FULL if RS==0
gen RS_global_drop19_alt = RS_global_drop19
replace RS_global_drop19_alt = c_drop19 if RS==0
* Scalars - p1
sum meanI_full_l
scalar m1 = r(mean)
sum meanI_d19_l
scalar m2 =r(mean)
sum mean_full_l
scalar m_1_y = r(mean)
sum mean_d19_l
scalar m_2_y = r(mean)

* Scalars - p2
sum INCOME_4
scalar m3 = r(mean)
sum meanI_d19_f 
scalar m4 = r(mean)
sum RS_global_FULL_alt
scalar m3_y = r(mean)
sum RS_global_drop19_alt
scalar m4_y = r(mean)

** Checks
di m3 // mean X of Income --- (+)
di m4
di m3_y // mean RS_FULL --- (-)
di m4_y
reg y ihs_x
nlcom (_b[ihs_x]*xbar)/(ybar*sqrt(xbar^2 + 1))


* For LaTeX
/*
cap gen log1_INCOME = log(INCOME_4 + 1)

quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 [pw = wt], ll(c_FULL) vce(robust)
nlcom (_b[INCOME_IHS]*m3)/(m3_y*sqrt(m3^2+1))
scalar b1 = r(table)[1,1]
scalar s1 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG8.txt, replace tex ctitle(Full) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_IHS)

quietly tobit RS_global_drop19_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if RS_global_drop19 != . & YEAR_WAVE != 2019 [pw = wt], ll(c_drop19) vce(robust)
nlcom (_b[INCOME_IHS]*m4)/(m4_y*sqrt(m4^2+1))
scalar b2 = r(table)[1,1]
scalar s2 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG8.txt, append tex ctitle(Drop 19) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_IHS)

quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 [pw = wt], ll(c_FULL) vce(robust)
nlcom (_b[INCOME_IHS]*m3)/(abs(m3_y)*sqrt(m3^2+1))
scalar b3 = r(table)[1,1]
scalar s3 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG8.txt, append tex ctitle(Full) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_IHS)

quietly tobit RS_global_drop19_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if RS_global_drop19 != . & YEAR_WAVE != 2019 [pw = wt], ll(c_drop19) vce(robust)
nlcom (_b[INCOME_IHS]*m4)/(abs(m4_y)*sqrt(m4^2+1))
scalar b4 = r(table)[1,1]
scalar s4 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG8.txt, append tex ctitle(Drop 19) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_IHS)

quietly tobit RS_global_FULL_alt log1_INCOME age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 [pw = wt], ll(c_FULL) vce(robust)
scalar b5 = r(table)[1,1]
scalar s5 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG8.txt, append tex ctitle(Full) label addtext(Country FE, YES, Year FE, YES) keep(log1_INCOME)

quietly tobit RS_global_drop19_alt log1_INCOME age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if RS_global_drop19 != . & YEAR_WAVE != 2019 [pw = wt], ll(c_drop19) vce(robust)
scalar b6 = r(table)[1,1]
scalar s6 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG8.txt, append tex ctitle(Drop 19) label addtext(Country FE, YES, Year FE, YES) keep(log1_INCOME)

display b1 b2 b3 b4 b5 b6
display s1 s2 s3 s4 s5 s6
*/
save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis9.dta", replace
clear









