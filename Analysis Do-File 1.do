********************************
***      ANALYSIS & CODE     ***
********************************
** DATA
clear
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis3.dta", replace
ssc install outreg2, replace

/* OUTREG2 ATTEMPT -- Contact Jacob to get this fixed!!!
outreg2 using \\rschfs1x\userrs\K-Q\nwg6_RS\Downloads\test.tex, replace ctitle(Raw Score) tex label addtext(Country FE, YES, Year FE, YES)
*/

*************************************
*** GENERATING RELEVANT VARIABLES ***
*************************************
gen log_INCOME_4 = log(INCOME_4)
label variable log_INCOME_4 "Log Per Capita Annual Income in International Dollars"
ren income_2 INCOME_2
gen age2 = age^2
gen educ =0
replace educ =0 if education !="Completed elementary education or less (up to 8 years of basic education)" | education != "Secondary - 3 year TertiarySecondary education and some education beyond secondary education (9-15 years of educatio" | education != "Completed four years of education beyond high school and/or received a 4-year college degree."
replace educ =1 if education =="Completed elementary education or less (up to 8 years of basic education)"
replace educ =2 if education == "Secondary - 3 year TertiarySecondary education and some education beyond secondary education (9-15 years of educatio"
replace educ =3 if education == "Completed four years of education beyond high school and/or received a 4-year college degree."
gen educ_b = 0
replace educ_b =1 if educ >1
drop if WPID_RANDOM ==.
save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis4.dta", replace



** age age2 sex educ_b i.WP1223 rural HHsize_n child2adult

*********************************
*** PARSIMONIOUS (BASE MODEL) ***
*********************************
// REGRESSION TABLE 1 - Linear, HH income
reg RS_b INCOME_2 i.YEAR_WAVE i.WP5 [pw = wt]
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG1.txt, replace tex ctitle(Any) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_2)
reg RS_msb INCOME_2 i.YEAR_WAVE i.WP5 [pw = wt]
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG1.txt, append tex ctitle(Mod&Sev) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_2)
reg RS_sb INCOME_2 i.YEAR_WAVE i.WP5 [pw = wt]
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG1.txt, append  tex ctitle(Severe) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_2)
reg RS_b INCOME_2 age age2 gender_f educ_b i.WP1223 rural HHsize_n child2adult i.YEAR_WAVE i.WP5 [pw = wt]
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG1.txt, append  tex ctitle(Any) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_2)
reg RS_msb INCOME_2 age age2 gender_f educ_b i.WP1223 rural HHsize_n child2adult i.YEAR_WAVE i.WP5 [pw = wt]
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG1.txt, append  tex ctitle(Mod&Sev) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_2)
reg RS_sb INCOME_2 age age2 gender_f educ_b i.WP1223 rural HHsize_n child2adult i.YEAR_WAVE i.WP5 [pw = wt]
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG1.txt, append  tex ctitle(Severe) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_2)
/* REGRESSION TABLE ** ALTERNATIVES **
reg RS_b INCOME_2 i.YEAR_WAVE##i.WP5
reg RS_msb INCOME_2 i.YEAR_WAVE##i.WP5
reg RS_sb INCOME_2 i.YEAR_WAVE##i.WP5
reg RS_b INCOME_2 age age2 sex educ_b i.WP1223 rural HHsize_n child2adult i.YEAR_WAVE##i.WP5 
reg RS_msb INCOME_2 age age2 sex educ_b i.WP1223 rural HHsize_n child2adult i.YEAR_WAVE##i.WP5
reg RS_sb INCOME_2 age age2 sex educ_b i.WP1223 rural HHsize_n child2adult i.YEAR_WAVE##i.WP5
*/


********************************
*** DIFFERENT SPECIFICATIONS ***
********************************
// REGRESSION TABLE 2 - Log, HH income
reg RS_b log_INCOME_2 i.YEAR_WAVE i.WP5 [pw = wt]
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG2.txt, replace tex ctitle(Any) label addtext(Country FE, YES, Year FE, YES) keep(log_INCOME_2)
reg RS_msb log_INCOME_2 i.YEAR_WAVE i.WP5 [pw = wt]
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG2.txt, append tex ctitle(Mod&Sev) label addtext(Country FE, YES, Year FE, YES) keep(log_INCOME_2)
reg RS_sb log_INCOME_2 i.YEAR_WAVE i.WP5 [pw = wt]
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG2.txt, append  tex ctitle(Severe) label addtext(Country FE, YES, Year FE, YES) keep(log_INCOME_2)
reg RS_b log_INCOME_2 age age2 gender_f educ_b i.WP1223 rural HHsize_n child2adult  i.YEAR_WAVE i.WP5 [pw = wt]
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG2.txt, append  tex ctitle(Any) label addtext(Country FE, YES, Year FE, YES) keep(log_INCOME_2)
reg RS_msb log_INCOME_2 age age2 gender_f educ_b i.WP1223 rural HHsize_n child2adult  i.YEAR_WAVE i.WP5 [pw = wt]
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG2.txt, append  tex ctitle(Mod&Sev) label addtext(Country FE, YES, Year FE, YES) keep(log_INCOME_2)
reg RS_sb log_INCOME_2 age age2 gender_f educ_b i.WP1223 rural HHsize_n child2adult i.YEAR_WAVE i.WP5 [pw = wt]
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG2.txt, append  tex ctitle(Severe) label addtext(Country FE, YES, Year FE, YES) keep(log_INCOME_2)

// REGRESSION TABLE 3 - Linear with quadratic and cubic, HH income
//-9.54e-07 parsed down -> -6.77e-07
//-2.63e-06 full model

reg RS_b INCOME_2 INCOME_22 INCOME_23 i.YEAR_WAVE i.WP5 [pw = wt]
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG3.txt, replace tex ctitle(Any) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_2 INCOME_22 INCOME_23)
reg RS_msb INCOME_2 INCOME_22 INCOME_23 i.YEAR_WAVE i.WP5 [pw = wt]
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG3.txt, append tex ctitle(Mod&Sev) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_2 INCOME_22 INCOME_23)
reg RS_sb INCOME_2 INCOME_22 INCOME_23 i.YEAR_WAVE i.WP5 [pw = wt]
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG3.txt, append  tex ctitle(Severe) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_2 INCOME_22 INCOME_23)
reg RS_b INCOME_2 INCOME_22 INCOME_23 age age2 gender_f educ_b i.WP1223 rural HHsize_n child2adult i.YEAR_WAVE i.WP5 [pw = wt]
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG3.txt, append  tex ctitle(Any) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_2 INCOME_22 INCOME_23)
reg RS_msb INCOME_2 INCOME_22 INCOME_23 age age2 gender_f educ_b i.WP1223 rural HHsize_n child2adult  i.YEAR_WAVE i.WP5 [pw = wt]
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG3.txt, append  tex ctitle(Mod&Sev) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_2 INCOME_22 INCOME_23)
reg RS_sb INCOME_2 INCOME_22 INCOME_23 age age2 gender_f educ_b i.WP1223 rural HHsize_n child2adult i.YEAR_WAVE i.WP5 [pw = wt]
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG3.txt, append  tex ctitle(Severe) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_2 INCOME_22 INCOME_23)

*************************************
*** ASSOCIATION BETWEEN COUNTRIES ***
*************************************
gen region_c = 0
replace region_c =1 if Region =="East Asia & Pacific"
replace region_c =2 if Region =="Europe & Central Asia"
replace region_c =3 if Region =="Latin America & Caribbean"
replace region_c =4 if Region =="Middle East & North Africa"
replace region_c =5 if Region =="North America"
replace region_c =6 if Region =="South Asia"
replace region_c =7 if Region =="Sub-Saharan Africa"

// REGRESSION 
/*TABLE 4: By region - HH income, linear - NOT NEEDED
//INCOME - Any
forvalues j = 1/5 {
	forvalues i = 1/7 {
		reg RS_b INCOME_2 INCOME_22 INCOME_23 i.YEAR_WAVE i.WP5 if region_c ==`i' & INCOME_5 == `j' [pw = wt]
		matrix A`i'_b = r(table)
		scalar b`i'_b`j' = A`i'_b[1,1]
		
	}
}
forvalues i = 1/7 {
	forvalues j = 1/5 {
		display b`i'_b`j'
	}
}

//INCOME - Mod&Sev
forvalues j = 1/5 {
	forvalues i = 1/7 {
		reg RS_msb INCOME_2 INCOME_22 INCOME_23 i.YEAR_WAVE i.WP5 if region_c ==`i' & INCOME_5 == `j' [pw = wt]
		matrix A`i'_b = r(table)
		scalar b`i'_b`j' = A`i'_b[1,1]
	}
}
forvalues i = 1/7 {
	forvalues j = 1/5 {
		display b`i'_b`j'
	}
}

//INCOME - Severe
forvalues j = 1/5 {
	forvalues i = 1/7 {
		reg RS_sb INCOME_2 INCOME_22 INCOME_23 i.YEAR_WAVE i.WP5 if region_c ==`i' & INCOME_5 == `j' [pw = wt]
		matrix A`i'_b = r(table)
		scalar b`i'_b`j' = A`i'_b[1,1]
	}
}
forvalues i = 1/7 {
	forvalues j = 1/5 {
		display b`i'_b`j'
	}
}
*/

// REGRESSION TABLE 5: By Income Quintile - HH income, linear
** by INCOME Quintile
//INCOME Quintiles - Any
forvalues i = 1/5 {
	reg RS_b INCOME_2 i.YEAR_WAVE i.WP5 if INCOME_5 ==`i'
	matrix A`i'_bq = r(table)
	scalar b`i'_bq = A`i'_bq[1,1]
	scalar se`i'_bq = A`i'_bq[2,1]
}
forvalues i = 1/5 {
	display b`i'_bq
	display se`i'_bq
}
//Income Quintiles - Mod&Sev
forvalues i = 1/5 {
	reg RS_b INCOME_2 i.YEAR_WAVE i.WP5 if INCOME_5 ==`i'
	matrix A`i'_msbq = r(table)
	scalar b`i'_msbq = A`i'_msbq[1,1]
	scalar se`i'_msbq = A`i'_msbq[2,1]
}
forvalues i = 1/5 {
	display b`i'_msbq
	display se`i'_msbq
}
//INCOME Quintiles - Severe
forvalues i = 1/5 {
	reg RS_b INCOME_2 i.YEAR_WAVE i.WP5 if INCOME_5 ==`i'
	matrix A`i'_sbq = r(table)
	scalar b`i'_sbq = A`i'_sbq[1,1]
	scalar se`i'_sbq = A`i'_sbq[2,1]
}
forvalues i = 1/5 {
	display b`i'_sbq
	display se`i'_sbq



scalar se`i'_ba = A`i'_ba[2,1]
}





*************************************
*** OTHER STUFF ***
*************************************
// REGRESSION TABLE: By Gender - HH income, linear
** by GENDER if [f==1] 
/*
//female - Any
forvalues i = 0/1 {
	reg RS_b INCOME_2 i.YEAR_WAVE i.WP5 if gender_f ==`i'
	matrix A`i'_bg = r(table)
	scalar b`i'_bg = A`i'_bg[1,1]
	scalar se`i'_bg = A`i'_bg[2,1]
}
forvalues i = 0/1 {
	display b`i'_bg
	display se`i'_bg
}
//female - Mod&Sev
forvalues i = 0/1 {
	reg RS_msb INCOME_2 i.YEAR_WAVE i.WP5 if gender_f ==`i'
	matrix A`i'_msbg = r(table)
	scalar b`i'_msbg = A`i'_msbg[1,1]
	scalar se`i'_msbg = A`i'_msbg[2,1]
}
forvalues i = 0/1 {
	display b`i'_msbg
	display se`i'_msbg
}
//female- Severe
forvalues i = 0/1 {
	reg RS_b INCOME_2 i.YEAR_WAVE i.WP5 if gender_f ==`i'
	matrix A`i'_sbg = r(table)
	scalar b`i'_sbg = A`i'_sbg[1,1]
	scalar se`i'_sbg = A`i'_sbg[2,1]
}
forvalues i = 0/1 {
	display b`i'_sbg
	display se`i'_sbg
}
*/



*******************EXTRAS************************

//NEED TO FIX
forvalues i = 1/7 { **FIX LATER**
reg RS_b INCOME_2 i.YEAR_WAVE i.WP5
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG4_`i'.txt, replace tex ctitle(Any) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_2)
reg RS_msb INCOME_2 i.YEAR_WAVE i.WP5
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG4_`i'.txt, append tex ctitle(Mod&Sev) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_2)
reg RS_sb INCOME_2 i.YEAR_WAVE i.WP5
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG4_`i'.txt, append  tex ctitle(Severe) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_2) 
reg RS_b INCOME_2 age age2 gender_f educ_b i.WP1223 rural HHsize_n child2adult  i.YEAR_WAVE i.WP5
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG4_`i'.txt, append  tex ctitle(Any) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_2)
reg RS_msb INCOME_2 age age2 gender_f educ_b i.WP1223 rural HHsize_n child2adult  i.YEAR_WAVE i.WP5
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG4_`i'.txt, append  tex ctitle(Mod&Sev) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_2)
reg RS_sb INCOME_2 age age2 gender_f educ_b i.WP1223 rural HHsize_n child2adult i.YEAR_WAVE i.WP5
outreg2 using U:\FIES\Visualizations\Regression_Tables\REG4_`i'.txt, append  tex ctitle(Severe) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_2)
}

reg RS_b INCOME_2 i.YEAR_WAVE i.WP5
matrix A1 = r(table)
scalar b1 = A1[1,1]
scalar se1 = A1[2,1]
display b1
display se1





/* ** EXTRA VARS **
gen educ = 0 
replace educ = 1 if education == "Secondary - 3 year TertiarySecondary education and some education beyond secondary education (9-15 years of educatio"
replace educ = 2 if education == "Completed four years of education beyond high school and/or received a 4-year college degree."
gen INCOMEt2 = INCOMEt^2
gen INCOMEt3 = INCOMEt^3
*/
