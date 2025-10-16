***********************************
*** CORRECTED REGRESSION TABLES ***
***********************************
clear

* Upload the data
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis9.dta"

* Remove any observation that isn't relevant
drop n_adults-prob_sev area-countrynew EMP_2010-HHsize INDEX_CA M1 M30 WP1230 WP1230Recoded RS_par PROB_mod_sev PROB_sev log_INCOME_2 
* Destring INCOME_WB
replace INCOME_WB = "123456" if INCOME_WB == ""
replace INCOME_WB = "123456" if INCOME_WB == "null"
destring INCOME_WB, replace
replace INCOME_WB = . if INCOME_WB == 123456

** POOLED Coefficients Across Quintiles
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_5 == `i' & YEAR_WAVE == 2018 [pw = wt], ll(c_FULL) vce(robust)
	estimates store Q`i'
	scalar b`i' =r(table)[1,1]
	scalar s`i' = r(table)[1,2]
}
di b1 b2 b3 b4 b5
di s1 s2 s3 s4 s5
sum RS_global_FULL_alt



** GEOGRAPHIES Coefficients Across Quintiles
* Region 1 EAPa A
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_5 ==`i' & RegionA == "EAPa" & YEAR_WAVE == 2018 [pw = wt], ll(c_FULL) vce(robust)
	di r(table)[1,1]
	estimates store A`i'
}
* Region 2 EuCA B
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_5 ==`i' & RegionA == "EuCA" & YEAR_WAVE == 2018 [pw = wt], ll(c_FULL) vce(robust)
	di r(table)[1,1]
	estimates store B`i'
}
* Region 3 LAmC C
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_5 ==`i' & RegionA == "LAmC" & YEAR_WAVE == 2018 [pw = wt], ll(c_FULL) vce(robust)
	di r(table)[1,1]
	estimates store C`i'
}
* Region 4 MEAN D
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_5 ==`i' & RegionA == "MEAN" & YEAR_WAVE == 2018 [pw = wt], ll(c_FULL) vce(robust)
	di r(table)[1,1]
	estimates store D`i'
}
* Region 5 NAm E
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_5 ==`i' & RegionA == "NAm" & YEAR_WAVE == 2018 [pw = wt], ll(c_FULL) vce(robust)
	di r(table)[1,1]
	estimates store E`i'
}
* Region 6 SAs F
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_5 ==`i' & RegionA == "SAs" & YEAR_WAVE == 2018 [pw = wt], ll(c_FULL) vce(robust)
	di r(table)[1,1]
	estimates store F`i'
}
* Region 7 SSA G 
forvalues i = 1/5 {
	quietly tobit RS_global_drop19_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_5 ==`i' & RegionA == "SSA" & YEAR_WAVE == 2018 [pw = wt], ll(c_FULL) vce(robust)
	di r(table)[1,1]
	estimates store G`i'

}
/* WITH NORTH AMERICA
cap label variable INCOME_IHS "IHST Income"
coefplot 	(A1,  pstyle(p1) offset(1.6) nokey) ///
			(A2,  pstyle(p1) offset(1.55) nokey) ///
			(A3, label(EAPa) pstyle(p1) offset(1.5) mlabel("East Asia & Pacific") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(A4,  pstyle(p1) offset(1.45) nokey) ///
			(A5,  pstyle(p1) offset(1.4) nokey) ///
			(B1,  pstyle(p2) offset(1.1) nokey) ///
			(B2,  pstyle(p2) offset(1.05) nokey) ///
			(B3, label(EuCA) pstyle(p2) offset(1) mlabel("Europe & Central Asia") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(B4,  pstyle(p2) offset(0.95) nokey) ///
			(B5,  pstyle(p2) offset(0.9) nokey) ///
			(C1,  pstyle(p3) offset(0.6) nokey) ///
			(C2,  pstyle(p3) offset(0.55) nokey) ///
			(C3, label(LAmC) pstyle(p3) offset(0.5) mlabel("Latin America & Caribbean") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(C4,  pstyle(p3) offset(0.45) nokey) ///
			(C5,  pstyle(p3) offset(0.45) nokey) ///
			(D1,  pstyle(p4) offset(0.1) nokey) ///
			(D2,  pstyle(p4) offset(0.05) nokey) ///
			(D3, label(MEAN) pstyle(p4) offset(0.0) mlabel("Middle East & North Africa") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(D4,  pstyle(p4) offset(-0.05) nokey) ///
			(D5,  pstyle(p4) offset(-0.1) nokey) ///
			(E1,  pstyle(p5) offset(-0.4) nokey) ///
			(E2,  pstyle(p5) offset(-0.45) nokey) ///
			(E3, label(NAm) pstyle(p5) offset(-0.5) mlabel("North America") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(E4,  pstyle(p5) offset(-0.55) nokey) ///
			(E5,  pstyle(p5) offset(-0.6) nokey) ///
			(F1,  pstyle(p6) offset(-0.9) nokey) ///
			(F2,  pstyle(p6) offset(-0.95) nokey) ///
			(F3, label(SAs) pstyle(p6) offset(-1) mlabel("South Asia") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(F4,  pstyle(p6) offset(-1.05) nokey) ///
			(F5,  pstyle(p6) offset(-1.1) nokey) ///
			(G1,  pstyle(p7) offset(-1.4) nokey) ///
			(G2,  pstyle(p7) offset(-1.45) nokey) ///
			(G3, label(SSA) pstyle(p7) offset(-1.5) mlabel("Sub-Saharan Africa") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(G4,  pstyle(p7) offset(-1.55) nokey) ///
			(G5,  pstyle(p7) offset(-1.6) nokey) ///
			, drop(_cons) xline(0) keep(INCOME_IHS) leg(off)
*/			
/* NO NORTH AMERICA
cap label variable INCOME_IHS "FIES Semi-Elasticity"
coefplot 	(A1,  pstyle(p1) offset(1.6) nokey) ///
			(A2,  pstyle(p1) offset(1.55) nokey) ///
			(A3, label(EAPa) pstyle(p1) offset(1.5) mlabel("East Asia & Pacific") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(A4,  pstyle(p1) offset(1.45) nokey) ///
			(A5,  pstyle(p1) offset(1.4) nokey) ///
			(B1,  pstyle(p2) offset(1.1) nokey) ///
			(B2,  pstyle(p2) offset(1.05) nokey) ///
			(B3, label(EuCA) pstyle(p2) offset(1) mlabel("Europe & Central Asia") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(B4,  pstyle(p2) offset(0.95) nokey) ///
			(B5,  pstyle(p2) offset(0.9) nokey) ///
			(C1,  pstyle(p3) offset(0.6) nokey) ///
			(C2,  pstyle(p3) offset(0.55) nokey) ///
			(C3, label(LAmC) pstyle(p3) offset(0.5) mlabel("Latin America & Caribbean") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(C4,  pstyle(p3) offset(0.45) nokey) ///
			(C5,  pstyle(p3) offset(0.45) nokey) ///
			(D1,  pstyle(p4) offset(0.1) nokey) ///
			(D2,  pstyle(p4) offset(0.05) nokey) ///
			(D3, label(MEAN) pstyle(p4) offset(0.0) mlabel("Middle East & North Africa") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(D4,  pstyle(p4) offset(-0.05) nokey) ///
			(D5,  pstyle(p4) offset(-0.1) nokey) ///
			(F1,  pstyle(p6) offset(-0.4) nokey) ///
			(F2,  pstyle(p6) offset(-0.45) nokey) ///
			(F3, label(SAs) pstyle(p6) offset(-0.5) mlabel("South Asia") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(F4,  pstyle(p6) offset(-0.55) nokey) ///
			(F5,  pstyle(p6) offset(-0.6) nokey) ///
			(G1,  pstyle(p7) offset(-0.9) nokey) ///
			(G2,  pstyle(p7) offset(-.95) nokey) ///
			(G3, label(SSA) pstyle(p7) offset(-1) mlabel("Sub-Saharan Africa") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(G4,  pstyle(p7) offset(-1.05) nokey) ///
			(G5,  pstyle(p7) offset(-1.10) nokey) ///
			, rescale(2.2) drop(_cons) xline(0) keep(INCOME_IHS) leg(off) 
*/	



** GDPPC GROUPINGS - by World Bank
tab WP5 if Incomegroup == "High income"
sum INCOME_WB if YEAR_WAVE ==2018 & Incomegroup == "High income"
sum INCOME_WB if YEAR_WAVE ==2018 & Incomegroup == "Upper middle income"
sum INCOME_WB if YEAR_WAVE ==2018 & Incomegroup == "Lower middle income"
sum INCOME_WB if YEAR_WAVE ==2018 & Incomegroup == "Low income"
* High income
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_5 ==`i' & Incomegroup == "High income" & YEAR_WAVE == 2018 [pw = wt], ll(c_FULL) vce(robust) iterate(100)
	scalar h`i' = r(table)[1,1]
	scalar hs`i' = r(table)[1,2]
	estimates store H`i'
}
di h1 h2 h3 h4 h5
di hs1 hs2 hs3 hs4 hs5
* Upper middle
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_5 ==`i' & Incomegroup == "Upper middle income" & YEAR_WAVE == 2018 [pw = wt], ll(c_FULL) vce(robust)
	scalar i`i' = r(table)[1,1]
	scalar is`i' = r(table)[1,2]
	estimates store I`i'
}
di i1 i2 i3 i4 i5
di is1 is2 is3 is4 is5
* Lower middle
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_5 ==`i' & Incomegroup == "Lower middle income" & YEAR_WAVE == 2018 [pw = wt], ll(c_FULL) vce(robust)
	scalar j`i' = r(table)[1,1]
	scalar js`i' = r(table)[1,2]
	estimates store J`i'
}
di j1 j2 j3 j4 j5
di js1 js2 js3 js4 js5
* Low income
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_5 ==`i' & Incomegroup == "Low income" & YEAR_WAVE == 2018 [pw = wt], ll(c_FULL) vce(robust)
	scalar k`i' = r(table)[1,1]
	scalar ks`i' = r(table)[1,2]
	estimates store K`i'
}
di k1 k2 k3 k4 k5 
di ks1 ks2 ks3 ks4 ks5



** Cleaning
clear
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis9.dta", replace
drop if YEAR_WAVE != 2018



** FOOD INSECURITY BY COUNTRY - three groups (>10, >20, >30 Mod&Sev FS)
clear
* Data
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis9.dta", replace
drop if YEAR_WAVE != 2018
* Calculate thresholds
gen FI_ms = 0
replace FI_ms = 1 if RS_global_FULL_alt > -0.394
gen FI_s = 0
replace FI_s = 1 if RS_global_FULL_alt > 1.649
* Calculate prevalence
bysort WP5: egen FS_MS = mean(FI_ms)
bysort WP5: egen FS_S = mean(FI_s)
* Describe data
hist FS_MS, bin(20) percent
sum FS_MS, detail
hist FS_S, bin(20) percent
sum FS_S, detail
* Bin of Group FS_MS
gen FI_MS1 = 0  
replace FI_MS1 = 1 if FS_MS < .2
* Bin of Group FS_S
gen FI_S1 = 0
replace FI_S1 = 1 if FS_S < .05
* Describe the data
sum FI_MS1 FI_S1

* FI_MS1 Higher BIN - Lower prevalence countries
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_5 ==`i' & FI_MS1 == 0 [pw = wt], ll(c_FULL) vce(robust)
	scalar L`i' = r(table)[1,1]
	scalar Ls`i' = r(table)[1,2]
}
di L1 L2 L3 L4 L5 
* FI_MS1 Lower BIN - Lower prevalence countries
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_5 ==`i' &  FI_MS1 == 1 [pw = wt], ll(c_FULL) vce(robust)
	scalar M`i' = r(table)[1,1]
	scalar Ms`i' = r(table)[1,2]
}
di M1 M2 M3 M4 M5
* FI_S1 Higher BIN - higher prevalence countries
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_5 ==`i' & FI_S1 == 0 [pw = wt], ll(c_FULL) vce(robust)
	scalar N`i' = r(table)[1,1]
	scalar Ns`i' = r(table)[1,2]
}
di N1 N2 N3 N4 N5
* FI_S1 Lower BIN - higher prevalence countries
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_5 ==`i' & FI_S1 == 1 [pw = wt], ll(c_FULL) vce(robust) iterate(100)
	scalar O`i' = r(table)[1,1]
	scalar Os`i' = r(table)[1,2]
}
di O1 O2 O3 O4 O5
* Big Summary
di L1 L2 L3 L4 L5 
di M1 M2 M3 M4 M5 // relatively lower
di N1 N2 N3 N4 N5
di O1 O2 O3 O4 O5 // relatively lower





















* Sandbox (NEED REGIONS) ******************************************************
tobit RS_global_drop19_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_5 == 1 [pw = wt], ll(c_FULL) vce(robust)

coefplot (IHST_`i', label(Pooled)), drop(_cons) xline(0)
		
** Not good, all on same line
coefplot 	(A1 B1, label(Q1)) ///
			(A2 B2, label(Q2)) ///
			(A3 B3, label(Q3)) ///
			(A4 B4, label(Q4)) ///
			(A5 B5, label(Q5)) ///
			, drop(_cons) xline(0) keep(INCOME_IHS)
		
** Try again
coefplot	(B1, label(EuCA)) ///
			(B2, label(EuCA)) ///
			(B3, label(EuCA)) ///
			(B4, label(EuCA)) ///
			(B1, label(EuCA)) ///
			, drop(_cons) xline(0) keep(INCOME_IHS)
			
coefplot 	(A1 A2 A3 A4 A5, label(EAPa) offset (0.05)) ///
			(B1 B2 B3 B4 B5, label(EuCA) offset(0.05)) ///
			, drop(_cons) xline(0) keep(INCOME_IHS)

coefplot (IHST_A1 IHST_A2 , label(Pooled)), drop(_cons) xline(0) keep(IHST_A1 IHST_A2)

coefplot (IHST_A1, label(Pooled1)) (IHST_A2 ,label(Pooled2)), drop(_cons) xline(0)


** Basic Regression Model (Drop19)
/*
quietly tobit RS_global_drop19_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if RS_global_drop19 != . [pw = wt], ll(c_drop19) vce(robust)
nlcom (_b[INCOME_IHS]*m4)/(abs(m4_y)*sqrt(m4^2+1))
scalar b4 = r(table)[1,1]
scalar s4 = r(table)[2,1]
outreg2 using U:\FIES\Final_Tables\REG8.txt, append tex ctitle(Drop 19) label addtext(Country FE, YES, Year FE, YES) keep(INCOME_IHS)



display b1 b2 b3 b4 b5 b6
display s1 s2 s3 s4 s5 s6
*/














