***********************************
*** CORRECTED REGRESSION TABLES ***
***********************************
clear

* Upload the data
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis9.dta", replace

* Destring INCOME_WB
replace INCOME_WB = "123456" if INCOME_WB == ""
replace INCOME_WB = "123456" if INCOME_WB == "null"
destring INCOME_WB, replace
replace INCOME_WB = . if INCOME_WB == 123456
* Keep only 2018
drop if YEAR_WAVE != 2018
* Save in a new file
save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis_9_2018.dta", replace
* Fresh Start
clear
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis_9_2018.dta", replace
* Terciles binning
tab WP5 if INCOME_4 == .
replace WP5 =. if WP51 == "Venezuela"
replace WP5 =. if WP51 == "Niger"
replace WP5 =. if WP51 == "Mauritania"
set trace on
levelsof WP5 if YEAR_WAVE == 2018
gen INCOME_t = 0 
foreach i in `r(levels)' {
	quietly xtile INCOME_t_ph = INCOME_4 if WP5 == `i' , nq(3)
	replace INCOME_t = INCOME_t_ph if WP5 == `i'
	drop INCOME_t_ph
}
set trace off
tab INCOME_t
replace WP5 = 32 if WP51 == "Venezuela"
replace WP5 = 60 if WP51 == "Mauritania"
replace WP5 = 62 if WP51 == "Niger"


*************************************
** POOLED Coefficients Across Bins **
*************************************
* Quintiles
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_5 == `i' & YEAR_WAVE == 2018 [pw = wt], ll(c_FULL) vce(robust)
	*estimates store Q`i'
	scalar b`i' =r(table)[1,1]
	scalar s`i' = r(table)[1,2]
}
di b1 b2 b3 b4 b5
di s1 s2 s3 s4 s5
* Terciles
forvalues i = 1/3 {
	tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.WP5 if INCOME_t == `i' [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	*estimates store Q`i'
	scalar b`i' =r(table)[1,1]
	scalar s`i' = r(table)[1,2]
}
di b1 b2 b3 // No convergence on rd2
di s1 s2 s3 
sum RS_global_FULL_alt



******************************************
** GEOGRAPHIES Coefficients Across Bins **
******************************************
* Quintiles
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.WP5 if INCOME_5 ==`i' & RegionA == "EAPa" [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	di r(table)[1,1]
	estimates store A`i'
}
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.WP5 if INCOME_5 ==`i' & RegionA == "EuCA" [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	di r(table)[1,1]
	estimates store B`i'
}
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.WP5 if INCOME_5 ==`i' & RegionA == "LAmC" [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	di r(table)[1,1]
	estimates store C`i'
}
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.WP5 if INCOME_5 ==`i' & RegionA == "MEAN" [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	di r(table)[1,1]
	estimates store D`i'
}
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.WP5 if INCOME_5 ==`i' & RegionA == "NAm" [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	di r(table)[1,1]
	estimates store E`i'
}
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.WP5 if INCOME_5 ==`i' & RegionA == "SAs"  [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	di r(table)[1,1]
	estimates store F`i'
}
forvalues i = 1/5 {
	quietly tobit RS_global_drop19_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.WP5 if INCOME_5 ==`i' & RegionA == "SSA"  [pw = wt], ll(c_FULL) vce(robust) iterate(50)
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
			,  rescale(2.2) drop(_cons) xline(0) keep(INCOME_IHS) leg(off) xtitle(Standard Deviations from Mean Rasch Score)
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
			, rescale(2.2) drop(_cons) xline(0) keep(INCOME_IHS) leg(off) xtitle(Standard Deviations from Mean Rasch Score)
*/	

* Terciles
forvalues i = 1/3 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_t ==`i' & RegionA == "EAPa"  [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	di r(table)[1,1]
	estimates store A`i'
}
forvalues i = 1/3 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_t ==`i' & RegionA == "EuCA" [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	di r(table)[1,1]
	estimates store B`i'
}
forvalues i = 1/3 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_t ==`i' & RegionA == "LAmC" [pw = wt], ll(c_FULL) vce(robust)
	di r(table)[1,1]
	estimates store C`i'
}
forvalues i = 1/3 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_t ==`i' & RegionA == "MEAN" [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	di r(table)[1,1]
	estimates store D`i'
}
forvalues i = 1/3 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_t ==`i' & RegionA == "NAm" [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	di r(table)[1,1]
	estimates store E`i'
}
forvalues i = 1/3 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_t ==`i' & RegionA == "SAs" [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	di r(table)[1,1]
	estimates store F`i'
}
forvalues i = 1/3 {
	quietly tobit RS_global_drop19_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_t ==`i' & RegionA == "SSA" [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	di r(table)[1,1]
	estimates store G`i'

}

/* WITH NORTH AMERICA
cap label variable INCOME_IHS "IHST Income"
coefplot 	(A1,  pstyle(p1) offset(1.6) nokey) ///
			(A2, label(EAPa) pstyle(p1) offset(1.5) mlabel("East Asia & Pacific") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(A3,  pstyle(p1) offset(1.4) nokey) ///
			(B1,  pstyle(p2) offset(1.1) nokey) ///
			(B2, label(EuCA) pstyle(p2) offset(1) mlabel("Europe & Central Asia") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(B3,  pstyle(p2) offset(0.9) nokey) ///
			(C1,  pstyle(p3) offset(0.6) nokey) ///
			(C2, label(LAmC) pstyle(p3) offset(0.5) mlabel("Latin America & Caribbean") mlabposition(12) mlabgap(3) mlabcolor(black) ) ///
			(C3,  pstyle(p3) offset(0.4) nokey ) ///
			(D1,  pstyle(p4) offset(0.1) nokey ) ///
			(D2, label(MEAN) pstyle(p4) offset(0.0) mlabel("Middle East & North Africa") mlabposition(12) mlabgap(3) mlabcolor(black) ) ///
			(D3,  pstyle(p4) offset(-0.1) nokey) ///
			(E1,  pstyle(p5) offset(-0.4) nokey ) ///
			(E2, label(NAm) pstyle(p5) offset(-0.5) mlabel("North America") mlabposition(12) mlabgap(3) mlabcolor(black) ) ///
			(E3,  pstyle(p5) offset(-0.6) nokey) ///
			(F1,  pstyle(p6) offset(-0.9) nokey) ///
			(F2, label(SAs) pstyle(p6) offset(-1) mlabel("South Asia") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(F3,  pstyle(p6) offset(-1.1) nokey) ///
			(G1,  pstyle(p7) offset(-1.4) nokey) ///
			(G2, label(SSA) pstyle(p7) offset(-1.5) mlabel("Sub-Saharan Africa") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(G3,  pstyle(p7) offset(-1.6) nokey ) ///
			, rescale(2.2) drop(_cons) xline(0) keep(INCOME_IHS) leg(off) xtitle(Standard Deviations from Mean Rasch Score)
*/			
/* NO NORTH AMERICA
cap label variable INCOME_IHS "SEFI"
coefplot 	(A1,  msymbol(D) pstyle(p1) offset(1.6) nokey) ///
			(A2, label(EAPa) pstyle(p1) offset(1.5) mlabel("East Asia & Pacific") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(A3,  msymbol(T) pstyle(p1) offset(1.4) nokey) ///
			(B1,  msymbol(D) pstyle(p2) offset(1.1) nokey) ///
			(B2, label(EuCA) pstyle(p2) offset(1) mlabel("Europe & Central Asia") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(B3,  msymbol(T) pstyle(p2) offset(0.9) nokey) ///
			(C1,  msymbol(D) pstyle(p3) offset(0.6) nokey) ///
			(C2, label(LAmC) pstyle(p3) offset(0.5) mlabel("Latin America & Caribbean") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(C3,  msymbol(T) pstyle(p3) offset(0.45) nokey) ///
			(D1,  msymbol(D) pstyle(p4) offset(0.1) nokey) ///
			(D2, label(MEAN) pstyle(p4) offset(0.0) mlabel("Middle East & North Africa") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(D3,  msymbol(T) pstyle(p4) offset(-0.1) nokey) ///
			(F1,  msymbol(D) pstyle(p6) offset(-0.4) nokey) ///
			(F2, label(SAs) pstyle(p6) offset(-0.5) mlabel("South Asia") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(F3,  msymbol(T) pstyle(p6) offset(-0.6) nokey) ///
			(G1,  msymbol(D) pstyle(p7) offset(-0.9) nokey) ///
			(G2, label(SSA) pstyle(p7) offset(-1) mlabel("Sub-Saharan Africa") mlabposition(12) mlabgap(3) mlabcolor(black)) ///
			(G3, msymbol(T) pstyle(p7) offset(-1.10) nokey) ///
			, rescale(2.2) drop(_cons) xline(0) keep(INCOME_IHS) legend(label(1 "Q1" 2 "Q2" 3 "Q3")) xtitle(Standard Deviations from Mean Rasch Score)
*/	



* VERSION 1
/*
cap label variable INCOME_IHS "SEFI"
coefplot (A1, msymbol(D) pstyle(p1) offset(1.35) mlabel("Lowest Income Tercile") mlabposition(9) mlabgap(43) mlabcolor(black) nokey) ///
		 (A2, msymbol(O) label(EAPa) pstyle(p1) offset(1.25) mlabel("Middle Income Tercile") mlabposition(9) mlabgap(16) mlabcolor(black) ) ///
		 (A3, msymbol(T) pstyle(p1) offset(1.15) mlabel("Highest Income Tercile") mlabposition(9) mlabgap(26) mlabcolor(black) nokey) ///
		 (B1, msymbol(D) pstyle(p2) offset(0.85) mlabel("T1") mlabposition(3) mlabgap(2) mlabcolor(black) nokey) ///
		 (B2, msymbol(O) label(EuCA) pstyle(p2) offset(0.75) mlabel("T2") mlabposition(3) mlabgap(2) mlabcolor(black) ) ///
		 (B3, msymbol(T) pstyle(p2) offset(0.65) mlabel("T3") mlabposition(3) mlabgap(2) mlabcolor(black) nokey) ///
		 (C1, msymbol(D) pstyle(p3) offset(0.35) mlabel("T1") mlabposition(3) mlabgap(2) mlabcolor(black) nokey) ///
		 (C2, msymbol(O) label(LAmC) pstyle(p3) offset(0.25) mlabel("T2") mlabposition(3) mlabgap(2) mlabcolor(black) ) ///
		 (C3, msymbol(T) pstyle(p3) offset(0.15) mlabel("T3") mlabposition(3) mlabgap(2) mlabcolor(black) nokey) ///
		 (D1, msymbol(D) pstyle(p4) offset(-0.15) mlabel("T1") mlabposition(3) mlabgap(2) mlabcolor(black) nokey) ///
		 (D2, msymbol(O) label(MENA) pstyle(p4) offset(-0.25) mlabel("T2") mlabposition(3) mlabgap(2) mlabcolor(black) ) ///
		 (D3, msymbol(T) pstyle(p4) offset(-0.35) mlabel("T3") mlabposition(3) mlabgap(2) mlabcolor(black) nokey) ///
		 (F1, msymbol(D) pstyle(p6) offset(-0.65) mlabel("T1") mlabposition(3) mlabgap(2) mlabcolor(black) nokey) ///
		 (F2, msymbol(O) label(SAs) pstyle(p6) offset(-0.75) mlabel("T2") mlabposition(3) mlabgap(2) mlabcolor(black) ) ///
		 (F3, msymbol(T) pstyle(p6) offset(-0.85) mlabel("T3") mlabposition(3) mlabgap(2) mlabcolor(black) nokey) ///
		 (G1, msymbol(D) pstyle(p7) offset(-1.15) mlabel("T1") mlabposition(3) mlabgap(2) mlabcolor(black) nokey) ///
		 (G2, msymbol(O) label(SSA) pstyle(p7) offset(-1.25) mlabel("T2") mlabposition(3) mlabgap(2) mlabcolor(black) ) ///
		 (G3, msymbol(T) pstyle(p7) offset(-1.35) mlabel("T3") mlabposition(3) mlabgap(2) mlabcolor(black) nokey) ///
	, rescale(2.2) drop(_cons) xline(0) keep(INCOME_IHS) legend(rows(1)) xtitle(Standard Deviations from Mean Rasch Score)
*/
		 
		 


* VERSION 2
/*
cap label variable INCOME_IHS "SEFI"
coefplot (A1, msymbol(D) pstyle(p1) offset(1.35) mlabel("Lowest Income Tercile") mlabposition(9) mlabgap(40) mlabcolor(black) nokey) ///
		 (A2, msymbol(O) label(EAPa) pstyle(p1) offset(1.25) mlabel("Middle Income Tercile") mlabposition(9) mlabgap(13) mlabcolor(black) ) ///
		 (A3, msymbol(T) pstyle(p1) offset(1.15) mlabel("Highest Income Tercile") mlabposition(9) mlabgap(23) mlabcolor(black) nokey) ///
		 (B1, msymbol(D) pstyle(p2) offset(0.85)  nokey) ///
		 (B2, msymbol(O) label(EuCA) offset(0.75) pstyle(p2) ) ///
		 (B3, msymbol(T) pstyle(p2) offset(0.65) nokey) ///
		 (C1, msymbol(D) pstyle(p3) offset(0.35) nokey) ///
		 (C2, msymbol(O) label(LAmC) pstyle(p3) offset(0.25)  ) ///
		 (C3, msymbol(T) pstyle(p3) offset(0.15) nokey) ///
		 (D1, msymbol(D) pstyle(p4) offset(-0.15)  nokey) ///
		 (D2, msymbol(O) label(MENA) pstyle(p4) offset(-0.25) ) ///
		 (D3, msymbol(T) pstyle(p4) offset(-0.35) nokey) ///
		 (F1, msymbol(D) pstyle(p6) offset(-0.65)  nokey) ///
		 (F2, msymbol(O) label(SAs) pstyle(p6) offset(-0.75) ) ///
		 (F3, msymbol(T) pstyle(p6) offset(-0.85)  nokey) ///
		 (G1, msymbol(D) pstyle(p7) offset(-1.15)  nokey) ///
		 (G2, msymbol(O) label(SSA) pstyle(p7) offset(-1.25) ) ///
		 (G3, msymbol(T) pstyle(p7) offset(-1.35) nokey) ///
	, rescale(2.2) drop(_cons) xline(0) keep(INCOME_IHS) legend(rows(1)) xtitle(Standard Deviations from Mean)
*/
		 
	









*************************************
** GDPPC GROUPINGS - by World Bank **
*************************************
* Adjustments
tab WP5 if Incomegroup == "High income"
sum INCOME_WB if Incomegroup == "High income"
sum INCOME_WB if Incomegroup == "Upper middle income"
sum INCOME_WB if Incomegroup == "Lower middle income"
sum INCOME_WB if Incomegroup == "Low income"
** QUINTILES **
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.WP5 if INCOME_5 ==`i' & Incomegroup == "High income" [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	scalar b`i' = r(table)[1,1]
	scalar s`i' = r(table)[1,2]
	estimates store H`i'
}
di b1 b2 b3 b4 b5
di s1 s2 s3 s4 s5
* Upper middle
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.WP5 if INCOME_5 ==`i' & Incomegroup == "Upper middle income" [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	scalar b`i' = r(table)[1,1]
	scalar s`i' = r(table)[1,2]
	estimates store I`i'
}
di b1 b2 b3 b4 b5
di s1 s2 s3 s4 s5
* Lower middle
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.WP5 if INCOME_5 ==`i' & Incomegroup == "Lower middle income" [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	scalar b`i' = r(table)[1,1]
	scalar s`i' = r(table)[1,2]
	estimates store J`i'
}
di b1 b2 b3 b4 b5
di s1 s2 s3 s4 s5
* Low income
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.WP5 if INCOME_5 ==`i' & Incomegroup == "Low income" [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	scalar b`i' = r(table)[1,1]
	scalar s`i' = r(table)[1,2]
	estimates store K`i'
}
di b1 b2 b3 b4 b5
di s1 s2 s3 s4 s5
** TERCILES **
forvalues i = 1/3 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.WP5 if INCOME_t ==`i' & Incomegroup == "High income" [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	scalar b`i' = r(table)[1,1]
	scalar s`i' = r(table)[1,2]
	estimates store H`i'
}
di b1 b2 b3 
di s1 s2 s3 
forvalues i = 1/3 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.WP5 if INCOME_t ==`i' & Incomegroup == "Upper middle income" [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	scalar b`i' = r(table)[1,1]
	scalar s`i' = r(table)[1,2]
	estimates store I`i'
}
di b1 b2 b3 
di s1 s2 s3 
forvalues i = 1/3 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.WP5 if INCOME_t ==`i' & Incomegroup == "Lower middle income" [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	scalar b`i' = r(table)[1,1]
	scalar s`i' = r(table)[1,2]
	estimates store J`i'
}
di b1 b2 b3 
di s1 s2 s3 
forvalues i = 1/3 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.WP5 if INCOME_t ==`i' & Incomegroup == "Low income" [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	scalar b`i' = r(table)[1,1]
	scalar s`i' = r(table)[1,2]
	estimates store K`i'
}
di b1 b2 b3 
di s1 s2 s3 



********************************
** FOOD INSECURITY BY COUNTRY **
********************************
* Three groups (>10, >20, >30 Mod&Sev FS)
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
** QUINTILES **
* FI_MS1 Higher BIN - Lower prevalence countries
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.WP5 if INCOME_5 ==`i' & FI_MS1 == 0 [pw = wt], ll(c_FULL) vce(robust)
	scalar b`i' = r(table)[1,1]
	scalar s`i' = r(table)[1,2]
}
di b1 b2 b3 b4 b5
di s1 s2 s3 b4 b5
* FI_MS1 Lower BIN - Lower prevalence countries
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis  i.WP5 if INCOME_5 ==`i' &  FI_MS1 == 1 [pw = wt], ll(c_FULL) vce(robust)
	scalar b`i' = r(table)[1,1]
	scalar s`i' = r(table)[1,2]
}
di b1 b2 b3 b4 b5
di s1 s2 s3 b4 b5
* FI_S1 Higher BIN - higher prevalence countries
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.WP5 if INCOME_5 ==`i' & FI_S1 == 0 [pw = wt], ll(c_FULL) vce(robust)
	scalar b`i' = r(table)[1,1]
	scalar s`i' = r(table)[1,2]
}
di b1 b2 b3 b4 b5
di s1 s2 s3 b4 b5
* FI_S1 Lower BIN - higher prevalence countries
forvalues i = 1/5 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.WP5 if INCOME_5 ==`i' & FI_S1 == 1 [pw = wt], ll(c_FULL) vce(robust) iterate(100)
	scalar b`i' = r(table)[1,1]
	scalar s`i' = r(table)[1,2]
}
di b1 b2 b3 b4 b5
di s1 s2 s3 b4 b5
** TERCILES **
* FI_MS1 Higher BIN - Lower prevalence countries
forvalues i = 1/3 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.WP5 if INCOME_t ==`i' & FI_MS1 == 0 [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	scalar b`i' = r(table)[1,1]
	scalar s`i' = r(table)[1,2]
}
di b1 b2 b3 
di s1 s2 s3 
* FI_MS1 Lower BIN - Lower prevalence countries
forvalues i = 1/3 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis  i.WP5 if INCOME_t ==`i' &  FI_MS1 == 1 [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	scalar b`i' = r(table)[1,1]
	scalar s`i' = r(table)[1,2]
}
di b1 b2 b3 
di s1 s2 s3
* FI_S1 Higher BIN - higher prevalence countries
forvalues i = 1/3 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.WP5 if INCOME_t ==`i' & FI_S1 == 0 [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	scalar b`i' = r(table)[1,1]
	scalar s`i' = r(table)[1,2]
}
di b1 b2 b3 
di s1 s2 s3
* FI_S1 Lower BIN - higher prevalence countries
forvalues i = 1/3 {
	quietly tobit RS_global_FULL_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.WP5 if INCOME_t ==`i' & FI_S1 == 1 [pw = wt], ll(c_FULL) vce(robust) iterate(50)
	scalar b`i' = r(table)[1,1]
	scalar s`i' = r(table)[1,2]
}
di b1 b2 b3 
di s1 s2 s3




* Sandbox  ******************************************************
tobit RS_global_drop19_alt INCOME_IHS age_mis age2_mis gender_f educ_b_mis rural_mis i.WP1223_mis HHsize_n_mis child2adult_mis i.YEAR_WAVE i.WP5 if INCOME_5 == 1 [pw = wt], ll(c_FULL) vce(robust)
coefplot (IHST_`i', label(Pooled)), drop(_cons) xline(0)
** Not good, all on same line
coefplot 	(A1 B1, label(Q1)) ///
			(A2 B2, label(Q2)) ///
			(A3 B3, label(Q3)) ///
			(A4 B4, label(Q4)) ///
			(A5 B5, label(Q5)) ///
			, drop(_cons) xline(0) keep(INCOME_IHS)

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














