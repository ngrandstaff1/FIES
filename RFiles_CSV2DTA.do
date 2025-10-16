**************************************************
** GLOBAL REFERENCE SCALE R TO STATA CONVERSION **
**************************************************
clear

** FULL SET **
* Basic Results: resC_ (b, se, infit, outfit)
import delimited "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\resC_FULL.csv"
destring *, replace ignore("NA") 
drop v1
foreach i in q1 q2 q3 q4 q5 q6 q7 q8 {
	ren `i' `i'_FULL
	ren `i'_seb `i'_seb_FULL
	ren o_`i' o_`i'_FULL
	ren i_`i' i_`i'_FULL
}
ren reliab reliab_FULL
ren reliabfl reliabfl_FULL
ren wp5 WP5
ren iso3 ISO3
save "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\resC_FULL.dta", replace

* Scale: scale_ (b_adj, removed items)
import delimited "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\scale_FULL.csv", encoding(ISO-8859-2) clear
drop check v1
ren wp5 WP5
ren iso3 ISO3
destring *, replace ignore("NA") 
foreach i in q1 q2 q3 q4 q5 q6 q7 q8 {
	ren `i' `i'_FULL
}
save "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\scale_FULL.dta", replace

* Equated Scale (b_global)
import delimited "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\equated_FULL.csv", clear
drop v1
ren wp5 WP5
ren iso3 ISO3
destring *, replace ignore("NA") 
forval i = 1/8 {
	ren t`i' e`i'_FULL
}
ren corr_comm corr_comm_FULL
save "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\equated_FULL.dta", replace

* Global Scale (as a row vector, corr_coeff)
import delimited "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\global_scale_FULL.csv", encoding(UTF-8) clear
drop v1
destring *, replace ignore("NA") 
forval i = 1/8 {
	ren s`i' gs`i'_FULL
}
save "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\global_scale_FULL.dta", replace

clear


** COVER 6 **
* Basic Results: resC_ (b, se, infit, outfit)
import delimited "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\resC_cover6.csv"
destring *, replace ignore("NA") 
drop v1
foreach i in q1 q2 q3 q4 q5 q6 q7 q8 {
	ren `i' `i'_cover6
	ren `i'_seb `i'_seb_cover6
	ren o_`i' o_`i'_cover6
	ren i_`i' i_`i'_cover6
}
ren reliab reliab_cover6
ren reliabfl reliabfl_cover6
ren wp5 WP5
ren iso3 ISO3
save "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\resC_cover6.dta", replace

* Scale: scale_ (b_adj, removed items)
import delimited "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\scale_cover6.csv", encoding(ISO-8859-2) clear
drop check v1
ren wp5 WP5
ren iso3 ISO3
destring *, replace ignore("NA") 
foreach i in q1 q2 q3 q4 q5 q6 q7 q8 {
	ren `i' `i'_cover6
}
save "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\scale_cover6.dta", replace

* Equated Scale (b_global)
import delimited "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\equated_cover6.csv", clear
drop v1
ren wp5 WP5
ren iso3 ISO3
destring *, replace ignore("NA") 
forval i = 1/8 {
	ren t`i' e`i'_cover6
}
ren corr_comm corr_comm_cover6
save "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\equated_cover6.dta", replace

* Global Scale (as a row vector, corr_coeff)
import delimited "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\global_scale_cover6.csv", encoding(UTF-8) clear
drop v1
destring *, replace ignore("NA") 
forval i = 1/8 {
	ren s`i' gs`i'_cover6
}
save "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\global_scale_cover6.dta", replace

clear


** DROP 14 **
* Basic Results: resC_ (b, se, infit, outfit)
import delimited "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\resC_drop14.csv"
destring *, replace ignore("NA") 
drop v1
foreach i in q1 q2 q3 q4 q5 q6 q7 q8 {
	ren `i' `i'_drop14
	ren `i'_seb `i'_seb_drop14
	ren o_`i' o_`i'_drop14
	ren i_`i' i_`i'_drop14
}
ren reliab reliab_drop14
ren reliabfl reliabfl_drop14
ren wp5 WP5
ren iso3 ISO3
save "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\resC_drop14.dta", replace

* Scale: scale_ (b_adj, removed items)
import delimited "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\scale_drop14.csv", encoding(ISO-8859-2) clear
drop check v1
ren wp5 WP5
ren iso3 ISO3
destring *, replace ignore("NA") 
foreach i in q1 q2 q3 q4 q5 q6 q7 q8 {
	ren `i' `i'_drop14
}
save "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\scale_drop14.dta", replace

* Equated Scale (b_global)
import delimited "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\equated_drop14.csv", clear
drop v1
ren wp5 WP5
ren iso3 ISO3
destring *, replace ignore("NA") 
forval i = 1/8 {
	ren t`i' e`i'_drop14
}
ren corr_comm corr_comm_drop14
save "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\equated_drop14.dta", replace

* Global Scale (as a row vector, corr_coeff)
import delimited "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\global_scale_drop14.csv", encoding(UTF-8) clear
drop v1
destring *, replace ignore("NA") 
forval i = 1/8 {
	ren s`i' gs`i'_drop14
}
save "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\global_scale_drop14.dta", replace

clear


** DROP 19 **
* Basic Results: resC_ (b, se, infit, outfit)
import delimited "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\resC_drop19.csv"
destring *, replace ignore("NA") 
drop v1
foreach i in q1 q2 q3 q4 q5 q6 q7 q8 {
	ren `i' `i'_drop19
	ren `i'_seb `i'_seb_drop19
	ren o_`i' o_`i'_drop19
	ren i_`i' i_`i'_drop19
}
ren reliab reliab_drop19
ren reliabfl reliabfl_drop19
ren wp5 WP5
ren iso3 ISO3
save "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\resC_drop19.dta", replace

* Scale: scale_ (b_adj, removed items)
import delimited "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\scale_drop19.csv", encoding(ISO-8859-2) clear
drop check v1
ren wp5 WP5
ren iso3 ISO3
destring *, replace ignore("NA") 
foreach i in q1 q2 q3 q4 q5 q6 q7 q8 {
	ren `i' `i'_drop19
}
save "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\scale_drop19.dta", replace

* Equated Scale (b_global)
import delimited "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\equated_drop19.csv", clear
drop v1
ren wp5 WP5
ren iso3 ISO3
destring *, replace ignore("NA") 
forval i = 1/8 {
	ren t`i' e`i'_drop19
}
ren corr_comm corr_comm_drop19
save "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\equated_drop19.dta", replace

* Global Scale (as a row vector, corr_coeff)
import delimited "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\global_scale_drop19.csv", encoding(UTF-8) clear
drop v1
destring *, replace ignore("NA") 
forval i = 1/8 {
	ren s`i' gs`i'_drop19
}
save "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\global_scale_drop19.dta", replace

clear



