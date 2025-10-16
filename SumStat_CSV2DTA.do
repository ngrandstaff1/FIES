********************************
** CSV2DTA Summary Statistics **
********************************

// Merge All Files Together //
* FULL SET *
use "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\resC_FULL.dta"
merge 1:1 WP5 ISO3 using "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\scale_FULL.dta"
drop _merge
merge 1:1 WP5 ISO3 using "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\equated_FULL.dta"
drop _merge
drop s1-t8
save "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\FULL_sum.dta", replace
clear

* COVER 6 *
use "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\resC_cover6.dta"
merge 1:1 WP5 ISO3 using "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\scale_cover6.dta"
drop _merge
merge 1:1 WP5 ISO3 using "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\equated_cover6.dta"
drop _merge
drop s1-t8
save "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\COVER6_sum.dta", replace
clear

* DROP 14 *
use "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\resC_drop14.dta", clear
merge 1:1 WP5 ISO3 using "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\scale_drop14.dta"
drop _merge
merge 1:1 WP5 ISO3 using "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\equated_drop14.dta"
drop _merge
drop s1-t8
save "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\DROP14_sum.dta", replace
clear

* DROP 19 *
use "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\resC_drop19.dta", clear
merge 1:1 WP5 ISO3 using "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\scale_drop19.dta"
drop _merge
merge 1:1 WP5 ISO3 using "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\equated_drop19.dta"
drop _merge
drop s1-t8
save "C:\Users\ngran\Box\JHodd - FIES\R Code\R Output\DROP19_sum.dta", replace
clear


* Infit

* Outfits

* Reliability

* Flat Reliability


















