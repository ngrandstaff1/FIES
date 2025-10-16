***********************
** OBTAINING SUBSETS **
***********************
** DATA
clear
use "C:\Users\ngran\Box\JHodd - FIES\R Code\testfile.dta"

** COVER6
gen m1 = missing(WORRIED)
gen m2 = missing(HEALTHY)
gen m3 = missing(FEWFOOD)
gen m4 = missing(SKIPPED)
gen m5 = missing(ATELESS)
gen m6 = missing(RUNOUT)
gen m7 = missing(HUNGRY)
gen m8 = missing(WHLDAY)


gen m = (m1+m2+m3+m4+m5+m6+m7+m8)/8
bysort WP5 YEAR_WAVE: egen missing = mean(m)


drop m1-m8 m WORRIED-WHLDAY 
drop WPID_RANDOM wt RS RS_par 
* Dropping stuff
sort YEAR_WAVE WP5
quietly by YEAR_WAVE WP5: gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup
reshape wide missing, i(WP5) j(YEAR_WAVE)
* Drop condition
forvalues i = 2014/2019 {
	replace missing`i'=1 if missing`i'==.


	gen m`i' = 0
	replace m`i' = 1 if missing`i' == 1
}


gen cover6 = 0
replace cover6 = 1 if m2014<1 & m2015<1 & m2016<1 & m2017<1 & m2018<1 & m2019<1
tab cover6












tab WP5 if m>0.8 & YEAR_WAVE








sum m if WP5 ==1 & YEAR_WAVE==2014
sum count if WP5 ==1 & YEAR_WAVE==2014







