/////////////////////////////////////////////////////////////////
/////////////             FIES DO-FILE 2            /////////////
/////////////////////////////////////////////////////////////////

** Use descriptive statistics to motivate dropping years 2004-2013, 2019
clear all
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis.dta", replace

** ANALYZING THE NON-MATCH RESPONSES 
* for "Using Only" - 8,867 unmatched from the FIES Data
tab countrynew if _merge==1
tab YEAR_WAVE if countrynew=="Armenia"
tab _merge if countrynew=="Armenia" & YEAR_WAVE==2019

tab YEAR_WAVE if countrynew=="Iraq"
tab _merge if countrynew=="Iraq" & YEAR_WAVE==2019

tab YEAR_WAVE if countrynew=="Israel"
tab _merge if countrynew=="Israel" & YEAR_WAVE==2019

tab YEAR_WAVE if countrynew=="Jamaica"
tab _merge if countrynew=="Jamaica" & YEAR_WAVE==2019

tab YEAR_WAVE if countrynew=="Pakistan"
tab _merge if countrynew=="Pakistan" & YEAR_WAVE==2019

tab YEAR_WAVE if countrynew=="Russia"
tab _merge if countrynew=="Russia" & YEAR_WAVE==2019

tab YEAR_WAVE if countrynew=="United Kingdom"
tab _merge if countrynew=="United Kingdom" & YEAR_WAVE==2018

* for "Master Only" - 1,336,655 from the Gallup Data
tab countrynew if _merge==2
tab countrynew if _merge==2 & 2014<YEAR_WAVE<2019

* for years in FIES dataset
tab YEAR_WAVE if _merge ==3

* big drop of unmatched data for pre-2019
drop if _merge != 3
drop _merge

save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis0.dta", replace

********************************
**  World Bank - Income Data  **
********************************
merge m:1 countrynew YEAR_WAVE using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\WB_Income.dta"

//Note: Data issue on <tab countrynew if wpid == .> s.t. htere are only ID's and FIES scores for 143 individuals

drop _merge
drop INDEX*
save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis1.dta", replace

