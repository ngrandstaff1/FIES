/////////////////////////////////////////////////////////////////
/////////////             FIES Do-File 1            /////////////
/////////////////////////////////////////////////////////////////

clear all
set maxvar 10000

********************************
** STREAMLINE THE GALLUP DATA **
********************************
** Stata Preparation **
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\The_Gallup_022820\The_Gallup_022820.dta"

** Drop by Year
drop if YEAR_WAVE < 2014
drop if YEAR_WAVE > 2019

** Regional Dummies
drop REGION2* REGION3* REGION4

** Cutting groups of variables; with justification
drop WP10004-WP10120 //Peace, migration, health survey questions
drop WP10122-WP10206 //Employment, child marriage survey questions
drop wp10232-WP10261 //Quality of life in city survey questions
drop WP10265-WP10947 //Institutions, social norms, beliefs survey questions
drop WP10998-WP11280 //Religion, social network, telecom survey questions
drop wp1129-WP11524 //Social norms, religion, location of work survey questions
drop WP11539-WP11619 //Natural disasters, business conditions survey questions
drop WP11621-WP117 WP118-WP12105 //Views of Iran, Muslims, job search survey questions
drop WP122 WP12200-WP12211 WP12219-WP12225 WP12230-WP12241 WP12256-wp1226 WP1227 WP1228-WP12292 //Assorted survey questions
drop WP12297 WP12298 WP12307-WP12309 WP12310-WP12338 WP1233Recoded-WP12413 //Assorted survey questions
drop WP12451-WP125 //Political and protest survey questions
drop WP126-wp12718 //Political conditions in-country and health survey questions
drop wp12720-wp12722 //Political conditions survey questions
drop wp12727-WP1391 //Politics and safety survey questions
drop WP140 WP14012-WP14445 //Int'l politics survey questions
drop WP14450-wp1450 WP1452-WP14678 //Institutions, infrastructure survey questions
drop WP14689-WP147 //Country-specific survey questions
drop WP14702-WP14735 //Country-specific survey questions
drop WP1477- WP159 //Health and well-being, int'l issues survey questions
drop WP16-WP16289 WP167-WP3112 //Int'l issues survey questions
drop WP3119-WP3674 //Household characteristics survey questions
drop WP37-WP3999 WP4000-WP4371 WP4400-WP4657 WP4688-WP5245 //Household characteristics survey questions
drop wp53-WP64 wp6414-WP6599 WP662-WP6880 //Views of country, large firms, government, health survey questions
drop WP6888-wp702 WP7021-WP755 WP7577-WP7748 WP7787-WP813 WP814 WP8151-WP82 WP821-wp864 WP87-WP9080 //Assorted survey questions
drop WP9086-WP9199 WP9200-WP98 WP9830-WP9983
save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES Subset 1.dta", replace


*************************************
** MERGE IN FIES COUNTRY QUESTIONS **
*************************************
** Import Excel Country List and Save at DTA
clear
import excel "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Referenced Files\FIES_Country_List.xlsx", sheet("Sheet1") firstrow
save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Referenced Files\FIES_Country_List.dta", replace
clear

** Format FIES Country files in a Loop
clear
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Referenced Files\FIES_Country_List.dta", clear
levelsof country, local(levels) 
foreach l of local levels {
    clear
	import delimited "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES Microdata2020_CSV\FIES_data_`l'.csv"
	ren random_id WPID_RANDOM
	ren year YEAR_WAVE
	gen countrynew = "`l'"
	tostring n_adults, replace
	tostring n_child, replace
	tostring rawscore, replace
	save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES Microdata2020_DTA\FIES_data_`l'.dta", replace
}

** Combine all the FIES Country files in a Loop
clear
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\Referenced Files\FIES_Country_List.dta", clear
levelsof country, local(levels) 
clear
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES Microdata2020_DTA\FIES_data_Afghanistan.dta"
local N = _N
drop in 1/`N'
save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data.dta", replace

foreach l of local levels {		
	append using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES Microdata2020_DTA\FIES_data_`l'.dta", force
}
save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data.dta", replace
clear

** Merge FIES with Gallup
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data.dta", replace
merge 1:1 WPID_RANDOM using "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES Subset 1.dta"
sort _merge
tab _merge
save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data.dta", replace

* Save seperate 
save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis.dta", replace

