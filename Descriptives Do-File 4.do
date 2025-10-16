*******************************
***       DESCRIPTIVES      *** CHECKING SKIPPING RESPONSE RATES ** 
*******************************

** DATA **
clear
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis9.dta", replace

* Description of missing data
mdesc age gender_f educ_b WP1223 rural HHsize_n child2adult

* MISSING COVARIATES
tab WP5 if age ==. //country level skips
tab WP5 if rural ==. // not included in some years
tab WP5 if HHsize_n ==. //random
tab WP5 if HHsize_n ==. & INCOME_5==1
tab WP5 if HHsize_n ==. & INCOME_5==5
tab WP5 if child2adult ==. //random
tab WP5 if child2adult ==. & INCOME_5==1
tab WP5 if child2adult ==. & INCOME_5==5

* MISSING INCOME
mdesc INCOME_4
tab WP5 if INCOME_4 ==.
