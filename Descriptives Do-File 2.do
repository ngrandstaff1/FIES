*******************************
***       DESCRIPTIVES      ***
*******************************
** DATA **
clear
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis3.dta", replace

****************************************
******     INCOME  STATISTICS     ******  by Region
****************************************
// Check for bimodality
hist income_2 if Region =="East Asia & Pacific" & income_2<500000
hist income_2 if Region =="Europe & Central Asia" & income_2<500000
hist income_2 if Region =="Latin America & Caribbean" & income_2 < 500000
hist income_2 if Region =="Middle East & North Africa" & income_2<500000
hist income_2 if Region =="North America" & income_2<500000
hist income_2 if Region =="South Asia" & income_2<500000
hist income_2 if Region =="Sub-Saharan Africa" & income_2<500000

// Mean income by region and in-country HH income quintile
forvalues i = 1/5 {
sum income_2 if Region =="East Asia & Pacific" & YEAR_CAL==2018 & INCOME_5 == `i' 
}
forvalues i = 1/5 {
sum income_2 if Region =="Europe & Central Asia" & YEAR_CAL==2018 & INCOME_5 == `i'
}
forvalues i = 1/5 {
sum income_2 if Region =="Latin America & Caribbean" & YEAR_CAL==2018 & INCOME_5 == `i'
}
forvalues i = 1/5 {
sum income_2 if Region =="Middle East & North Africa" & YEAR_CAL==2018 & INCOME_5 == `i'
}
forvalues i = 1/5 {
sum income_2 if Region =="North America" & YEAR_CAL==2018 & INCOME_5 == `i'
}
forvalues i = 1/5 {
sum income_2 if Region =="South Asia" & YEAR_CAL==2018 & INCOME_5 == `i'
}
forvalues i = 1/5 {
sum income_2 if Region =="Sub-Saharan Africa" & YEAR_CAL==2018 & INCOME_5 == `i'
}


****************************************
******  FOOD SECURITY STATISTICS  ******  by Region (from WB Data)
****************************************
// ANY Food insecurity
forvalues i = 1/5 {
sum RS_b if Region =="East Asia & Pacific" & YEAR_CAL==2018 & INCOME_5 == `i' 
}
forvalues i = 1/5 {
sum RS_b if Region =="Europe & Central Asia" & YEAR_CAL==2018 & INCOME_5 == `i'
}
forvalues i = 1/5 {
sum RS_b if Region =="Latin America & Caribbean" & YEAR_CAL==2018 & INCOME_5 == `i'
}
forvalues i = 1/5 {
sum RS_b if Region =="Middle East & North Africa" & YEAR_CAL==2018 & INCOME_5 == `i'
}
forvalues i = 1/5 {
sum RS_b if Region =="North America" & YEAR_CAL==2018 & INCOME_5 == `i'
}
forvalues i = 1/5 {
sum RS_b if Region =="South Asia" & YEAR_CAL==2018 & INCOME_5 == `i'
}
forvalues i = 1/5 {
sum RS_b if Region =="Sub-Saharan Africa" & YEAR_CAL==2018 & INCOME_5 == `i'
}

// MODERATE OR SEVERE Food insecurity
forvalues i = 1/5 {
sum RS_msb if Region =="East Asia & Pacific" & YEAR_CAL==2018 & INCOME_5 == `i' 
}
forvalues i = 1/5 {
sum RS_msb if Region =="Europe & Central Asia" & YEAR_CAL==2018 & INCOME_5 == `i'
}
forvalues i = 1/5 {
sum RS_msb if Region =="Latin America & Caribbean" & YEAR_CAL==2018 & INCOME_5 == `i'
}
forvalues i = 1/5 {
sum RS_msb if Region =="Middle East & North Africa" & YEAR_CAL==2018 & INCOME_5 == `i'
}
forvalues i = 1/5 {
sum RS_msb if Region =="North America" & YEAR_CAL==2018 & INCOME_5 == `i'
}
forvalues i = 1/5 {
sum RS_msb if Region =="South Asia" & YEAR_CAL==2018 & INCOME_5 == `i'
}
forvalues i = 1/5 {
sum RS_msb if Region =="Sub-Saharan Africa" & YEAR_CAL==2018 & INCOME_5 == `i'
}

// SEVERE Food insecurity
forvalues i = 1/5 {
sum RS_sb if Region =="East Asia & Pacific" & YEAR_CAL==2018 & INCOME_5 == `i' 
}
forvalues i = 1/5 {
sum RS_sb if Region =="Europe & Central Asia" & YEAR_CAL==2018 & INCOME_5 == `i'
}
forvalues i = 1/5 {
sum RS_sb if Region =="Latin America & Caribbean" & YEAR_CAL==2018 & INCOME_5 == `i'
}
forvalues i = 1/5 {
sum RS_sb if Region =="Middle East & North Africa" & YEAR_CAL==2018 & INCOME_5 == `i'
}
forvalues i = 1/5 {
sum RS_sb if Region =="North America" & YEAR_CAL==2018 & INCOME_5 == `i'
}
forvalues i = 1/5 {
sum RS_sb if Region =="South Asia" & YEAR_CAL==2018 & INCOME_5 == `i'
}
forvalues i = 1/5 {
sum RS_sb if Region =="Sub-Saharan Africa" & YEAR_CAL==2018 & INCOME_5 == `i'
}

