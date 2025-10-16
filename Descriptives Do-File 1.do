*******************************
***       DESCRIPTIVES      ***
*******************************

** DATA **
clear
use "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis2.dta", replace

******************************
******  COUNTRY GROUPS  ******
******************************
** Income Groups
tab countrynew if Incomegroup=="High income"
tab countrynew if Incomegroup=="Upper middle income"
tab countrynew if Incomegroup=="Lower middle income"
tab countrynew if Incomegroup=="Low income"

** Geography Groups
tab countrynew if REG2_GLOBAL == 1 // Europe
tab countrynew if REG2_GLOBAL == 2 // Former Soviet Union
tab countrynew if REG2_GLOBAL == 3 // Asia
tab countrynew if REG2_GLOBAL == 4 // Americas
tab countrynew if REG2_GLOBAL == 5 // Middle East and North Africa
tab countrynew if REG2_GLOBAL == 6 // Sub-Saharan Africa

** OTHER Regions - which is what is used
tab countrynew if Region == "East Asia & Pacific"
tab countrynew if Region == "Europe & Central Asia"
tab countrynew if Region == "Latin America & Caribbean"
tab countrynew if Region == "Middle East & North Africa" 
tab countrynew if Region == "North America"
tab countrynew if Region == "South Asia"
tab countrynew if Region == "Sub-Saharan Africa"

** Abbreviated Geography Groups
gen RegionA = Region // better region labels
replace RegionA = "EAPa" if Region =="East Asia & Pacific"
replace RegionA = "EuCA" if Region =="Europe & Central Asia"
replace RegionA = "LAmC" if Region =="Latin America & Caribbean"
replace RegionA = "MENA" if Region =="Middle East & North Africa"
replace RegionA = "NAm" if Region =="North America"
replace RegionA = "SAs" if Region =="South Asia"
replace RegionA = "SSA" if Region =="Sub-Saharan Africa"

** Regional Meta
tab Region


**************************************
******  DESCRIPTIVE STATISTICS  ******
**************************************

** INDIVIDUAL LEVEL CHARACTERISTICS
* Age
ttest age == WP1220 // the two specifications of age--essentially same
hist age , width(1)
graph bar age, over(REG2_GLOBAL) // Consistent w/ median of each region
sum age if Region =="East Asia & Pacific" // 46.18/18.24
sum age if Region =="Europe & Central Asia" // 47.05/18.28
sum age if Region =="Latin America & Caribbean" // 41.90/18.34
sum age if Region =="Middle East & North Africa" // 37.83/15.10
sum age if Region =="North America" // 53.26/18.65
sum age if Region =="South Asia" // 37.11/15.61
sum age if Region =="Sub-Saharan Africa" // 34.31/15.63

* Gender
gen gender_f = 0
replace gender_f = 1 if gender == "Female"
sum gender_f // 54% female overall n = 848588
sum gender_f if age < 20 // 51% respondents female < 20, n = 77427
sum gender_f if age >= 20 // 54% respondents female >= 20 n = 771161
graph bar gender_f, over(Incomegroup) // Roughly equal across groups
graph bar gender_f, over(REG2_GLOBAL) // Roughly equal across groups
sum gender_f if Region =="East Asia & Pacific" // 0.560
sum gender_f if Region =="Europe & Central Asia" // 0.558
sum gender_f if Region =="Latin America & Caribbean" // 0.585
sum gender_f if Region =="Middle East & North Africa" // 0.463
sum gender_f if Region =="North America" //0.492
sum gender_f if Region =="South Asia" // 0.505
sum gender_f if Region =="Sub-Saharan Africa" //0.509 

* Education - 1=Elementary; 2=Secondary; 3=post-secondary
tab WP3117 // Five groups; two of non-response three of educ levels
graph bar WP3117 if WP3117<4, over(Incomegroup) 
*Low income ~1.4 all else above
graph bar WP3117 if WP3117<4, over(REG2_GLOBAL)  //All with average >1.5
* For >= HS education (Secondary - 3 year TertiarySecondary education and some education beyond secondary education (9-15 years of education)
tab education if Region =="East Asia & Pacific" //42.67
tab education if Region =="Europe & Central Asia" //59.47
tab education if Region =="Latin America & Caribbean" //51.51
tab education if Region =="Middle East & North Africa" //48.28 
tab education if Region =="North America" //54.05
tab education if Region =="South Asia" //34.01
tab education if Region =="Sub-Saharan Africa" //43.42 

* Marriage
gen marrieddompart = 0 
replace marrieddompart = 1 if WP1223 == 2 // married or domestic partner
replace marrieddompart = 1 if WP1223 == 8
gen sepdivwidow = 0 //catch-all for separated, divorced, widowed
replace sepdivwidow = 1 if WP1223 == 3 // separated
replace sepdivwidow = 1 if WP1223 == 4 // divorced 4
replace sepdivwidow= 1 if WP1223 == 5 // widowed 5
sum marrieddom sepdiv if Region =="East Asia & Pacific" //
sum marrieddom sepdiv if Region =="Europe & Central Asia" //
sum marrieddom sepdiv if Region =="Latin America & Caribbean" //
sum marrieddom sepdiv if Region =="Middle East & North Africa" //
sum marrieddom sepdiv if Region =="North America" //
sum marrieddom sepdiv if Region =="South Asia" //
sum marrieddom sepdiv if Region =="Sub-Saharan Africa" //

* Rural/Urban
gen rural = WP7572-1
sum rural // 53% rural
graph bar rural, over(Incomegroup) // more rural in low income
graph bar rural, over(Region) // over half in Asia and SSA 
sum rural if Region =="East Asia & Pacific" // 0.
sum rural if Region =="Europe & Central Asia" // 0.
sum rural if Region =="Latin America & Caribbean" // 0.
sum rural if Region =="Middle East & North Africa" // 0.
sum rural if Region =="South Asia" // 0.
sum rural if Region =="Sub-Saharan Africa" //0.
tab WP14 if Region =="North America"

** HOUSEHOLD CHARACTERISTICS
* Size - total
tab HHsize
hist HHsize
graph bar HHsize, over(Incomegroup) // largest in low income
graph bar HHsize, over(REG2_GLOBAL) // largest in SSA
sum HHsize if Region =="East Asia & Pacific" // 0.
sum HHsize if Region =="Europe & Central Asia" // 0.
sum HHsize if Region =="Latin America & Caribbean" // 0.
sum HHsize if Region =="Middle East & North Africa" // 0.
sum HHsize if Region =="North America" //0.
sum HHsize if Region =="South Asia" // 0.
sum HHsize if Region =="Sub-Saharan Africa" //0.

* Size - children
tab child_n
hist child_n
graph bar child_n, over(Incomegroup) // largest in low income
graph bar child_n, over(REG2_GLOBAL) //SHar largest in SSA

* Composition - share 15 years old or younger
gen child2adult = child_n/(adult_n+child_n)
hist child2adult
graph bar child2adult, over(Incomegroup) // highest in low income
graph bar child2adult, over(REG2_GLOBAL) // highest in SSA
sum child2adult if Region =="East Asia & Pacific" // 0.
sum child2adult if Region =="Europe & Central Asia" // 0.
sum child2adult if Region =="Latin America & Caribbean" // 0.
sum child2adult if Region =="Middle East & North Africa" // 0.
sum child2adult if Region =="North America" //0.
sum child2adult if Region =="South Asia" // 0.
sum child2adult if Region =="Sub-Saharan Africa" //0.

* Income
tab income // quintiles
tab INCOME_1 // HH income in local currency - continuous
sum income_2 // HH income in international dollars - continuous
hist income_2
sum INCOME_3 // per cap income local - continuous
hist INCOME_3
sum INCOME_4 // per cap income international - continuous
hist INCOME_4
tab INCOME_5 // per cap income quintiles - categorical
tab INCOME_7 // Imputation notes - categorical

graph bar income_2, over(Incomegroup) // ave HH income by country cat
graph bar INCOME_4, over(Incomegroup) // ave per cap inc
graph bar income_2, over(REG2_GLOBAL) // ave HH income by region cat
graph bar INCOME_4, over(REG2_GLOBAL) // ave per cap inc

gen RegionA = Region // better region labels
replace RegionA = "EAPa" if Region =="East Asia & Pacific"
replace RegionA = "EuCA" if Region =="Europe & Central Asia"
replace RegionA = "LAmC" if Region =="Latin America & Caribbean"
replace RegionA = "MEAN" if Region =="Middle East & North Africa"
replace RegionA = "NAm" if Region =="North America"
replace RegionA = "SAs" if Region =="South Asia"
replace RegionA = "SSA" if Region =="Sub-Saharan Africa"

gen INCOME_41 = INCOME_4  // Clustered bar chart
replace INCOME_41 = . if INCOME_5 != 1
label variable INCOME_41 "First Quintile"
gen INCOME_42 = INCOME_4 
replace INCOME_42 = . if INCOME_5 != 2
label variable INCOME_42 "Second Quintile"
gen INCOME_43 = INCOME_4 
replace INCOME_43 = . if INCOME_5 != 3
label variable INCOME_43 "Third Quintile"
gen INCOME_44 = INCOME_4 
replace INCOME_44 = . if INCOME_5 != 4
label variable INCOME_44 "Fourth Quintile"
gen INCOME_45 = INCOME_4 
replace INCOME_45 = . if INCOME_5 != 5
label variable INCOME_45 "Fifth Quintile"
graph bar (median) INCOME_41 (median) INCOME_42 (median) INCOME_43 (median) INCOME_44 (median) INCOME_45, over(RegionA) ///
	legend(label(1 "Quint. 1") ///
	label(2 "Quint. 2") ///
	label(3 "Quint. 3") ///
	label(4 "Quint. 4") ///
	label(5 "Quint. 5")) ///
	ytitle("Median Household Income, per capita", margin(medium)) legend(row(1) size(2.1)) ///
	note("Source: Gallup World Poll 2018; 2016 U.S. Dollars PPP; Unweighted by within-region population") // Median Income
graph export "U:\FIES\Visualizations\ClusterBar_Med_Income.png", as(png) name("Graph")
	
graph bar INCOME_41 INCOME_42 INCOME_43 INCOME_44 INCOME_45, over(RegionA) ///
	legend(label(1 "Quint. 1") ///
	label(2 "Quint. 2") ///
	label(3 "Quint. 3") ///
	label(4 "Quint. 4") ///
	label(5 "Quint. 5")) ///
	ytitle("Mean Household Income, per capita", margin(medium)) legend(row(1) size(2.1)) ///
	note("Source: Gallup World Poll 2018; 2016 U.S. Dollars PPP; Unweighted by within-region population") // Mean Income
graph export "U:\FIES\Visualizations\ClusterBar_Mean_Income.png", as(png) name("Graph")


****************************************
******  FOOD SECURITY STATISTICS  ******
****************************************
** Using Raw Scores measure
* Any food insecurity for RS>0 (intensive margin)
tab RS // 45% nonzero
hist RS 
tab RS if RS>0
gen RS_b = 0
replace RS_b = 1 if RS>0
graph bar RS, over(Incomegroup) // highest in low income
graph bar RS, over(REG2_GLOBAL) // highest in SSA

* Presence moderate or severe food insecurity RS>3 
tab RS_msb // 28% nonzero
graph bar RS_msb, over(Incomegroup) // highest in low income
graph bar RS_msb, over(REG2_GLOBAL) // highest in SSA

* Presence severe food insecurity RS>6 
tab RS_sb // 15% nonzero
graph bar RS_sb, over(Incomegroup) // highest in low income by a lot
graph bar RS_sb, over(REG2_GLOBAL) // highest in SSA by a lot

* Probability moderate and severe food insecurity
sum PROB_mod_sev
sum PROB_sev 
graph bar PROB_mod_sev, over(Incomegroup)
graph bar PROB_sev, over(REG2_GLOBAL)

** RS_b Clustered Bar Chart
gen RS_b1 = RS_b
replace RS_b1 = . if INCOME_5 != 1
gen RS_b2 = RS_b
replace RS_b2 = . if INCOME_5 != 2
gen RS_b3 = RS_b
replace RS_b3 = . if INCOME_5 != 3
gen RS_b4 = RS_b
replace RS_b4 = . if INCOME_5 != 4
gen RS_b5 = RS_b
replace RS_b5 = . if INCOME_5 != 5
graph bar RS_b1 RS_b2 RS_b3 RS_b4 RS_b5, over(RegionA) ///
	legend(label(1 "Quint. 1") ///
	label(2 "Quint. 2") ///
	label(3 "Quint. 3") ///
	label(4 "Quint. 4") ///
	label(5 "Quint. 5")) ///
	ytitle("Mean Food Insecurity Binary Severity Thresholds", margin(medium)) ///
	subtitle("Any food insecurity; RS > 0") ///
	legend(row(1) size(2.1)) ///
	note("Source: Gallup World Poll 2018; Unweighted by within-region population") // Mean
graph export "U:\FIES\Visualizations\ClusterBar_RS_b.png", as(png) name("Graph")

** RS_msb Clustered Bar Chart
gen RS_msb1 = RS_msb
replace RS_msb1 = . if INCOME_5 != 1
gen RS_msb2 = RS_msb
replace RS_msb2 = . if INCOME_5 != 2
gen RS_msb3 = RS_msb
replace RS_msb3 = . if INCOME_5 != 3
gen RS_msb4 = RS_msb
replace RS_msb4 = . if INCOME_5 != 4
gen RS_msb5 = RS_msb
replace RS_msb5 = . if INCOME_5 != 5
graph bar RS_msb1 RS_msb2 RS_msb3 RS_msb4 RS_msb5, over(RegionA) ///
	legend(label(1 "Quint. 1") ///
	label(2 "Quint. 2") ///
	label(3 "Quint. 3") ///
	label(4 "Quint. 4") ///
	label(5 "Quint. 5")) ///
	ytitle("Mean Food Insecurity Binary Severity Thresholds", margin(medium)) ///
	subtitle("Moderate & severe food insecurity; RS > 3") ///
	legend(row(1) size(2.1)) ///
	note("Source: Gallup World Poll 2018; Unweighted by within-region population") // Mean
graph export "U:\FIES\Visualizations\ClusterBar_RS_msb.png", as(png) name("Graph")

** RS_sb Clustered Bar Chart
gen RS_sb1 = RS_sb
replace RS_sb1 = . if INCOME_5 != 1
gen RS_sb2 = RS_sb
replace RS_sb2 = . if INCOME_5 != 2
gen RS_sb3 = RS_sb
replace RS_sb3 = . if INCOME_5 != 3
gen RS_sb4 = RS_sb
replace RS_sb4 = . if INCOME_5 != 4
gen RS_sb5 = RS_sb
replace RS_sb5 = . if INCOME_5 != 5
graph bar RS_sb1 RS_sb2 RS_sb3 RS_sb4 RS_sb5, over(RegionA) ///
	legend(label(1 "Quint. 1") ///
	label(2 "Quint. 2") ///
	label(3 "Quint. 3") ///
	label(4 "Quint. 4") ///
	label(5 "Quint. 5")) ///
	ytitle("Mean Food Insecurity Binary Severity Thresholds", margin(medium)) ///
	subtitle("Severe food insecurity; RS > 6") ///
	legend(row(1) size(2.1)) ///
	note("Source: Gallup World Poll 2018; Unweighted by within-region population") // Mean
graph export "U:\FIES\Visualizations\ClusterBar_RS_sb.png", as(png) name("Graph")

* Cleaning
	drop RS_b1-RS_sb5
	drop INCOME_41-INCOME_45

save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\FIES_data_analysis3.dta", replace




