**************************************
** Clearning WB Income Data Do-file **
**************************************

** Income WB File Creation, Generally
clear
import excel "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\WB Income Data\WB Income Data.xlsx", sheet("Sheet1") firstrow
drop E-L R
sort Country
ren Country countrynew
ren M year_1
ren N year_2
ren O year_3
ren P year_4
ren Q year_5
ren S year_6
reshape long year_, i(countrynew) j(YEAR_WAVE)
drop Lendingcategory

replace YEAR_WAVE = 2014 if YEAR_WAVE == 1
replace YEAR_WAVE = 2015 if YEAR_WAVE == 2
replace YEAR_WAVE = 2016 if YEAR_WAVE == 3
replace YEAR_WAVE = 2017 if YEAR_WAVE == 4
replace YEAR_WAVE = 2018 if YEAR_WAVE == 5
replace YEAR_WAVE = 2019 if YEAR_WAVE == 6
ren year_ INCOME_WB

drop if countrynew == "American Samoa"
drop if countrynew == "Andorra"
drop if countrynew == "Antigua and Barbuda"
drop if countrynew == "Aruba"
drop if countrynew == "Bahamas, The"
drop if countrynew == "Barbados"
drop if countrynew == "Bermuda"
drop if countrynew == "British Virgin Islands"
drop if countrynew == "Brunei Darussalam"
drop if countrynew == "Cabo Verde"
drop if countrynew == "Cayman Islands"
drop if countrynew == "Channel Islands"
replace countrynew = "Congo (Kinshasa)" if countrynew == "Congo, Dem. Rep."
replace countrynew = "Congo Brazzaville" if countrynew == "Congo, Rep."
replace countrynew = "Ivory Coast" if countrynew == "Cote d'Ivoire"
drop if countrynew == "Cuba"
drop if countrynew == "Curacao"
drop if countrynew == "Djibouti"
drop if countrynew == "Dominica"
replace countrynew = "Egypt" if countrynew == "Egypt, Arab Rep."
drop if countrynew == "Equatorial Guinea"
drop if countrynew == "Eritrea"
replace countrynew = "Swaziland" if countrynew == "Eswatini"
drop if countrynew == "Faeroe Islands"
drop if countrynew == "Fiji"
drop if countrynew == "French Polynesia"
replace countrynew = "Gambia" if countrynew == "Gambia, The"
drop if countrynew == "Gibraltar"
drop if countrynew == "Greenland"
drop if countrynew == "Grenada"
drop if countrynew == "Guam"
drop if countrynew == "Guinea-Bissau"
drop if countrynew == "Guyana"
replace countrynew = "Hong Kong" if countrynew == "Hong Kong SAR, China"
replace countrynew = "Iran" if countrynew == "Iran, Islamic Rep."
drop if countrynew == "Isle of Man"
drop if countrynew == "Kiribati"
drop if countrynew == "Korea, Dem. Rep."
replace countrynew = "South Korea" if countrynew == "Korea, Rep."
replace countrynew = "Kyrgyzstan" if countrynew == "Kyrgyz Republic"
replace countrynew = "Laos" if countrynew == "Lao PDR"
drop if countrynew == "Liechtenstein"
drop if countrynew == "Macao SAR, China"
drop if countrynew == "Maldives"
replace countrynew = "Macedonia" if countrynew == "North Macedonia"
drop if countrynew == "Marshall Islands"
drop if countrynew == "Micronesia, Fed. Sts."
drop if countrynew == "Monaco"
drop if countrynew == "Nauru"
drop if countrynew == "New Caledonia"
drop if countrynew == "Northern Mariana Islands"
drop if countrynew == "Oman"
drop if countrynew == "Palau"
drop if countrynew == "Papua New Guinea"
drop if countrynew == "Puerto Rico"
drop if countrynew == "Qatar"
replace countrynew = "Russia" if countrynew == "Russian Federation"
drop if countrynew == "Samoa"
drop if countrynew == "San Marino"
drop if countrynew == "Sao Tome and Principe"
drop if countrynew == "Seychelles"
drop if countrynew == "Sint Maarten (Dutch part)"
replace countrynew = "Slovakia" if countrynew == "Slovak Republic"
drop if countrynew == "Solomon Islands"
drop if countrynew == "St. Kitts and Nevis"
drop if countrynew == "St. Lucia"
drop if countrynew == "St. Martin (French part)"
drop if countrynew == "St. Vincent and the Grenadines"
drop if countrynew == "Suriname"
replace countrynew = "Syria" if countrynew == "Syrian Arab Republic"
drop if countrynew == "Timor-Leste"
drop if countrynew == "Tonga"
drop if countrynew == "Trinidad and Tobago"
drop if countrynew == "Turks and Caicos Islands"
drop if countrynew == "Tuvalu"
drop if countrynew == "Vanuatu"
replace countrynew = "Venezuela" if countrynew == "Venezuela, RB"
drop if countrynew == "Virgin Islands (U.S.)"
drop if countrynew == "West Bank and Gaza"
replace countrynew = "Yemen" if countrynew == "Yemen, Rep."

save "\\rschfs1x\userrs\K-Q\nwg6_RS\Desktop\FIES\WB_Income.dta", replace


******************
* Visualizations *
******************


















