##############################################
## Adding and Accessing ISO-3 Country Codes ##
##############################################
# Source: https://wits.worldbank.org/wits/wits/witshelp/content/codes/country_codes.htm

## Add relevant packages
library(dplyr)

## Upload CSVs
setwd("C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code")
FULL = read.csv("testfile.csv")
ISO3 = read.csv("ISO3.csv")

## Edits to ISO3 before merging
colnames(ISO3) <- c('WP5','ISO3','X')
ISO3 = subset(ISO3, select = -c(X))
ISO3 <- ISO3 %>%  na.omit()
ISO3 <- subset(ISO3, nchar(as.character(ISO3)) <= 3)
ISO3 <- ISO3[!(ISO3$ISO3 == ""), ]
rownames(ISO3) <- 1:nrow(ISO3)

## Creation of country vector (as in CSV_Loop.R)
table(FULL$WP5)
c <- table(FULL$WP5)
c = data.frame(table(FULL$WP5))
c1 <- c[order(c$Var1),]
colnames(c1) <- c('WP5','X')

## Merge values and edits to merged file
c1_ISO3 <- merge(c1,ISO3,by="WP5", all=TRUE)
# Fixing countries that are in the WB ISO3 file
c1_ISO3$ISO3[c1_ISO3$WP5 == "Congo (Kinshasa)"] <- "ZAR"
c1_ISO3$ISO3[c1_ISO3$WP5 == "Congo Brazzaville"] <- "COG"
c1_ISO3$ISO3[c1_ISO3$WP5 == "Egypt"] <- "EGY"
c1_ISO3$ISO3[c1_ISO3$WP5 == "Eritrea"] <- "ERI"
c1_ISO3$ISO3[c1_ISO3$WP5 == "Eswatini"] <- "SWZ"
c1_ISO3$ISO3[c1_ISO3$WP5 == "Ethiopia"] <- "ETH"
c1_ISO3$ISO3[c1_ISO3$WP5 == "Hong Kong"] <- "HKG"
c1_ISO3$ISO3[c1_ISO3$WP5 == "Iran"] <- "IRN"
c1_ISO3$ISO3[c1_ISO3$WP5 == "Ivory Coast"] <- "CIV"
c1_ISO3$ISO3[c1_ISO3$WP5 == "Kyrgyzstan"] <- "KGZ"
c1_ISO3$ISO3[c1_ISO3$WP5 == "Laos"] <- "LAO"
c1_ISO3$ISO3[c1_ISO3$WP5 == "North Macedonia"] <- "MKD"
c1_ISO3$ISO3[c1_ISO3$WP5 == "Russia"] <- "RUS"
c1_ISO3$ISO3[c1_ISO3$WP5 == "Slovakia"] <- "SVK"
c1_ISO3$ISO3[c1_ISO3$WP5 == "South Korea"] <- "KOR"
c1_ISO3$ISO3[c1_ISO3$WP5 == "Syria"] <- "SYR"
c1_ISO3$ISO3[c1_ISO3$WP5 == "The Gambia"] <- "GAM"
c1_ISO3$ISO3[c1_ISO3$WP5 == "Yemen"] <- "YEM"
# Fixing countries not in WB ISO3 file
c1_ISO3$ISO3[c1_ISO3$WP5 == "Kosovo"] <- "XXK"
c1_ISO3$ISO3[c1_ISO3$WP5 == "Montenegro"] <- "MNE"
c1_ISO3$ISO3[c1_ISO3$WP5 == "Palestinian Territories"] <- "PSE"
c1_ISO3$ISO3[c1_ISO3$WP5 == "Serbia"] <- "SRB"
c1_ISO3$ISO3[c1_ISO3$WP5 == "South Sudan"] <- "SSD"
# Creating ISO3 Codes for territories not recognized by ISO3
c1_ISO3$ISO3[c1_ISO3$WP5 == "Northern Cyprus"] <- "TCY"


## Dropping ISO3 codes for countries not in the data
c1_ISO3 <- subset(c1_ISO3, c1_ISO3$X>0)
subset(c1_ISO3, select = -c(X))




