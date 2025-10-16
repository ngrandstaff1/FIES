############################################
## Country-Year Scale Stability via Rasch ## 
############################################
## Clear everything
rm(list=ls())

## STEPS: 
# 1.      Load relevant packages
# 2.      Uploading data 
# 3.      ISO3 Merge file and clean it
# 4.      Creating C-Y key for subsets
# 5.      Save dataframe (resC_stabF; resC_stabD)    
# 6.      
# 7.      
# 8.      
# 9.      
# 10.     
# 11.     

## STEP 1:  Preliminary packages
{
  library(RM.weights)
  library(haven)      
  library(expss)
  library(ggplot2)
  library(dplyr)
}

## STEP 2:  Uploading data
{
  setwd("C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code")
  # Set the data
  FULL = read.csv("testfile.csv")
  ISO3 = read.csv("ISO3.csv")
}

## STEP 3:  ISO3 Merge file and clean it
{
  # Edits to ISO3 before merging
  colnames(ISO3) <- c('WP5','ISO3','X')
  ISO3 = subset(ISO3, select = -c(X))
  ISO3 <- ISO3 %>%  na.omit()
  ISO3 <- subset(ISO3, nchar(as.character(ISO3)) <= 3)
  ISO3 <- ISO3[!(ISO3$ISO3 == ""), ]
  rownames(ISO3) <- 1:nrow(ISO3)
  # Creation of country vector
  table(FULL$WP5)
  c <- table(FULL$WP5)
  c = data.frame(table(FULL$WP5))
  c1 <- c[order(c$Var1),]
  colnames(c1) <- c('WP5','X')
  # Merge values and edits to merged file
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
  c1_ISO3 <- subset(c1_ISO3, select = -c(X))
  rm(c, c1)
}

## STEP 4:  Creating C-Y key for subsets
{
  # Combined Country-Year (C-Y) Key:
  cy <- subset(FULL, select = c(YEAR_WAVE, WP5))
  duplicated(cy)
  cy[duplicated(cy),]
  cy <- cy[!duplicated(cy),]
  cy <- subset(cy, select = c(YEAR_WAVE, WP5))
  rownames(cy) <- 1:nrow(cy)
}

## STEP 5:  Creating names for the subsets
{
  CY_key <- merge(c1_ISO3,cy, by="WP5")
  CY_key$CY_key0 <- paste(CY_key$ISO3, CY_key$YEAR_WAVE, sep = "_", collapse = NULL)
}


























