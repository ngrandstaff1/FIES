############################################
## Loop over Country-Years for RM.weights ##
############################################
## Clear everything
rm(list=ls())

## STEPS: 
# 1. Load relevant packages
# 2. Load relevant data
# 3. Create ISO3 abbreviation merge file 
# 4. Create keys for the subsets: (C) 
# 5. Create the subsets 
# 6. Create the data frame for parameter storage
# 7. Estimate Rasch model and store in the data frame
# 8. Correcting for missing questions

## STEP 1: Preliminary packages
library(RM.weights)
library(haven)      
library(expss)
library(ggplot2)
library(dplyr)


## STEP 2: Uploading data
setwd("C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code")
FULL = read.csv("testfile.csv")
ISO3 = read.csv("ISO3.csv")


## STEP 3: ISO3 Merge file anc clean it
#Edits to ISO3 before merging
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


## STEP 4: Create subset keys for to (C) and (C-Y) sets
# Individual Country and Year Vector Keys: 
table(FULL$WP5)
c <- table(FULL$WP5)
c = data.frame(table(FULL$WP5))
table(FULL$YEAR_WAVE)
y <- table(FULL$YEAR_WAVE)
y = data.frame(table(FULL$YEAR_WAVE))
# Combined Country-Year (C-Y) Key:
cy <- subset(FULL, select = c(YEAR_WAVE, WP5))
duplicated(cy)
cy[duplicated(cy),]
cy <- cy[!duplicated(cy),]
cy <- subset(cy, select = c(YEAR_WAVE, WP5))
rownames(cy) <- 1:nrow(cy)


## STEP 5: Creating the subsets (C)
# (C) Creating looping variable (year-blind)
m_FULL <- merge(FULL,c1_ISO3,by="WP5")
sum(is.na(m_FULL$ISO3))
c1 <- subset(c1_ISO3,select = c(ISO3))
c1 <- c1 %>% mutate(row_number = 1:n()) # add num.C column
# (C) Loop to create data frames
  for (i in c1$ISO3) {
    df <- data.frame(subset(m_FULL, ISO3==i, select=c(WORRIED:WHLDAY, wt)))
    assign(i,df )
  }
rm(i)

  
## STEP 6: Creating and saving the data frame for parameter storage
# (C-FULL) Create data frame in which to store RM results
resC_FULL <- data.frame(matrix(ncol = 34, nrow = nrow(c1_ISO3)))
colnames(resC_FULL) <- c('Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8','q1_se.b','q2_se.b','q3_se.b','q4_se.b','q5_se.b','q6_se.b','q7_se.b','q8_se.b','reliab','reliab.fl','O_q1','O_q2','O_q3','O_q4','O_q5','O_q6','O_q7','O_q8','I_q1','I_q2','I_q3','I_q4','I_q5','I_q6','I_q7','I_q8')
v1 <- data.frame(matrix(ncol=2,nrow=nrow(c)))
v1 <- data.frame(c$Var1,c$Freq)
colnames(v1) <- c('WP5','Freq')
v1 <- merge(c1_ISO3,v1,by="WP5")
resC_FULL <- merge(v1,resC_FULL)
resC_FULL <- resC_FULL[!duplicated(resC_FULL$WP5),]


## STEP 7: (C-FULL) Estimate Rasch model and extract values to data frame
# General loop over ISO3-indexed data sets
for (i in c1$ISO3) {
  tryCatch ( {
    temp_w <- subset(get(i), select = c(wt))
    temp <- subset(get(i), select = c(WORRIED:WHLDAY))
    res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
    
    for (n in 1:8) {
      x <- res$b[[n]]
      assign(paste("b", n, sep=""),x) 
      y <- res$se.b[[n]]
      assign(paste("se.b", n, sep=""),y) 
      z <- res$infit[[n]]
      assign(paste("infit", n, sep=""),z) 
      w <- res$outfit[[n]]
      assign(paste("outfit", n, sep=""),w) 
    }
    reliab <- res$reliab
    reliab.fl <- res$reliab.fl
    resC_FULL$Q1[resC_FULL$ISO3 == i] <- b1
    resC_FULL$Q2[resC_FULL$ISO3 == i] <- b2
    resC_FULL$Q3[resC_FULL$ISO3 == i] <- b3
    resC_FULL$Q4[resC_FULL$ISO3 == i] <- b4
    resC_FULL$Q5[resC_FULL$ISO3 == i] <- b5
    resC_FULL$Q6[resC_FULL$ISO3 == i] <- b6
    resC_FULL$Q7[resC_FULL$ISO3 == i] <- b7
    resC_FULL$Q8[resC_FULL$ISO3 == i] <- b8
    # Standard errors on coefficients
    resC_FULL$q1_se.b[resC_FULL$ISO3 == i] <- se.b1
    resC_FULL$q2_se.b[resC_FULL$ISO3 == i] <- se.b2
    resC_FULL$q3_se.b[resC_FULL$ISO3 == i] <- se.b3
    resC_FULL$q4_se.b[resC_FULL$ISO3 == i] <- se.b4
    resC_FULL$q5_se.b[resC_FULL$ISO3 == i] <- se.b5
    resC_FULL$q6_se.b[resC_FULL$ISO3 == i] <- se.b6
    resC_FULL$q7_se.b[resC_FULL$ISO3 == i] <- se.b7
    resC_FULL$q8_se.b[resC_FULL$ISO3 == i] <- se.b8
    # Reliabilities
    resC_FULL$reliab[resC_FULL$ISO3 == i] <- reliab
    resC_FULL$reliab.fl[resC_FULL$ISO3 == i] <- reliab.fl
    # Infits
    resC_FULL$I_q1[resC_FULL$ISO3 == i] <- infit1
    resC_FULL$I_q2[resC_FULL$ISO3 == i] <- infit2
    resC_FULL$I_q3[resC_FULL$ISO3 == i] <- infit3
    resC_FULL$I_q4[resC_FULL$ISO3 == i] <- infit4
    resC_FULL$I_q5[resC_FULL$ISO3 == i] <- infit5
    resC_FULL$I_q6[resC_FULL$ISO3 == i] <- infit6
    resC_FULL$I_q7[resC_FULL$ISO3 == i] <- infit7
    resC_FULL$I_q8[resC_FULL$ISO3 == i] <- infit8
    # Outfits
    resC_FULL$O_q1[resC_FULL$ISO3 == i] <- outfit1 
    resC_FULL$O_q2[resC_FULL$ISO3 == i] <- outfit2
    resC_FULL$O_q3[resC_FULL$ISO3 == i] <- outfit3
    resC_FULL$O_q4[resC_FULL$ISO3 == i] <- outfit4
    resC_FULL$O_q5[resC_FULL$ISO3 == i] <- outfit5 
    resC_FULL$O_q6[resC_FULL$ISO3 == i] <- outfit6 
    resC_FULL$O_q7[resC_FULL$ISO3 == i] <- outfit7 
    resC_FULL$O_q8[resC_FULL$ISO3 == i] <- outfit8 
  }, error = function(e){})
}
# Cleaning the environment
rm(b1,b2,b3,b4,b5,b6,b7,b8)
rm(infit1,infit2,infit3,infit4,infit5,infit6,infit7,infit8)
rm(outfit1,outfit2,outfit3,outfit4,outfit5,outfit6,outfit7,outfit8)
rm(se.b1,se.b2,se.b3,se.b4,se.b5,se.b6,se.b7,se.b8)
rm(reliab, reliab.fl)
rm(n)
rm(i,w,x,y,z)
rm(res,temp,temp_w)


## STEP 8: (C-FULL) Correcting for missing questions
# Countries skipping a question
resC_fix <- subset(resC_FULL, is.na(resC_FULL$Q1))
cou_fix <- data.frame(resC_fix$ISO3)
# Set up a place to store results
resC_fix <- data.frame(subset(resC_fix, select=c(WP5,ISO3,Q1:Q8)))
# LOOP over non-response countries
for (i in cou_fix$resC_fix.ISO3) {
  t <- length(i)
  print(t)
  temp <- subset(get(i), select = c(WORRIED:WHLDAY))
  S <- colSums(is.na(temp))
  q1 <- if (S[[1]] == t) 1 else 0
  q2 <- if (S[[2]] == t) 1 else 0
  q3 <- if (S[[3]] == t) 1 else 0
  q4 <- if (S[[4]] == t) 1 else 0
  q5 <- if (S[[5]] == t) 1 else 0
  q6 <- if (S[[6]] == t) 1 else 0
  q7 <- if (S[[7]] == t) 1 else 0
  q8 <- if (S[[8]] == t) 1 else 0
  resC_fix$Q1[resC_fix$ISO3 == i] <- q1
  resC_fix$Q2[resC_fix$ISO3 == i] <- q2
  resC_fix$Q3[resC_fix$ISO3 == i] <- q3
  resC_fix$Q4[resC_fix$ISO3 == i] <- q4
  resC_fix$Q5[resC_fix$ISO3 == i] <- q5
  resC_fix$Q6[resC_fix$ISO3 == i] <- q6
  resC_fix$Q7[resC_fix$ISO3 == i] <- q7
  resC_fix$Q8[resC_fix$ISO3 == i] <- q8
}
# Check again
for (i in cou_fix$resC_fix.ISO3) {
  View(get(i))
}
View(resC_fix)
# Adjust scales with missing questions



























