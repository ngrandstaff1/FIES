############################################
## Loop over Country-Years for RM.weights ## -- COVER 5 drop 2019
############################################
## Clear everything
rm(list=ls())

## STEPS: 
# 1.      Load relevant packages
# 2.      Uploading data and Dropping Turkmenistan
# 3.      Create ISO3 abbreviation merge file 
# 4.      Create keys for the subsets: (C) 
# 5.      Create the subsets 
# 6.      Create the data frame for parameter storage
# 7.      Estimate Rasch model and store in the data frame
# 8.      Correcting for missing questions, Hard-coding missing country items
# 9.      Create table of standardized items across contexts    (Baseline step)
# 10.     "WHILE" so updates create no unique items             (Iterative step)
# 11.     Print the scale
# 12.     Add the equated scales to a data frame with associated cut-offs
# FINAL:  Output a table of translated values

## STEP 1:  Preliminary packages
{
  library(RM.weights)
  library(haven)      
  library(expss)
  library(ggplot2)
  library(dplyr)
}

## STEP 2:  Uploading data and Dropping Turkmenistan
{
  setwd("C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code")
  # Set the data
  FULL = read.csv("cover5drop19.csv")
  ISO3 = read.csv("ISO3.csv")
  # Drop Turkmenistan
  FULL = FULL[FULL$WP5 != "Turkmenistan", ]
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
}

## STEP 4:  Create subset keys for to (C) and (C-Y) sets
{
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
}

## STEP 5:  Creating the subsets (C)
{
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
}

## STEP 6:  Creating and saving the data frame for parameter storage
{
  # (C-drop19) Create data frame in which to store RM results
  resC_drop19 <- data.frame(matrix(ncol = 34, nrow = nrow(c1_ISO3)))
  colnames(resC_drop19) <- c('Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8','q1_se.b','q2_se.b','q3_se.b','q4_se.b','q5_se.b','q6_se.b','q7_se.b','q8_se.b','reliab','reliab.fl','O_q1','O_q2','O_q3','O_q4','O_q5','O_q6','O_q7','O_q8','I_q1','I_q2','I_q3','I_q4','I_q5','I_q6','I_q7','I_q8')
  v1 <- data.frame(matrix(ncol=2,nrow=nrow(c)))
  v1 <- data.frame(c$Var1,c$Freq)
  colnames(v1) <- c('WP5','Freq')
  v1 <- merge(c1_ISO3,v1,by="WP5")
  resC_drop19 <- merge(v1,resC_drop19)
  resC_drop19 <- resC_drop19[!duplicated(resC_drop19$WP5),]
}

## STEP 7:  (C-drop19) Estimate Rasch model and extract values to data frame
{
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
      resC_drop19$Q1[resC_drop19$ISO3 == i] <- b1
      resC_drop19$Q2[resC_drop19$ISO3 == i] <- b2
      resC_drop19$Q3[resC_drop19$ISO3 == i] <- b3
      resC_drop19$Q4[resC_drop19$ISO3 == i] <- b4
      resC_drop19$Q5[resC_drop19$ISO3 == i] <- b5
      resC_drop19$Q6[resC_drop19$ISO3 == i] <- b6
      resC_drop19$Q7[resC_drop19$ISO3 == i] <- b7
      resC_drop19$Q8[resC_drop19$ISO3 == i] <- b8
      # Standard errors on coefficients
      resC_drop19$q1_se.b[resC_drop19$ISO3 == i] <- se.b1
      resC_drop19$q2_se.b[resC_drop19$ISO3 == i] <- se.b2
      resC_drop19$q3_se.b[resC_drop19$ISO3 == i] <- se.b3
      resC_drop19$q4_se.b[resC_drop19$ISO3 == i] <- se.b4
      resC_drop19$q5_se.b[resC_drop19$ISO3 == i] <- se.b5
      resC_drop19$q6_se.b[resC_drop19$ISO3 == i] <- se.b6
      resC_drop19$q7_se.b[resC_drop19$ISO3 == i] <- se.b7
      resC_drop19$q8_se.b[resC_drop19$ISO3 == i] <- se.b8
      # Reliabilities
      resC_drop19$reliab[resC_drop19$ISO3 == i] <- reliab
      resC_drop19$reliab.fl[resC_drop19$ISO3 == i] <- reliab.fl
      # Infits
      resC_drop19$I_q1[resC_drop19$ISO3 == i] <- infit1
      resC_drop19$I_q2[resC_drop19$ISO3 == i] <- infit2
      resC_drop19$I_q3[resC_drop19$ISO3 == i] <- infit3
      resC_drop19$I_q4[resC_drop19$ISO3 == i] <- infit4
      resC_drop19$I_q5[resC_drop19$ISO3 == i] <- infit5
      resC_drop19$I_q6[resC_drop19$ISO3 == i] <- infit6
      resC_drop19$I_q7[resC_drop19$ISO3 == i] <- infit7
      resC_drop19$I_q8[resC_drop19$ISO3 == i] <- infit8
      # Outfits
      resC_drop19$O_q1[resC_drop19$ISO3 == i] <- outfit1 
      resC_drop19$O_q2[resC_drop19$ISO3 == i] <- outfit2
      resC_drop19$O_q3[resC_drop19$ISO3 == i] <- outfit3
      resC_drop19$O_q4[resC_drop19$ISO3 == i] <- outfit4
      resC_drop19$O_q5[resC_drop19$ISO3 == i] <- outfit5 
      resC_drop19$O_q6[resC_drop19$ISO3 == i] <- outfit6 
      resC_drop19$O_q7[resC_drop19$ISO3 == i] <- outfit7 
      resC_drop19$O_q8[resC_drop19$ISO3 == i] <- outfit8 
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
}


## STEP 8:  (C-drop19) Correcting for missing questions
{
  # Countries skipping a question
  resC_fix <- subset(resC_drop19, is.na(resC_drop19$Q1))
  cou_fix <- data.frame(resC_fix$ISO3)
  View(cou_fix)
  # Hard-code fixes for countries missing data
  # "DNK" - missing 1,7
  {
    temp_w <- subset(DNK, select = c(wt))
    temp <- subset(DNK, select = c(HEALTHY,FEWFOOD,SKIPPED,ATELESS,RUNOUT,WHLDAY))
    res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
    for (n in 1:8) {
      tryCatch ( {
        x <- res$b[[n]]
        assign(paste("b", n, sep=""),x) 
        y <- res$se.b[[n]]
        assign(paste("se.b", n, sep=""),y) 
        z <- res$infit[[n]]
        assign(paste("infit", n, sep=""),z) 
        w <- res$outfit[[n]]
        assign(paste("outfit", n, sep=""),w) 
      }, error = function(e){})
    }
    reliab <- res$reliab
    reliab.fl <- res$reliab.fl
    resC_drop19$Q2[resC_drop19$ISO3 == "DNK"] <- b1
    resC_drop19$Q3[resC_drop19$ISO3 == "DNK"] <- b2
    resC_drop19$Q4[resC_drop19$ISO3 == "DNK"] <- b3
    resC_drop19$Q5[resC_drop19$ISO3 == "DNK"] <- b4
    resC_drop19$Q6[resC_drop19$ISO3 == "DNK"] <- b5
    resC_drop19$Q8[resC_drop19$ISO3 == "DNK"] <- b6
    #resC_drop19$Q7[resC_drop19$ISO3 == "DNK"] <- b7
    #resC_drop19$Q8[resC_drop19$ISO3 == "DNK"] <- b8
    # Standard errors on coefficients
    resC_drop19$q2_se.b[resC_drop19$ISO3 == "DNK"] <- se.b1
    resC_drop19$q3_se.b[resC_drop19$ISO3 == "DNK"] <- se.b2
    resC_drop19$q4_se.b[resC_drop19$ISO3 == "DNK"] <- se.b3
    resC_drop19$q5_se.b[resC_drop19$ISO3 == "DNK"] <- se.b4
    resC_drop19$q6_se.b[resC_drop19$ISO3 == "DNK"] <- se.b5
    resC_drop19$q8_se.b[resC_drop19$ISO3 == "DNK"] <- se.b6
    #resC_drop19$q7_se.b[resC_drop19$ISO3 == "DNK"] <- se.b7
    #resC_drop19$q8_se.b[resC_drop19$ISO3 == "DNK"] <- se.b8
    # Reliabilities
    resC_drop19$reliab[resC_drop19$ISO3 == "DNK"] <- reliab
    resC_drop19$reliab.fl[resC_drop19$ISO3 == "DNK"] <- reliab.fl
    # Infits
    resC_drop19$I_q2[resC_drop19$ISO3 == "DNK"] <- infit1
    resC_drop19$I_q3[resC_drop19$ISO3 == "DNK"] <- infit2
    resC_drop19$I_q4[resC_drop19$ISO3 == "DNK"] <- infit3
    resC_drop19$I_q5[resC_drop19$ISO3 == "DNK"] <- infit4
    resC_drop19$I_q6[resC_drop19$ISO3 == "DNK"] <- infit5
    resC_drop19$I_q8[resC_drop19$ISO3 == "DNK"] <- infit6
    #resC_drop19$I_q7[resC_drop19$ISO3 == "DNK"] <- infit7
    #resC_drop19$I_q8[resC_drop19$ISO3 == "DNK"] <- infit8
    # Outfits
    resC_drop19$O_q2[resC_drop19$ISO3 == "DNK"] <- outfit1 
    resC_drop19$O_q3[resC_drop19$ISO3 == "DNK"] <- outfit2
    resC_drop19$O_q4[resC_drop19$ISO3 == "DNK"] <- outfit3
    resC_drop19$O_q5[resC_drop19$ISO3 == "DNK"] <- outfit4
    resC_drop19$O_q6[resC_drop19$ISO3 == "DNK"] <- outfit5 
    resC_drop19$O_q8[resC_drop19$ISO3 == "DNK"] <- outfit6 
    #resC_drop19$O_q7[resC_drop19$ISO3 == "DNK"] <- outfit7 
    #resC_drop19$O_q8[resC_drop19$ISO3 == "DNK"] <- outfit8 
  }
  # MLT - missing 1
  {
    temp_w <- subset(MLT, select = c(wt))
    temp <- subset(MLT, select = c(HEALTHY,FEWFOOD,SKIPPED,ATELESS,RUNOUT,HUNGRY,WHLDAY))
    res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
    for (n in 1:8) {
      tryCatch ( {
        x <- res$b[[n]]
        assign(paste("b", n, sep=""),x) 
        y <- res$se.b[[n]]
        assign(paste("se.b", n, sep=""),y) 
        z <- res$infit[[n]]
        assign(paste("infit", n, sep=""),z) 
        w <- res$outfit[[n]]
        assign(paste("outfit", n, sep=""),w) 
      }, error = function(e){})
    }
    reliab <- res$reliab
    reliab.fl <- res$reliab.fl
    resC_drop19$Q2[resC_drop19$ISO3 == "MLT"] <- b1
    resC_drop19$Q3[resC_drop19$ISO3 == "MLT"] <- b2
    resC_drop19$Q4[resC_drop19$ISO3 == "MLT"] <- b3
    resC_drop19$Q5[resC_drop19$ISO3 == "MLT"] <- b4
    resC_drop19$Q6[resC_drop19$ISO3 == "MLT"] <- b5
    resC_drop19$Q7[resC_drop19$ISO3 == "MLT"] <- b6
    resC_drop19$Q8[resC_drop19$ISO3 == "MLT"] <- b7
    #resC_drop19$Q8[resC_drop19$ISO3 == "MLT"] <- b8
    # Standard errors on coefficients
    resC_drop19$q2_se.b[resC_drop19$ISO3 == "MLT"] <- se.b1
    resC_drop19$q3_se.b[resC_drop19$ISO3 == "MLT"] <- se.b2
    resC_drop19$q4_se.b[resC_drop19$ISO3 == "MLT"] <- se.b3
    resC_drop19$q5_se.b[resC_drop19$ISO3 == "MLT"] <- se.b4
    resC_drop19$q6_se.b[resC_drop19$ISO3 == "MLT"] <- se.b5
    resC_drop19$q7_se.b[resC_drop19$ISO3 == "MLT"] <- se.b6
    resC_drop19$q8_se.b[resC_drop19$ISO3 == "MLT"] <- se.b7
    #resC_drop19$q8_se.b[resC_drop19$ISO3 == "MLT"] <- se.b8
    # Reliabilities
    resC_drop19$reliab[resC_drop19$ISO3 == "MLT"] <- reliab
    resC_drop19$reliab.fl[resC_drop19$ISO3 == "MLT"] <- reliab.fl
    # Infits
    resC_drop19$I_q2[resC_drop19$ISO3 == "MLT"] <- infit1
    resC_drop19$I_q3[resC_drop19$ISO3 == "MLT"] <- infit2
    resC_drop19$I_q4[resC_drop19$ISO3 == "MLT"] <- infit3
    resC_drop19$I_q5[resC_drop19$ISO3 == "MLT"] <- infit4
    resC_drop19$I_q6[resC_drop19$ISO3 == "MLT"] <- infit5
    resC_drop19$I_q7[resC_drop19$ISO3 == "MLT"] <- infit6
    resC_drop19$I_q8[resC_drop19$ISO3 == "MLT"] <- infit7
    #resC_drop19$I_q8[resC_drop19$ISO3 == "MLT"] <- infit8
    # Outfits
    resC_drop19$O_q2[resC_drop19$ISO3 == "MLT"] <- outfit1 
    resC_drop19$O_q3[resC_drop19$ISO3 == "MLT"] <- outfit2
    resC_drop19$O_q4[resC_drop19$ISO3 == "MLT"] <- outfit3
    resC_drop19$O_q5[resC_drop19$ISO3 == "MLT"] <- outfit4
    resC_drop19$O_q6[resC_drop19$ISO3 == "MLT"] <- outfit5 
    resC_drop19$O_q7[resC_drop19$ISO3 == "MLT"] <- outfit6 
    resC_drop19$O_q8[resC_drop19$ISO3 == "MLT"] <- outfit7 
    #resC_drop19$O_q8[resC_drop19$ISO3 == "MLT"] <- outfit8 
  }
  # JPN - missing 7
  {
    temp_w <- subset(JPN, select = c(wt))
    temp <- subset(JPN, select = c(WORRIED,HEALTHY,FEWFOOD,SKIPPED,ATELESS,RUNOUT,WHLDAY))
    res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
    for (n in 1:8) {
      tryCatch ( {
        x <- res$b[[n]]
        assign(paste("b", n, sep=""),x) 
        y <- res$se.b[[n]]
        assign(paste("se.b", n, sep=""),y) 
        z <- res$infit[[n]]
        assign(paste("infit", n, sep=""),z) 
        w <- res$outfit[[n]]
        assign(paste("outfit", n, sep=""),w) 
      }, error = function(e){})
    }
    reliab <- res$reliab
    reliab.fl <- res$reliab.fl
    resC_drop19$Q1[resC_drop19$ISO3 == "JPN"] <- b1
    resC_drop19$Q2[resC_drop19$ISO3 == "JPN"] <- b2
    resC_drop19$Q3[resC_drop19$ISO3 == "JPN"] <- b3
    resC_drop19$Q4[resC_drop19$ISO3 == "JPN"] <- b4
    resC_drop19$Q5[resC_drop19$ISO3 == "JPN"] <- b5
    resC_drop19$Q6[resC_drop19$ISO3 == "JPN"] <- b6
    resC_drop19$Q8[resC_drop19$ISO3 == "JPN"] <- b7
    #resC_drop19$Q8[resC_drop19$ISO3 == "JPN"] <- b8
    # Standard errors on coefficients
    resC_drop19$q1_se.b[resC_drop19$ISO3 == "JPN"] <- se.b1
    resC_drop19$q2_se.b[resC_drop19$ISO3 == "JPN"] <- se.b2
    resC_drop19$q3_se.b[resC_drop19$ISO3 == "JPN"] <- se.b3
    resC_drop19$q4_se.b[resC_drop19$ISO3 == "JPN"] <- se.b4
    resC_drop19$q5_se.b[resC_drop19$ISO3 == "JPN"] <- se.b5
    resC_drop19$q6_se.b[resC_drop19$ISO3 == "JPN"] <- se.b6
    resC_drop19$q8_se.b[resC_drop19$ISO3 == "JPN"] <- se.b7
    #resC_drop19$q8_se.b[resC_drop19$ISO3 == "JPN"] <- se.b8
    # Reliabilities
    resC_drop19$reliab[resC_drop19$ISO3 == "JPN"] <- reliab
    resC_drop19$reliab.fl[resC_drop19$ISO3 == "JPN"] <- reliab.fl
    # Infits
    resC_drop19$I_q1[resC_drop19$ISO3 == "JPN"] <- infit1
    resC_drop19$I_q2[resC_drop19$ISO3 == "JPN"] <- infit2
    resC_drop19$I_q3[resC_drop19$ISO3 == "JPN"] <- infit3
    resC_drop19$I_q4[resC_drop19$ISO3 == "JPN"] <- infit4
    resC_drop19$I_q5[resC_drop19$ISO3 == "JPN"] <- infit5
    resC_drop19$I_q6[resC_drop19$ISO3 == "JPN"] <- infit6
    resC_drop19$I_q8[resC_drop19$ISO3 == "JPN"] <- infit7
    #resC_drop19$I_q8[resC_drop19$ISO3 == "JPN"] <- infit8
    # Outfits
    resC_drop19$O_q1[resC_drop19$ISO3 == "JPN"] <- outfit1 
    resC_drop19$O_q2[resC_drop19$ISO3 == "JPN"] <- outfit2
    resC_drop19$O_q3[resC_drop19$ISO3 == "JPN"] <- outfit3
    resC_drop19$O_q4[resC_drop19$ISO3 == "JPN"] <- outfit4
    resC_drop19$O_q5[resC_drop19$ISO3 == "JPN"] <- outfit5 
    resC_drop19$O_q6[resC_drop19$ISO3 == "JPN"] <- outfit6 
    resC_drop19$O_q8[resC_drop19$ISO3 == "JPN"] <- outfit7 
    #resC_drop19$O_q8[resC_drop19$ISO3 == "JPN"] <- outfit8 
  }
  # NLD - missing 4,5,6
  {
    temp_w <- subset(NLD, select = c(wt))
    temp <- subset(NLD, select = c(WORRIED,HEALTHY,FEWFOOD,HUNGRY,WHLDAY))
    res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
    for (n in 1:8) {
      tryCatch ( {
        x <- res$b[[n]]
        assign(paste("b", n, sep=""),x) 
        y <- res$se.b[[n]]
        assign(paste("se.b", n, sep=""),y) 
        z <- res$infit[[n]]
        assign(paste("infit", n, sep=""),z) 
        w <- res$outfit[[n]]
        assign(paste("outfit", n, sep=""),w) 
      }, error = function(e){})
    }
    reliab <- res$reliab
    reliab.fl <- res$reliab.fl
    resC_drop19$Q1[resC_drop19$ISO3 == "NLD"] <- b1
    resC_drop19$Q2[resC_drop19$ISO3 == "NLD"] <- b2
    resC_drop19$Q3[resC_drop19$ISO3 == "NLD"] <- b3
    resC_drop19$Q7[resC_drop19$ISO3 == "NLD"] <- b4
    resC_drop19$Q8[resC_drop19$ISO3 == "NLD"] <- b5
    #resC_drop19$Q6[resC_drop19$ISO3 == "NLD"] <- b6
    #resC_drop19$Q8[resC_drop19$ISO3 == "NLD"] <- b7
    #resC_drop19$Q8[resC_drop19$ISO3 == "NLD"] <- b8
    # Standard errors on coefficients
    resC_drop19$q1_se.b[resC_drop19$ISO3 == "NLD"] <- se.b1
    resC_drop19$q2_se.b[resC_drop19$ISO3 == "NLD"] <- se.b2
    resC_drop19$q3_se.b[resC_drop19$ISO3 == "NLD"] <- se.b3
    resC_drop19$q7_se.b[resC_drop19$ISO3 == "NLD"] <- se.b4
    resC_drop19$q8_se.b[resC_drop19$ISO3 == "NLD"] <- se.b5
    #resC_drop19$q6_se.b[resC_drop19$ISO3 == "NLD"] <- se.b6
    #resC_drop19$q8_se.b[resC_drop19$ISO3 == "NLD"] <- se.b7
    #resC_drop19$q8_se.b[resC_drop19$ISO3 == "NLD"] <- se.b8
    # Reliabilities
    resC_drop19$reliab[resC_drop19$ISO3 == "NLD"] <- reliab
    resC_drop19$reliab.fl[resC_drop19$ISO3 == "NLD"] <- reliab.fl
    # Infits
    resC_drop19$I_q1[resC_drop19$ISO3 == "NLD"] <- infit1
    resC_drop19$I_q2[resC_drop19$ISO3 == "NLD"] <- infit2
    resC_drop19$I_q3[resC_drop19$ISO3 == "NLD"] <- infit3
    resC_drop19$I_q7[resC_drop19$ISO3 == "NLD"] <- infit4
    resC_drop19$I_q8[resC_drop19$ISO3 == "NLD"] <- infit5
    #resC_drop19$I_q6[resC_drop19$ISO3 == "NLD"] <- infit6
    #resC_drop19$I_q8[resC_drop19$ISO3 == "NLD"] <- infit7
    #resC_drop19$I_q8[resC_drop19$ISO3 == "NLD"] <- infit8
    # Outfits
    resC_drop19$O_q1[resC_drop19$ISO3 == "NLD"] <- outfit1 
    resC_drop19$O_q2[resC_drop19$ISO3 == "NLD"] <- outfit2
    resC_drop19$O_q3[resC_drop19$ISO3 == "NLD"] <- outfit3
    resC_drop19$O_q7[resC_drop19$ISO3 == "NLD"] <- outfit4
    resC_drop19$O_q8[resC_drop19$ISO3 == "NLD"] <- outfit5 
    #resC_drop19$O_q6[resC_drop19$ISO3 == "NLD"] <- outfit6 
    #resC_drop19$O_q8[resC_drop19$ISO3 == "NLD"] <- outfit7 
    #resC_drop19$O_q8[resC_drop19$ISO3 == "NLD"] <- outfit8 
  }

  # Optional:
  # rm(b1,b2,b3,b4,b5,b6,b7,b8,infit1,infit2,infit3,infit4,infit5,infit6,infit7,infit8,outfit1,outfit2,outfit3,outfit4,outfit5,outfit6,outfit7,outfit8,se.b1,se.b2,se.b3,se.b4,se.b5,se.b6,se.b7,se.b8,reliab, reliab.fl,n,i,w,x,y,z, res,temp,temp_w)
}

## STEP 8_ALT: For outliers (infit)
  # for 1.5 infit
  {
    #infit_t3 <- subset(resC_drop19, resC_drop19$I_q1 < 1.5)
    #infit_t3 <- subset(resC_drop19, resC_drop19$I_q2 < 1.5)
    #infit_t3 <- subset(resC_drop19, resC_drop19$I_q3 < 1.5)
    #infit_t3 <- subset(resC_drop19, resC_drop19$I_q4 < 1.5)
    #infit_t3 <- subset(resC_drop19, resC_drop19$I_q5 < 1.5)
    #infit_t3 <- subset(resC_drop19, resC_drop19$I_q6 < 1.5)
    #infit_t3 <- subset(resC_drop19, resC_drop19$I_q7 < 1.5)
    #infit_t3 <- subset(resC_drop19, resC_drop19$I_q8 < 1.5)
    #resC_drop19 <- infit_t3
  }
  # for 1.4 infit
  {
    #infit_t1 <- subset(resC_drop19, resC_drop19$I_q1 < 1.4)
    #infit_t1 <- subset(resC_drop19, resC_drop19$I_q2 < 1.4)
    #infit_t1 <- subset(resC_drop19, resC_drop19$I_q3 < 1.4)
    #infit_t1 <- subset(resC_drop19, resC_drop19$I_q4 < 1.4)
    #infit_t1 <- subset(resC_drop19, resC_drop19$I_q5 < 1.4)
    #infit_t1 <- subset(resC_drop19, resC_drop19$I_q6 < 1.4)
    #infit_t1 <- subset(resC_drop19, resC_drop19$I_q7 < 1.4)
    #infit_t1 <- subset(resC_drop19, resC_drop19$I_q8 < 1.4)
    #resC_drop19 <- infit_t1
  }
  # for 1.3 infit
  {
    #infit_t2 <- subset(resC_drop19, resC_drop19$I_q1 < 1.3)
    #infit_t2 <- subset(resC_drop19, resC_drop19$I_q2 < 1.3)
    #infit_t2 <- subset(resC_drop19, resC_drop19$I_q3 < 1.3)
    #infit_t2 <- subset(resC_drop19, resC_drop19$I_q4 < 1.3)
    #infit_t2 <- subset(resC_drop19, resC_drop19$I_q5 < 1.3)
    #infit_t2 <- subset(resC_drop19, resC_drop19$I_q6 < 1.3)
    #infit_t2 <- subset(resC_drop19, resC_drop19$I_q7 < 1.3)
    #infit_t2 <- subset(resC_drop19, resC_drop19$I_q8 < 1.3)
    #resC_drop19 <- infit_t2
  }


## STEP 9_ALT: For tolerance levels
    # Run every time
     tol <- 0.4 # Tolerance level where =1 if "common" (higher number is better)
    # tol <- 0.3
    # tol <- 0.5

## STEP 9:  (C-drop19) Creating table of standardized item coefficients (Baseline)
{
  # Creating a subset of only CML coefficients from Rasch
    scale_drop19 <- subset(resC_drop19,select = c('WP5','ISO3','Freq','Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8'))
  # Add standardized columns
    v1 <- data.frame(matrix(ncol = 8, nrow = nrow(resC_drop19)))
    colnames(v1) <- c('S1','S2','S3','S4','S5','S6','S7','S8')
    scale_drop19 <- cbind(scale_drop19, v1)
  # Add tolerated columns
    v1 <- data.frame(matrix(ncol = 8, nrow = nrow(resC_drop19)))
    colnames(v1) <- c('T1','T2','T3','T4','T5','T6','T7','T8')
    scale_drop19 <- cbind(scale_drop19, v1)
  # First round standardize scores by country
    row.names(scale_drop19) <- 1:nrow(scale_drop19)
    for (i in 1:nrow(resC_drop19)) { 
      n  <- (8 - rowSums( is.na( scale_drop19[i,4:11])))     # number of items, by C
      x  <- rowMeans(scale_drop19[i , c(4:11)], na.rm=TRUE)  # CHANGED/FIXED
      v  <- data.frame(matrix(ncol = 1, nrow = 8))
      colnames(v) <- c('D')
      v[1,1] <- (scale_drop19[i,4] - x)
      v[2,1] <- (scale_drop19[i,5] - x)
      v[3,1] <- (scale_drop19[i,6] - x)
      v[4,1] <- (scale_drop19[i,7] - x)
      v[5,1] <- (scale_drop19[i,8] - x)
      v[6,1] <- (scale_drop19[i,9] - x)
      v[7,1] <- (scale_drop19[i,10] - x)
      v[8,1] <- (scale_drop19[i,11] - x)
      v2 <-v^2
      colnames(v2) <- c('D')
      d  <- sum(v2,na.rm=TRUE)  
      dT <- d/(n-1)
      sd <- sqrt(dT)
      e1 <- v[1,1]
      e2 <- v[2,1]
      e3 <- v[3,1]
      e4 <- v[4,1]
      e5 <- v[5,1]
      e6 <- v[6,1]
      e7 <- v[7,1]
      e8 <- v[8,1]
      scale_drop19[i,12] <- e1/sd
      scale_drop19[i,13] <- e2/sd
      scale_drop19[i,14] <- e3/sd
      scale_drop19[i,15] <- e4/sd
      scale_drop19[i,16] <- e5/sd
      scale_drop19[i,17] <- e6/sd
      scale_drop19[i,18] <- e7/sd
      scale_drop19[i,19] <- e8/sd
    }
  # Column medians in a data frame - PROVISIONAL GLOBAL 8-ITEM SCALE
    s <- data.frame(lapply(scale_drop19[,12:19], median, na.rm = TRUE))
  # Generate binaries on common items by specified tolerance levels 
    # tol <- 0.4 # Tolerance level where =1 if "common" (higher number is better)
  # Question 1 Tolerance 
    for (i in 1:nrow(resC_drop19)) {
    tryCatch ( {
      if(abs(scale_drop19[i,12]-s[[1]]) <= tol) {  # =1 if common
        scale_drop19[i,20] = 1
      } else {
        scale_drop19[i,20] = 0
      }
    }, error = function(e){})
  }
  # Question 2 Tolerance 
    for (i in 1:nrow(resC_drop19)) {
    tryCatch ( {
      if(abs(scale_drop19[i,13]-s[[2]]) <= tol) {  # =1 if common
        scale_drop19[i,21] = 1
      } else {
        scale_drop19[i,21] = 0
      }
    }, error = function(e){})
  }
  # Question 3 Tolerance 
    for (i in 1:nrow(resC_drop19)) {
    tryCatch ( {
      if(abs(scale_drop19[i,14]-s[[3]]) <= tol) {  # =1 if common
        scale_drop19[i,22] = 1
      } else {
        scale_drop19[i,22] = 0
      }
    }, error = function(e){})
  }
  # Question 4 Tolerance 
    for (i in 1:nrow(resC_drop19)) {
    tryCatch ( {
      if(abs(scale_drop19[i,15]-s[[4]]) <= tol) {  # =1 if common
        scale_drop19[i,23] = 1
      } else {
        scale_drop19[i,23] = 0
      }
    }, error = function(e){})
  }
  # Question 5 Tolerance 
    for (i in 1:nrow(resC_drop19)) {
    tryCatch ( {
      if(abs(scale_drop19[i,16]-s[[5]]) <= tol) {  # =1 if common
        scale_drop19[i,24] = 1
      } else {
        scale_drop19[i,24] = 0
      }
    }, error = function(e){})
  }
  # Question 6 Tolerance
    for (i in 1:nrow(resC_drop19)) {
    tryCatch ( {
      if(abs(scale_drop19[i,17]-s[[6]]) <= tol) {  # =1 if common
        scale_drop19[i,25] = 1
      } else {
        scale_drop19[i,25] = 0
      }
    }, error = function(e){})
  }
  # Question 7 Tolerance
    for (i in 1:nrow(resC_drop19)) {
    tryCatch ( {
      if(abs(scale_drop19[i,18]-s[[7]]) <= tol) {  # =1 if common
        scale_drop19[i,26] = 1
      } else {
        scale_drop19[i,26] = 0
      }
    }, error = function(e){})
  }
  # Question 8 Tolerance
    for (i in 1:nrow(resC_drop19)) {
    tryCatch ( {
      if(abs(scale_drop19[i,19]-s[[8]]) <= tol) {  # =1 if common
        scale_drop19[i,27] = 1
      } else {
        scale_drop19[i,27] = 0
      }
    }, error = function(e){})
  }

  # Column counting common items per country
    v1 <- data.frame(matrix(ncol = 1, nrow = nrow(resC_drop19)))
    colnames(v1) <- c('check')
    scale_drop19 <- cbind(scale_drop19, v1)
    scale_drop19 <- scale_drop19[!(scale_drop19$ISO3 == "" | is.na(scale_drop19$ISO3)), ]
  # Sum number of unique items per country
    for (i in 1:length(resC_drop19)) {
      scale_drop19$check[i] <- rowSums(scale_drop19[i,20:27],na.rm=TRUE)
    }
  # Tabulate number of unique items per country (as in Tab. 2: Cafiero et al 2018)
    table(scale_drop19$check) # for tabulation of common items for a given tolerance level
    scale_drop19 <- scale_drop19[c(1:28)] # drops superfluous "check" columns
}     
    
## STEP 10: (C-drop19) "WHILE" so updates create no unique items (Iterative step)
{
  # Conditional values to be updated:
  common_n <- sum(scale_drop19$check, na.rm=TRUE) # total number common items
  NA_n     <- sum(is.na(scale_drop19$Q1))+sum(is.na(scale_drop19$Q2))+sum(is.na(scale_drop19$Q3))+sum(is.na(scale_drop19$Q4))+sum(is.na(scale_drop19$Q5))+sum(is.na(scale_drop19$Q6))+sum(is.na(scale_drop19$Q7))+sum(is.na(scale_drop19$Q8)) 
  unique_n <- (8*length(resC_drop19)) - NA_n - common_n  #total number unique items

  # WHILE there is a positive number of unique items in the data
  while (0 < unique_n) {
    {
      # Set unique items to "NA" to for iterative re-standardization
      {
        # Question 1
        for (i in 1:nrow(resC_drop19)) {
          tryCatch ( {
            if(scale_drop19[i,20] == 0) {  
              scale_drop19[i,4] = NA
            } 
          }, error = function(e){})
        }
        # Question 2
        for (i in 1:nrow(resC_drop19)) {
          tryCatch ( {
            if(scale_drop19[i,21] == 0) {  
              scale_drop19[i,5] = NA
            } 
          }, error = function(e){})
        }
        # Question 3
        for (i in 1:nrow(resC_drop19)) {
          tryCatch ( {
            if(scale_drop19[i,22] == 0) {  
              scale_drop19[i,6] = NA
            } 
          }, error = function(e){})
        }
        # Question 4
        for (i in 1:nrow(resC_drop19)) {
          tryCatch ( {
            if(scale_drop19[i,23] == 0) {  
              scale_drop19[i,7] = NA
            } 
          }, error = function(e){})
        }
        # Question 5
        for (i in 1:nrow(resC_drop19)) {
          tryCatch ( {
            if(scale_drop19[i,24] == 0) {  
              scale_drop19[i,8] = NA
            } 
          }, error = function(e){})
        }
        # Question 6
        for (i in 1:nrow(resC_drop19)) {
          tryCatch ( {
            if(scale_drop19[i,25] == 0) {  
              scale_drop19[i,9] = NA
            } 
          }, error = function(e){})
        }
        # Question 7
        for (i in 1:nrow(resC_drop19)) {
          tryCatch ( {
            if(scale_drop19[i,26] == 0) {  
              scale_drop19[i,10] = NA
            } 
          }, error = function(e){})
        }
        # Question 8
        for (i in 1:nrow(resC_drop19)) {
          tryCatch ( {
            if(scale_drop19[i,27] == 0) {  
              scale_drop19[i,11] = NA
            } 
          }, error = function(e){})
        }
      }
      
      
      # Iterative step standardize scores by country
      for (i in 1:nrow(resC_drop19)) {
        n  <- (8 - rowSums(is.na( scale_drop19[i,4:11]))) # sums # of NA's in items
        x  <- rowMeans(scale_drop19[i , c(4:11)], na.rm=TRUE) 
        v <- data.frame(matrix(ncol = 1, nrow = 8)) # creates differences vector, 8x1
        colnames(v) <- c('D') # renames column vector 
        v[1,1] <- (scale_drop19[i,4] - x) # replacing values in the differences vector
        v[2,1] <- (scale_drop19[i,5] - x)
        v[3,1] <- (scale_drop19[i,6] - x)
        v[4,1] <- (scale_drop19[i,7] - x)
        v[5,1] <- (scale_drop19[i,8] - x)
        v[6,1] <- (scale_drop19[i,9] - x)
        v[7,1] <- (scale_drop19[i,10] - x)
        v[8,1] <- (scale_drop19[i,11] - x)  
        v2 <-v^2  
        colnames(v2) <- c('D')
        d  <- sum(v2,na.rm=TRUE)  # Fixed. All good.
        dT <- d/(n-1)
        sd <- sqrt(dT)
        e1 <- v[1,1]
        e2 <- v[2,1]
        e3 <- v[3,1]
        e4 <- v[4,1]
        e5 <- v[5,1]
        e6 <- v[6,1]
        e7 <- v[7,1]
        e8 <- v[8,1]
        scale_drop19[i,12] <- e1/sd
        scale_drop19[i,13] <- e2/sd
        scale_drop19[i,14] <- e3/sd
        scale_drop19[i,15] <- e4/sd
        scale_drop19[i,16] <- e5/sd
        scale_drop19[i,17] <- e6/sd
        scale_drop19[i,18] <- e7/sd
        scale_drop19[i,19] <- e8/sd
      }    
      
      # Create a table of medians
      s <- data.frame(lapply(scale_drop19[,12:19], median, na.rm = TRUE))
      # Tabulate by tolerance
      #tol <- 0.4 # Tolerance level where =1 if "common" (higher number is better)
      # Question 1 Tolerance
      for (i in 1:nrow(resC_drop19)) {
        tryCatch ( {
          if(abs(scale_drop19[i,12]-s[[1]]) <= tol) {  # =1 if common
            scale_drop19[i,20] = 1
          } else {
            scale_drop19[i,20] = 0
          }
        }, error = function(e){})
      }  
      # Question 2 Tolerance
      for (i in 1:nrow(resC_drop19)) {
        tryCatch ( {
          if(abs(scale_drop19[i,13]-s[[2]]) <= tol) {  # =1 if common
            scale_drop19[i,21] = 1
          } else {
            scale_drop19[i,21] = 0
          }
        }, error = function(e){})
      }  
      # Question 3 Tolerance
      for (i in 1:nrow(resC_drop19)) {
        tryCatch ( {
          if(abs(scale_drop19[i,14]-s[[3]]) <= tol) {  # =1 if common
            scale_drop19[i,22] = 1
          } else {
            scale_drop19[i,22] = 0
          }
        }, error = function(e){})
      }  
      # Question 4 Tolerance
      for (i in 1:nrow(resC_drop19)) {
        tryCatch ( {
          if(abs(scale_drop19[i,15]-s[[4]]) <= tol) {  # =1 if common
            scale_drop19[i,23] = 1
          } else {
            scale_drop19[i,23] = 0
          }
        }, error = function(e){})
      }  
      # Question 5 Tolerance
      for (i in 1:nrow(resC_drop19)) {
        tryCatch ( {
          if(abs(scale_drop19[i,16]-s[[5]]) <= tol) {  # =1 if common
            scale_drop19[i,24] = 1
          } else {
            scale_drop19[i,24] = 0
          }
        }, error = function(e){})
      }  
      # Question 6 Tolerance
      for (i in 1:nrow(resC_drop19)) {
        tryCatch ( {
          if(abs(scale_drop19[i,17]-s[[6]]) <= tol) {  # =1 if common
            scale_drop19[i,25] = 1
          } else {
            scale_drop19[i,25] = 0
          }
        }, error = function(e){})
      }  
      # Question 7 Tolerance
      for (i in 1:nrow(resC_drop19)) {
        tryCatch ( {
          if(abs(scale_drop19[i,18]-s[[7]]) <= tol) {  # =1 if common
            scale_drop19[i,26] = 1
          } else {
            scale_drop19[i,26] = 0
          }
        }, error = function(e){})
      }  
      # Question 8 Tolerance
      for (i in 1:nrow(resC_drop19)) {
        tryCatch ( {
          if(abs(scale_drop19[i,19]-s[[8]]) <= tol) {  # =1 if common
            scale_drop19[i,27] = 1
          } else {
            scale_drop19[i,27] = 0
          }
        }, error = function(e){})
      } 
      # Replace T_i to "NA" when standardized values are equal to "NA"
      # Question 1
      for (i in 1:nrow(resC_drop19)) {
        tryCatch ( {
          if(is.na(scale_drop19[i,12])) {
            scale_drop19[i,20] = NA
          } 
        }, error = function(e){})
      }
      # Question 2
      for (i in 1:nrow(resC_drop19)) {
        tryCatch ( {
          if(is.na(scale_drop19[i,13])) {
            scale_drop19[i,21] = NA
          } 
        }, error = function(e){})
      }
      # Question 3
      for (i in 1:nrow(resC_drop19)) {
        tryCatch ( {
          if(is.na(scale_drop19[i,14])) {
            scale_drop19[i,22] = NA
          } 
        }, error = function(e){})
      }
      # Question 4
      for (i in 1:nrow(resC_drop19)) {
        tryCatch ( {
          if(is.na(scale_drop19[i,15])) {
            scale_drop19[i,23] = NA
          } 
        }, error = function(e){})
      }
      # Question 5
      for (i in 1:nrow(resC_drop19)) {
        tryCatch ( {
          if(is.na(scale_drop19[i,16])) {
            scale_drop19[i,24] = NA
          } 
        }, error = function(e){})
      }    
      # Question 6
      for (i in 1:nrow(resC_drop19)) {
        tryCatch ( {
          if(is.na(scale_drop19[i,17])) {
            scale_drop19[i,25] = NA
          } 
        }, error = function(e){})
      }    
      # Question 7
      for (i in 1:nrow(resC_drop19)) {
        tryCatch ( {
          if(is.na(scale_drop19[i,18])) {
            scale_drop19[i,26] = NA
          } 
        }, error = function(e){})
      }    
      # Question 8
      for (i in 1:nrow(resC_drop19)) {
        tryCatch ( {
          if(is.na(scale_drop19[i,19])) {
            scale_drop19[i,27] = NA
          } 
        }, error = function(e){})
      }    
      # Sum number of common items per country
      for (i in 1:nrow(resC_drop19)) {
        scale_drop19$check[i] <- rowSums(scale_drop19[i,20:27],na.rm=TRUE)
      }
      # Fixing NaN values
      scale_drop19$S1[is.nan(scale_drop19$S1)]<-NA
      scale_drop19$S2[is.nan(scale_drop19$S2)]<-NA
      scale_drop19$S3[is.nan(scale_drop19$S3)]<-NA
      scale_drop19$S4[is.nan(scale_drop19$S4)]<-NA
      scale_drop19$S5[is.nan(scale_drop19$S5)]<-NA
      scale_drop19$S6[is.nan(scale_drop19$S6)]<-NA
      scale_drop19$S7[is.nan(scale_drop19$S7)]<-NA
      scale_drop19$S8[is.nan(scale_drop19$S8)]<-NA    
      # Safe-guard
      A <- scale_drop19                                   # safe-guard
      # WHILE CONDITIONS UPDATE
      common_n <- sum(scale_drop19$check, na.rm=TRUE) # total number common items
      NA_n     <- sum(is.na(scale_drop19$Q1))+sum(is.na(scale_drop19$Q2))+sum(is.na(scale_drop19$Q3))+sum(is.na(scale_drop19$Q4))+sum(is.na(scale_drop19$Q5))+sum(is.na(scale_drop19$Q6))+sum(is.na(scale_drop19$Q7))+sum(is.na(scale_drop19$Q8)) 
      unique_n <- sum(scale_drop19$T1 == 0, na.rm=TRUE)+sum(scale_drop19$T2 == 0, na.rm=TRUE)+sum(scale_drop19$T3 == 0, na.rm=TRUE)+sum(scale_drop19$T4 == 0, na.rm=TRUE)+sum(scale_drop19$T5 == 0, na.rm=TRUE)+sum(scale_drop19$T6 == 0, na.rm=TRUE)+sum(scale_drop19$T7 == 0, na.rm=TRUE)+sum(scale_drop19$T8 == 0, na.rm=TRUE)
      
    }
  }
}
    
## STEP 11: Printing Scale and Cleaning up
{
  # Cleaning
  rm(m_FULL,FULL)
  rm(b1,b2,b3,b4,b5,b6,b7,b8)
  rm(e1,e2,e3,e4,e5,e6,e7,e8)
  rm(infit1,infit2,infit3,infit4,infit5,infit6,infit7,infit8)
  rm(outfit1,outfit2,outfit3,outfit4,outfit5,outfit6,outfit7,outfit8)
  rm(se.b1,se.b2,se.b3,se.b4,se.b5,se.b6,se.b7,se.b8)
  rm(s1,s2,s3,s4,s5,s6,s7,s8,x,d,dT)
  
  # Dropping countries with less than four common items
  for (i in 1:nrow(resC_drop19)) {
    tryCatch ( {
      if(scale_drop19[i,28] < 4) {  
        scale_drop19[i,12] = NA
        scale_drop19[i,13] = NA
        scale_drop19[i,14] = NA
        scale_drop19[i,15] = NA
        scale_drop19[i,16] = NA
        scale_drop19[i,17] = NA
        scale_drop19[i,18] = NA
        scale_drop19[i,19] = NA      
      } 
    }, error = function(e){})
  } 
  scale_drop19 <- scale_drop19[!(scale_drop19$ISO3 == "" | is.na(scale_drop19$ISO3)), ]
  # The completed global scale
  b <- data.frame(lapply(scale_drop19[,12:19], median, na.rm = TRUE))
}

## STEP 12: Equating for all countries to global scale
{
  # Create a place to save results
  equate_drop19 <- subset(resC_drop19,select = c('WP5','ISO3','Freq','Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8'))
  v1 <- data.frame(matrix(ncol = 23, nrow = nrow(resC_drop19)))
  colnames(v1) <- c('T1','T2','T3','T4','T5','T6','T7','T8','scale','shift','common1','common2','common3','common4','common5','common6','common7','common8','prev_FImod+','prev_FIsev','corr_comm','adj_FImod+','adj_FIsev')
  equate_drop19 <- cbind(equate_drop19, v1)

  # Run loop over Rasch model then extract equated values to the data frame
  b <- as.numeric(b) # to make the global loop accessible by FAO's e.e function
  row.names(resC_drop19) <- 1:nrow(resC_drop19)
  
    for (i in resC_drop19$ISO3) {
      tryCatch ( {
        temp_w <- subset(get(i), select = c(wt))
        temp <- subset(get(i), select = c(WORRIED:WHLDAY))
        res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
        ee1 = equating.fun(res, st = b, tol = tol, spec.com1 = c(1:8), spec.com2 = c(1:8)) 
        # Extracting values
        n <- which(equate_drop19$ISO3 == i)
        equate_drop19[n,20] <- ee1$scale
        equate_drop19[n,21] <- ee1$shift
        equate_drop19[n,22] <- ee1$common[1]
        equate_drop19[n,23] <- ee1$common[2]
        equate_drop19[n,24] <- ee1$common[3]
        equate_drop19[n,25] <- ee1$common[4]
        equate_drop19[n,26] <- ee1$common[5]
        equate_drop19[n,27] <- ee1$common[6]
        equate_drop19[n,28] <- ee1$common[7]
        equate_drop19[n,29] <- ee1$common[8]
        equate_drop19[n,30] <- ee1$prevs[1]
        equate_drop19[n,31] <- ee1$prevs[2]
        equate_drop19[n,32] <- ee1$cor.comm.items
        equate_drop19[n,33] <- ee1$adj.thres[1]
        equate_drop19[n,34] <- ee1$adj.thres[2]
        }, error = function(e){})
    }

  # Fill in questions with "NA" if deemed unique by e.e function
    equate_drop19$Q1[equate_drop19$common1 == "Unique"] <- NA
    equate_drop19$Q2[equate_drop19$common2 == "Unique"] <- NA
    equate_drop19$Q3[equate_drop19$common3 == "Unique"] <- NA
    equate_drop19$Q4[equate_drop19$common4 == "Unique"] <- NA
    equate_drop19$Q5[equate_drop19$common5 == "Unique"] <- NA
    equate_drop19$Q6[equate_drop19$common6 == "Unique"] <- NA
    equate_drop19$Q7[equate_drop19$common7 == "Unique"] <- NA
    equate_drop19$Q8[equate_drop19$common8 == "Unique"] <- NA
    names(equate_drop19)[names(equate_drop19) == 'Freq'] <- 'cond'   # OPTION: if needed
  # Global and country scale characteristics toward adjustment
    for (i in 1:nrow(resC_drop19)) {
      s1 <- s
      s1$S1[equate_drop19[i,22] == "Unique"] <- NA
      s1$S2[equate_drop19[i,23] == "Unique"] <- NA
      s1$S3[equate_drop19[i,24] == "Unique"] <- NA
      s1$S4[equate_drop19[i,25] == "Unique"] <- NA
      s1$S5[equate_drop19[i,26] == "Unique"] <- NA
      s1$S6[equate_drop19[i,27] == "Unique"] <- NA
      s1$S7[equate_drop19[i,28] == "Unique"] <- NA
      s1$S8[equate_drop19[i,29] == "Unique"] <- NA
      mean_g <- rowMeans(s1, na.rm=TRUE)                        # 1.2213 test
      sd_g   <- apply(s1, 1, sd,na.rm=TRUE)                     # -0.165 test
    # Country scale characteristics                             
      mean_c <- rowMeans(equate_drop19[i,c(4:11)], na.rm = TRUE)  # -0.1393
      sd_c <- apply(equate_drop19[i,c(4:11)], 1, sd, na.rm=TRUE)  # 1.33817
    # Fill the things back in
      equate_drop19[i,4][is.na(equate_drop19[i,4])] <- resC_drop19[i,4]
      equate_drop19[i,5][is.na(equate_drop19[i,5])] <- resC_drop19[i,5]
      equate_drop19[i,6][is.na(equate_drop19[i,6])] <- resC_drop19[i,6]
      equate_drop19[i,7][is.na(equate_drop19[i,7])] <- resC_drop19[i,7]
      equate_drop19[i,8][is.na(equate_drop19[i,8])] <- resC_drop19[i,8]
      equate_drop19[i,9][is.na(equate_drop19[i,9])] <- resC_drop19[i,9]
      equate_drop19[i,10][is.na(equate_drop19[i,10])] <- resC_drop19[i,10]
      equate_drop19[i,11][is.na(equate_drop19[i,11])] <- resC_drop19[i,11]
    # SHIFT THE THINGS
      equate_drop19[i,12] <- (equate_drop19[i,4] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
      equate_drop19[i,13] <- (equate_drop19[i,5] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
      equate_drop19[i,14] <- (equate_drop19[i,6] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
      equate_drop19[i,15] <- (equate_drop19[i,7] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
      equate_drop19[i,16] <- (equate_drop19[i,8] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
      equate_drop19[i,17] <- (equate_drop19[i,9] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
      equate_drop19[i,18] <- (equate_drop19[i,10] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
      equate_drop19[i,19] <- (equate_drop19[i,11] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
  }
}

## FINAL: Output a table of translated values (adjustments req. for CSVs)
{
  # Full set
  equated_drop19 <- subset(equate_drop19,select = c('WP5','ISO3','T1','T2','T3','T4','T5','T6','T7','T8','corr_comm'))
  View(equated_drop19)
  
  # Print Rasch scores
  b
  
  # Print tabulation
  tabulate(scale_drop19$check)
  
  # Relevant CSVs
  # CSV for 1.d
    write.csv(resC_drop19, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\resC_drop19_1d.csv")
    write.csv(scale_drop19, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\scale_drop19_1d.csv")
    write.csv(equated_drop19, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\equated_drop19_1d.csv")
    write.csv(s, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\global_scale_drop19_1d.csv")
    write.csv(resC_drop19, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\resC_drop19.csv")
    write.csv(scale_drop19, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\scale_drop19.csv")
    write.csv(equated_drop19, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\equated_drop19.csv")
    write.csv(s, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\global_scale_drop19.csv")
  
  # CSV for 1.d.2 (Infit < 1.4)
    #write.csv(resC_drop19, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\resC_drop19_1d2.csv")
    #write.csv(scale_drop19, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\scale_drop19_1d2.csv")
    #write.csv(equated_drop19, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\equated_drop19_1d2.csv")
    #write.csv(s, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\global_scale_drop19_1d2.csv")
  
  # CSV for 1.d.1 (Infit < 1.3)
    #write.csv(resC_drop19, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\resC_drop19_1d1.csv")
    #write.csv(scale_drop19, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\scale_drop19_1d1.csv")
    #write.csv(equated_drop19, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\equated_drop19_1d1.csv")
    #write.csv(s, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\global_scale_drop19_1d1.csv")
  
  
  
}



    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    