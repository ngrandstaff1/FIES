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
# 5.      Save data frame placeholders by C-Y index (resC_stabF)    
# 6.      Creating and saving the data frame for CY estimates
# 7.      Creating a (C) key
# 8.      Creating a (C-Y) key and estimating Rasch Model by C-Y
# 9.      Fixing (C-Y) pairs missing items and estimating Rasch Model
# 10.     (C-FULL) "WHILE" so updates create no unique items (Iterative step)
# 11.     Printing Scale and Cleaning up
# 12.     Equating for all countries to global scale

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

## STEP 6:  Creating and saving the data frame for CY estimates
{
  # (C-Y Stability: FULL) Create data frame in which to store RM results
  resC_stabD <- data.frame(matrix(ncol = 34, nrow = nrow(cy)))
  colnames(resC_stabD) <- c('Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8',
                            'q1_se.b','q2_se.b','q3_se.b','q4_se.b','q5_se.b','q6_se.b','q7_se.b','q8_se.b',
                            'reliab','reliab.fl',
                            'O_q1','O_q2','O_q3','O_q4','O_q5','O_q6','O_q7','O_q8',
                            'I_q1','I_q2','I_q3','I_q4','I_q5','I_q6','I_q7','I_q8')
  v1 <- data.frame(matrix(ncol=3,nrow=nrow(cy)))
  v1 <- data.frame(CY_key$ISO3,CY_key$YEAR_WAVE,CY_key$CY_key0)
  colnames(v1) <- c('WP5','YEAR_WAVE','CY_key0')
  # v1 <- merge(c1_ISO3,v1,by="WP5") # NOT SURE WHAT THIS DOES IN NON-STAB CASE
  resC_stabD <- merge(v1,resC_stabD)
  resC_stabD <- resC_stabD[!duplicated(resC_stabD$CY_key0),]
}

## STEP 7:  Creating a (C) key                   
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
  rm(i,df)
}

## STEP 8:  Creating a (C-Y) key and estimating Rasch Model by C-Y
{
  ## 2014
  # (C-Y) Create 2014 and populate the data set
  CY_key_2014 <- subset(CY_key, YEAR_WAVE==2014, select=c(WP5:CY_key0))
  # Creating C-Y 2014 Data Subsets
  for (i in CY_key_2014$ISO3) {
    df <- data.frame(subset(m_FULL, (ISO3==i) & (YEAR_WAVE==2014), select=c(WORRIED:WHLDAY, wt)))
    assign(paste(i,2014,sep = "_", collapse = NULL),df )
  }
  # Saving estimates of C-Y 2014 Rasch score output
  for (i in CY_key_2014$CY_key0) {
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
      # Save Reliabilities 
      reliab <- res$reliab
      reliab.fl <- res$reliab.fl
      # Save Non-standardized Rasch scores 
      resC_stabD$Q1[resC_stabD$CY_key0 == i] <- b1
      resC_stabD$Q2[resC_stabD$CY_key0 == i] <- b2
      resC_stabD$Q3[resC_stabD$CY_key0 == i] <- b3
      resC_stabD$Q4[resC_stabD$CY_key0 == i] <- b4
      resC_stabD$Q5[resC_stabD$CY_key0 == i] <- b5
      resC_stabD$Q6[resC_stabD$CY_key0 == i] <- b6
      resC_stabD$Q7[resC_stabD$CY_key0 == i] <- b7
      resC_stabD$Q8[resC_stabD$CY_key0 == i] <- b8
      # Standard errors on coefficients
      resC_stabD$q1_se.b[resC_stabD$CY_key0 == i] <- se.b1
      resC_stabD$q2_se.b[resC_stabD$CY_key0 == i] <- se.b2
      resC_stabD$q3_se.b[resC_stabD$CY_key0 == i] <- se.b3
      resC_stabD$q4_se.b[resC_stabD$CY_key0 == i] <- se.b4
      resC_stabD$q5_se.b[resC_stabD$CY_key0 == i] <- se.b5
      resC_stabD$q6_se.b[resC_stabD$CY_key0 == i] <- se.b6
      resC_stabD$q7_se.b[resC_stabD$CY_key0 == i] <- se.b7
      resC_stabD$q8_se.b[resC_stabD$CY_key0 == i] <- se.b8
      # Share Reliabilities
      resC_stabD$reliab[resC_stabD$CY_key0 == i] <- reliab
      resC_stabD$reliab.fl[resC_stabD$CY_key0 == i] <- reliab.fl
      # Infits
      resC_stabD$I_q1[resC_stabD$CY_key0 == i] <- infit1
      resC_stabD$I_q2[resC_stabD$CY_key0 == i] <- infit2
      resC_stabD$I_q3[resC_stabD$CY_key0 == i] <- infit3
      resC_stabD$I_q4[resC_stabD$CY_key0 == i] <- infit4
      resC_stabD$I_q5[resC_stabD$CY_key0 == i] <- infit5
      resC_stabD$I_q6[resC_stabD$CY_key0 == i] <- infit6
      resC_stabD$I_q7[resC_stabD$CY_key0 == i] <- infit7
      resC_stabD$I_q8[resC_stabD$CY_key0 == i] <- infit8
      # Outfits
      resC_stabD$O_q1[resC_stabD$CY_key0 == i] <- outfit1 
      resC_stabD$O_q2[resC_stabD$CY_key0 == i] <- outfit2
      resC_stabD$O_q3[resC_stabD$CY_key0 == i] <- outfit3
      resC_stabD$O_q4[resC_stabD$CY_key0 == i] <- outfit4
      resC_stabD$O_q5[resC_stabD$CY_key0 == i] <- outfit5 
      resC_stabD$O_q6[resC_stabD$CY_key0 == i] <- outfit6 
      resC_stabD$O_q7[resC_stabD$CY_key0 == i] <- outfit7 
      resC_stabD$O_q8[resC_stabD$CY_key0 == i] <- outfit8 
    }, error = function(e){})
  }
  # Delete C-Y 2014 data sets
  for (i in CY_key_2014$CY_key0) {
    rm(list=paste0(i))
  }
  
  ## 2015
  # (C-Y) Create 2015 and populate the data set
  CY_key_2015 <- subset(CY_key, YEAR_WAVE==2015, select=c(WP5:CY_key0))
  # Creating C-Y 2015 Data Subsets
  for (i in CY_key_2015$ISO3) {
    df <- data.frame(subset(m_FULL, (ISO3==i) & (YEAR_WAVE==2015), select=c(WORRIED:WHLDAY, wt)))
    assign(paste(i,2015,sep = "_", collapse = NULL),df )
  }
  # Saving estimates of C-Y 2015 Rasch score output
  for (i in CY_key_2015$CY_key0) {
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
      # Save Reliabilities 
      reliab <- res$reliab
      reliab.fl <- res$reliab.fl
      # Save Non-standardized Rasch scores 
      resC_stabD$Q1[resC_stabD$CY_key0 == i] <- b1
      resC_stabD$Q2[resC_stabD$CY_key0 == i] <- b2
      resC_stabD$Q3[resC_stabD$CY_key0 == i] <- b3
      resC_stabD$Q4[resC_stabD$CY_key0 == i] <- b4
      resC_stabD$Q5[resC_stabD$CY_key0 == i] <- b5
      resC_stabD$Q6[resC_stabD$CY_key0 == i] <- b6
      resC_stabD$Q7[resC_stabD$CY_key0 == i] <- b7
      resC_stabD$Q8[resC_stabD$CY_key0 == i] <- b8
      # Standard errors on coefficients
      resC_stabD$q1_se.b[resC_stabD$CY_key0 == i] <- se.b1
      resC_stabD$q2_se.b[resC_stabD$CY_key0 == i] <- se.b2
      resC_stabD$q3_se.b[resC_stabD$CY_key0 == i] <- se.b3
      resC_stabD$q4_se.b[resC_stabD$CY_key0 == i] <- se.b4
      resC_stabD$q5_se.b[resC_stabD$CY_key0 == i] <- se.b5
      resC_stabD$q6_se.b[resC_stabD$CY_key0 == i] <- se.b6
      resC_stabD$q7_se.b[resC_stabD$CY_key0 == i] <- se.b7
      resC_stabD$q8_se.b[resC_stabD$CY_key0 == i] <- se.b8
      # Share Reliabilities
      resC_stabD$reliab[resC_stabD$CY_key0 == i] <- reliab
      resC_stabD$reliab.fl[resC_stabD$CY_key0 == i] <- reliab.fl
      # Infits
      resC_stabD$I_q1[resC_stabD$CY_key0 == i] <- infit1
      resC_stabD$I_q2[resC_stabD$CY_key0 == i] <- infit2
      resC_stabD$I_q3[resC_stabD$CY_key0 == i] <- infit3
      resC_stabD$I_q4[resC_stabD$CY_key0 == i] <- infit4
      resC_stabD$I_q5[resC_stabD$CY_key0 == i] <- infit5
      resC_stabD$I_q6[resC_stabD$CY_key0 == i] <- infit6
      resC_stabD$I_q7[resC_stabD$CY_key0 == i] <- infit7
      resC_stabD$I_q8[resC_stabD$CY_key0 == i] <- infit8
      # Outfits
      resC_stabD$O_q1[resC_stabD$CY_key0 == i] <- outfit1 
      resC_stabD$O_q2[resC_stabD$CY_key0 == i] <- outfit2
      resC_stabD$O_q3[resC_stabD$CY_key0 == i] <- outfit3
      resC_stabD$O_q4[resC_stabD$CY_key0 == i] <- outfit4
      resC_stabD$O_q5[resC_stabD$CY_key0 == i] <- outfit5 
      resC_stabD$O_q6[resC_stabD$CY_key0 == i] <- outfit6 
      resC_stabD$O_q7[resC_stabD$CY_key0 == i] <- outfit7 
      resC_stabD$O_q8[resC_stabD$CY_key0 == i] <- outfit8 
    }, error = function(e){})
  }
  # Delete C-Y 2015 data sets
  for (i in CY_key_2015$CY_key0) {
    rm(list=paste0(i))
  }
  
  ## 2016
  # (C-Y) Create 2016 and populate the data set       
  CY_key_2016 <- subset(CY_key, YEAR_WAVE==2016, select=c(WP5:CY_key0))
  # Creating C-Y 2016 Data Subsets
  for (i in CY_key_2016$ISO3) {
    df <- data.frame(subset(m_FULL, (ISO3==i) & (YEAR_WAVE==2016), select=c(WORRIED:WHLDAY, wt)))
    assign(paste(i,2016,sep = "_", collapse = NULL),df )
  }
  # Saving estimates of C-Y 2016 Rasch score output
  for (i in CY_key_2016$CY_key0) {
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
      # Save Reliabilities 
      reliab <- res$reliab
      reliab.fl <- res$reliab.fl
      # Save Non-standardized Rasch scores 
      resC_stabD$Q1[resC_stabD$CY_key0 == i] <- b1
      resC_stabD$Q2[resC_stabD$CY_key0 == i] <- b2
      resC_stabD$Q3[resC_stabD$CY_key0 == i] <- b3
      resC_stabD$Q4[resC_stabD$CY_key0 == i] <- b4
      resC_stabD$Q5[resC_stabD$CY_key0 == i] <- b5
      resC_stabD$Q6[resC_stabD$CY_key0 == i] <- b6
      resC_stabD$Q7[resC_stabD$CY_key0 == i] <- b7
      resC_stabD$Q8[resC_stabD$CY_key0 == i] <- b8
      # Standard errors on coefficients
      resC_stabD$q1_se.b[resC_stabD$CY_key0 == i] <- se.b1
      resC_stabD$q2_se.b[resC_stabD$CY_key0 == i] <- se.b2
      resC_stabD$q3_se.b[resC_stabD$CY_key0 == i] <- se.b3
      resC_stabD$q4_se.b[resC_stabD$CY_key0 == i] <- se.b4
      resC_stabD$q5_se.b[resC_stabD$CY_key0 == i] <- se.b5
      resC_stabD$q6_se.b[resC_stabD$CY_key0 == i] <- se.b6
      resC_stabD$q7_se.b[resC_stabD$CY_key0 == i] <- se.b7
      resC_stabD$q8_se.b[resC_stabD$CY_key0 == i] <- se.b8
      # Share Reliabilities
      resC_stabD$reliab[resC_stabD$CY_key0 == i] <- reliab
      resC_stabD$reliab.fl[resC_stabD$CY_key0 == i] <- reliab.fl
      # Infits
      resC_stabD$I_q1[resC_stabD$CY_key0 == i] <- infit1
      resC_stabD$I_q2[resC_stabD$CY_key0 == i] <- infit2
      resC_stabD$I_q3[resC_stabD$CY_key0 == i] <- infit3
      resC_stabD$I_q4[resC_stabD$CY_key0 == i] <- infit4
      resC_stabD$I_q5[resC_stabD$CY_key0 == i] <- infit5
      resC_stabD$I_q6[resC_stabD$CY_key0 == i] <- infit6
      resC_stabD$I_q7[resC_stabD$CY_key0 == i] <- infit7
      resC_stabD$I_q8[resC_stabD$CY_key0 == i] <- infit8
      # Outfits
      resC_stabD$O_q1[resC_stabD$CY_key0 == i] <- outfit1 
      resC_stabD$O_q2[resC_stabD$CY_key0 == i] <- outfit2
      resC_stabD$O_q3[resC_stabD$CY_key0 == i] <- outfit3
      resC_stabD$O_q4[resC_stabD$CY_key0 == i] <- outfit4
      resC_stabD$O_q5[resC_stabD$CY_key0 == i] <- outfit5 
      resC_stabD$O_q6[resC_stabD$CY_key0 == i] <- outfit6 
      resC_stabD$O_q7[resC_stabD$CY_key0 == i] <- outfit7 
      resC_stabD$O_q8[resC_stabD$CY_key0 == i] <- outfit8 
    }, error = function(e){})
  }
  # Delete C-Y 2016 data sets
  for (i in CY_key_2016$CY_key0) {
    rm(list=paste0(i))
  }
  
  ## 2017
  # (C-Y) Create 2017 and populate the data set       
  CY_key_2017 <- subset(CY_key, YEAR_WAVE==2017, select=c(WP5:CY_key0))
  # Creating C-Y 2017 Data Subsets
  for (i in CY_key_2017$ISO3) {
    df <- data.frame(subset(m_FULL, (ISO3==i) & (YEAR_WAVE==2017), select=c(WORRIED:WHLDAY, wt)))
    assign(paste(i,2017,sep = "_", collapse = NULL),df )
  }
  # Saving estimates of C-Y 2017 Rasch score output
  for (i in CY_key_2017$CY_key0) {
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
      # Save Reliabilities 
      reliab <- res$reliab
      reliab.fl <- res$reliab.fl
      # Save Non-standardized Rasch scores 
      resC_stabD$Q1[resC_stabD$CY_key0 == i] <- b1
      resC_stabD$Q2[resC_stabD$CY_key0 == i] <- b2
      resC_stabD$Q3[resC_stabD$CY_key0 == i] <- b3
      resC_stabD$Q4[resC_stabD$CY_key0 == i] <- b4
      resC_stabD$Q5[resC_stabD$CY_key0 == i] <- b5
      resC_stabD$Q6[resC_stabD$CY_key0 == i] <- b6
      resC_stabD$Q7[resC_stabD$CY_key0 == i] <- b7
      resC_stabD$Q8[resC_stabD$CY_key0 == i] <- b8
      # Standard errors on coefficients
      resC_stabD$q1_se.b[resC_stabD$CY_key0 == i] <- se.b1
      resC_stabD$q2_se.b[resC_stabD$CY_key0 == i] <- se.b2
      resC_stabD$q3_se.b[resC_stabD$CY_key0 == i] <- se.b3
      resC_stabD$q4_se.b[resC_stabD$CY_key0 == i] <- se.b4
      resC_stabD$q5_se.b[resC_stabD$CY_key0 == i] <- se.b5
      resC_stabD$q6_se.b[resC_stabD$CY_key0 == i] <- se.b6
      resC_stabD$q7_se.b[resC_stabD$CY_key0 == i] <- se.b7
      resC_stabD$q8_se.b[resC_stabD$CY_key0 == i] <- se.b8
      # Share Reliabilities
      resC_stabD$reliab[resC_stabD$CY_key0 == i] <- reliab
      resC_stabD$reliab.fl[resC_stabD$CY_key0 == i] <- reliab.fl
      # Infits
      resC_stabD$I_q1[resC_stabD$CY_key0 == i] <- infit1
      resC_stabD$I_q2[resC_stabD$CY_key0 == i] <- infit2
      resC_stabD$I_q3[resC_stabD$CY_key0 == i] <- infit3
      resC_stabD$I_q4[resC_stabD$CY_key0 == i] <- infit4
      resC_stabD$I_q5[resC_stabD$CY_key0 == i] <- infit5
      resC_stabD$I_q6[resC_stabD$CY_key0 == i] <- infit6
      resC_stabD$I_q7[resC_stabD$CY_key0 == i] <- infit7
      resC_stabD$I_q8[resC_stabD$CY_key0 == i] <- infit8
      # Outfits
      resC_stabD$O_q1[resC_stabD$CY_key0 == i] <- outfit1 
      resC_stabD$O_q2[resC_stabD$CY_key0 == i] <- outfit2
      resC_stabD$O_q3[resC_stabD$CY_key0 == i] <- outfit3
      resC_stabD$O_q4[resC_stabD$CY_key0 == i] <- outfit4
      resC_stabD$O_q5[resC_stabD$CY_key0 == i] <- outfit5 
      resC_stabD$O_q6[resC_stabD$CY_key0 == i] <- outfit6 
      resC_stabD$O_q7[resC_stabD$CY_key0 == i] <- outfit7 
      resC_stabD$O_q8[resC_stabD$CY_key0 == i] <- outfit8 
    }, error = function(e){})
  }
  # Delete C-Y 2017 data sets
  for (i in CY_key_2017$CY_key0) {
    rm(list=paste0(i))
  }
  
  ## 2018
  # (C-Y) Create 2018 and populate the data set       
  CY_key_2018 <- subset(CY_key, YEAR_WAVE==2018, select=c(WP5:CY_key0))
  # Creating C-Y 2018 Data Subsets
  for (i in CY_key_2018$ISO3) {
    df <- data.frame(subset(m_FULL, (ISO3==i) & (YEAR_WAVE==2018), select=c(WORRIED:WHLDAY, wt)))
    assign(paste(i,2018,sep = "_", collapse = NULL),df )
  }
  # Saving estimates of C-Y 2018 Rasch score output
  for (i in CY_key_2018$CY_key0) {
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
      # Save Reliabilities 
      reliab <- res$reliab
      reliab.fl <- res$reliab.fl
      # Save Non-standardized Rasch scores 
      resC_stabD$Q1[resC_stabD$CY_key0 == i] <- b1
      resC_stabD$Q2[resC_stabD$CY_key0 == i] <- b2
      resC_stabD$Q3[resC_stabD$CY_key0 == i] <- b3
      resC_stabD$Q4[resC_stabD$CY_key0 == i] <- b4
      resC_stabD$Q5[resC_stabD$CY_key0 == i] <- b5
      resC_stabD$Q6[resC_stabD$CY_key0 == i] <- b6
      resC_stabD$Q7[resC_stabD$CY_key0 == i] <- b7
      resC_stabD$Q8[resC_stabD$CY_key0 == i] <- b8
      # Standard errors on coefficients
      resC_stabD$q1_se.b[resC_stabD$CY_key0 == i] <- se.b1
      resC_stabD$q2_se.b[resC_stabD$CY_key0 == i] <- se.b2
      resC_stabD$q3_se.b[resC_stabD$CY_key0 == i] <- se.b3
      resC_stabD$q4_se.b[resC_stabD$CY_key0 == i] <- se.b4
      resC_stabD$q5_se.b[resC_stabD$CY_key0 == i] <- se.b5
      resC_stabD$q6_se.b[resC_stabD$CY_key0 == i] <- se.b6
      resC_stabD$q7_se.b[resC_stabD$CY_key0 == i] <- se.b7
      resC_stabD$q8_se.b[resC_stabD$CY_key0 == i] <- se.b8
      # Share Reliabilities
      resC_stabD$reliab[resC_stabD$CY_key0 == i] <- reliab
      resC_stabD$reliab.fl[resC_stabD$CY_key0 == i] <- reliab.fl
      # Infits
      resC_stabD$I_q1[resC_stabD$CY_key0 == i] <- infit1
      resC_stabD$I_q2[resC_stabD$CY_key0 == i] <- infit2
      resC_stabD$I_q3[resC_stabD$CY_key0 == i] <- infit3
      resC_stabD$I_q4[resC_stabD$CY_key0 == i] <- infit4
      resC_stabD$I_q5[resC_stabD$CY_key0 == i] <- infit5
      resC_stabD$I_q6[resC_stabD$CY_key0 == i] <- infit6
      resC_stabD$I_q7[resC_stabD$CY_key0 == i] <- infit7
      resC_stabD$I_q8[resC_stabD$CY_key0 == i] <- infit8
      # Outfits
      resC_stabD$O_q1[resC_stabD$CY_key0 == i] <- outfit1 
      resC_stabD$O_q2[resC_stabD$CY_key0 == i] <- outfit2
      resC_stabD$O_q3[resC_stabD$CY_key0 == i] <- outfit3
      resC_stabD$O_q4[resC_stabD$CY_key0 == i] <- outfit4
      resC_stabD$O_q5[resC_stabD$CY_key0 == i] <- outfit5 
      resC_stabD$O_q6[resC_stabD$CY_key0 == i] <- outfit6 
      resC_stabD$O_q7[resC_stabD$CY_key0 == i] <- outfit7 
      resC_stabD$O_q8[resC_stabD$CY_key0 == i] <- outfit8 
    }, error = function(e){})
  }
  # Delete C-Y 2018 data sets
  for (i in CY_key_2018$CY_key0) {
    rm(list=paste0(i))
  }
  
  ## 2019
  # (C-Y) Create 2019 and populate the data set       
  CY_key_2019 <- subset(CY_key, YEAR_WAVE==2019, select=c(WP5:CY_key0))
  # Creating C-Y 2019 Data Subsets
  for (i in CY_key_2019$ISO3) {
    df <- data.frame(subset(m_FULL, (ISO3==i) & (YEAR_WAVE==2019), select=c(WORRIED:WHLDAY, wt)))
    assign(paste(i,2019,sep = "_", collapse = NULL),df )
  }
  # Saving estimates of C-Y 2019 Rasch score output
  for (i in CY_key_2019$CY_key0) {
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
      # Save Reliabilities 
      reliab <- res$reliab
      reliab.fl <- res$reliab.fl
      # Save Non-standardized Rasch scores 
      resC_stabD$Q1[resC_stabD$CY_key0 == i] <- b1
      resC_stabD$Q2[resC_stabD$CY_key0 == i] <- b2
      resC_stabD$Q3[resC_stabD$CY_key0 == i] <- b3
      resC_stabD$Q4[resC_stabD$CY_key0 == i] <- b4
      resC_stabD$Q5[resC_stabD$CY_key0 == i] <- b5
      resC_stabD$Q6[resC_stabD$CY_key0 == i] <- b6
      resC_stabD$Q7[resC_stabD$CY_key0 == i] <- b7
      resC_stabD$Q8[resC_stabD$CY_key0 == i] <- b8
      # Standard errors on coefficients
      resC_stabD$q1_se.b[resC_stabD$CY_key0 == i] <- se.b1
      resC_stabD$q2_se.b[resC_stabD$CY_key0 == i] <- se.b2
      resC_stabD$q3_se.b[resC_stabD$CY_key0 == i] <- se.b3
      resC_stabD$q4_se.b[resC_stabD$CY_key0 == i] <- se.b4
      resC_stabD$q5_se.b[resC_stabD$CY_key0 == i] <- se.b5
      resC_stabD$q6_se.b[resC_stabD$CY_key0 == i] <- se.b6
      resC_stabD$q7_se.b[resC_stabD$CY_key0 == i] <- se.b7
      resC_stabD$q8_se.b[resC_stabD$CY_key0 == i] <- se.b8
      # Share Reliabilities
      resC_stabD$reliab[resC_stabD$CY_key0 == i] <- reliab
      resC_stabD$reliab.fl[resC_stabD$CY_key0 == i] <- reliab.fl
      # Infits
      resC_stabD$I_q1[resC_stabD$CY_key0 == i] <- infit1
      resC_stabD$I_q2[resC_stabD$CY_key0 == i] <- infit2
      resC_stabD$I_q3[resC_stabD$CY_key0 == i] <- infit3
      resC_stabD$I_q4[resC_stabD$CY_key0 == i] <- infit4
      resC_stabD$I_q5[resC_stabD$CY_key0 == i] <- infit5
      resC_stabD$I_q6[resC_stabD$CY_key0 == i] <- infit6
      resC_stabD$I_q7[resC_stabD$CY_key0 == i] <- infit7
      resC_stabD$I_q8[resC_stabD$CY_key0 == i] <- infit8
      # Outfits
      resC_stabD$O_q1[resC_stabD$CY_key0 == i] <- outfit1 
      resC_stabD$O_q2[resC_stabD$CY_key0 == i] <- outfit2
      resC_stabD$O_q3[resC_stabD$CY_key0 == i] <- outfit3
      resC_stabD$O_q4[resC_stabD$CY_key0 == i] <- outfit4
      resC_stabD$O_q5[resC_stabD$CY_key0 == i] <- outfit5 
      resC_stabD$O_q6[resC_stabD$CY_key0 == i] <- outfit6 
      resC_stabD$O_q7[resC_stabD$CY_key0 == i] <- outfit7 
      resC_stabD$O_q8[resC_stabD$CY_key0 == i] <- outfit8 
    }, error = function(e){})
  }
  # Delete C-Y 2019 data sets
  for (i in CY_key_2019$CY_key0) {
    rm(list=paste0(i))
  }
}

## STEP 9:  Fixing (C-Y) pairs missing  items countries (JPN, MLT, NLD, DNK)
{
  # Denmark - missing 1 & 7
  {
    DNK_2014 <- data.frame(subset(m_FULL, (ISO3=="DNK") & (YEAR_WAVE==2014), select=c(HEALTHY:RUNOUT, WHLDAY, wt)))
    DNK_2015 <- data.frame(subset(m_FULL, (ISO3=="DNK") & (YEAR_WAVE==2015), select=c(HEALTHY:RUNOUT, WHLDAY, wt)))
    DNK_2016 <- data.frame(subset(m_FULL, (ISO3=="DNK") & (YEAR_WAVE==2016), select=c(HEALTHY:RUNOUT, WHLDAY, wt)))
    DNK_2017 <- data.frame(subset(m_FULL, (ISO3=="DNK") & (YEAR_WAVE==2017), select=c(HEALTHY:RUNOUT, WHLDAY, wt)))
    DNK_2018 <- data.frame(subset(m_FULL, (ISO3=="DNK") & (YEAR_WAVE==2018), select=c(HEALTHY:RUNOUT, WHLDAY, wt)))
    DNK_v <- c('DNK_2014', 'DNK_2015', 'DNK_2016', 'DNK_2017', 'DNK_2018')
    DNK_v <- data.frame(DNK_v)
    for (i in DNK_v$DNK_v) {
      tryCatch ( {
        temp_w <- subset(get(i), select = c(wt))
        temp <- subset(get(i), select = c(HEALTHY:RUNOUT, WHLDAY))
        res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
        for (n in 1:6) {
          x <- res$b[[n]]
          assign(paste("b", n, sep=""),x) 
          y <- res$se.b[[n]]
          assign(paste("se.b", n, sep=""),y) 
          z <- res$infit[[n]]
          assign(paste("infit", n, sep=""),z) 
          w <- res$outfit[[n]]
          assign(paste("outfit", n, sep=""),w) 
        }
        # Save Reliabilities 
        reliab <- res$reliab
        reliab.fl <- res$reliab.fl
        # Save Non-standardized Rasch scores 
        resC_stabD$Q2[resC_stabD$CY_key0 == i] <- b1
        resC_stabD$Q3[resC_stabD$CY_key0 == i] <- b2
        resC_stabD$Q4[resC_stabD$CY_key0 == i] <- b3
        resC_stabD$Q5[resC_stabD$CY_key0 == i] <- b4
        resC_stabD$Q6[resC_stabD$CY_key0 == i] <- b5
        resC_stabD$Q8[resC_stabD$CY_key0 == i] <- b6
        # Standard errors on coefficients
        resC_stabD$q2_se.b[resC_stabD$CY_key0 == i] <- se.b1
        resC_stabD$q3_se.b[resC_stabD$CY_key0 == i] <- se.b2
        resC_stabD$q4_se.b[resC_stabD$CY_key0 == i] <- se.b3
        resC_stabD$q5_se.b[resC_stabD$CY_key0 == i] <- se.b4
        resC_stabD$q6_se.b[resC_stabD$CY_key0 == i] <- se.b5
        resC_stabD$q8_se.b[resC_stabD$CY_key0 == i] <- se.b6
        # Share Reliabilities
        resC_stabD$reliab[resC_stabD$CY_key0 == i] <- reliab
        resC_stabD$reliab.fl[resC_stabD$CY_key0 == i] <- reliab.fl
        # Infits
        resC_stabD$I_q2[resC_stabD$CY_key0 == i] <- infit1
        resC_stabD$I_q3[resC_stabD$CY_key0 == i] <- infit2
        resC_stabD$I_q4[resC_stabD$CY_key0 == i] <- infit3
        resC_stabD$I_q5[resC_stabD$CY_key0 == i] <- infit4
        resC_stabD$I_q6[resC_stabD$CY_key0 == i] <- infit5
        resC_stabD$I_q8[resC_stabD$CY_key0 == i] <- infit6
        # Outfits
        resC_stabD$O_q2[resC_stabD$CY_key0 == i] <- outfit1
        resC_stabD$O_q3[resC_stabD$CY_key0 == i] <- outfit2
        resC_stabD$O_q4[resC_stabD$CY_key0 == i] <- outfit3
        resC_stabD$O_q5[resC_stabD$CY_key0 == i] <- outfit4 
        resC_stabD$O_q6[resC_stabD$CY_key0 == i] <- outfit5 
        resC_stabD$O_q8[resC_stabD$CY_key0 == i] <- outfit6 
      }, error = function(e){})
    }
  }
  # Japan - missing 7
  {
    JPN_2014 <- data.frame(subset(m_FULL, (ISO3=="JPN") & (YEAR_WAVE==2014), select=c(WORRIED:RUNOUT, WHLDAY, wt)))   
    JPN_2015 <- data.frame(subset(m_FULL, (ISO3=="JPN") & (YEAR_WAVE==2015), select=c(WORRIED:RUNOUT, WHLDAY, wt)))   
    JPN_2016 <- data.frame(subset(m_FULL, (ISO3=="JPN") & (YEAR_WAVE==2016), select=c(WORRIED:RUNOUT, WHLDAY, wt)))   
    JPN_2017 <- data.frame(subset(m_FULL, (ISO3=="JPN") & (YEAR_WAVE==2017), select=c(WORRIED:RUNOUT, WHLDAY, wt)))   
    JPN_2018 <- data.frame(subset(m_FULL, (ISO3=="JPN") & (YEAR_WAVE==2018), select=c(WORRIED:RUNOUT, WHLDAY, wt)))   
    JPN_v <- c('JPN_2014', 'JPN_2015', 'JPN_2016', 'JPN_2017', 'JPN_2018')
    JPN_v <- data.frame(JPN_v)
    for (i in JPN_v$JPN_v) {
      tryCatch ( {
        temp_w <- subset(get(i), select = c(wt))
        temp <- subset(get(i), select = c(WORRIED:RUNOUT, WHLDAY))
        res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
        for (n in 1:7) {
          tryCatch ( {
            x <- res$b[[n]]
            assign(paste("b", n, sep=""),x) 
            y <- res$se.b[[n]]
            assign(paste("se.b", n, sep=""),y) 
          }, error = function(e){})
        }
        # Save Reliabilities 
        reliab <- res$reliab
        reliab.fl <- res$reliab.fl
        # Save Non-standardized Rasch scores 
        resC_stabD$Q1[resC_stabD$CY_key0 == i] <- b1
        resC_stabD$Q2[resC_stabD$CY_key0 == i] <- b2
        resC_stabD$Q3[resC_stabD$CY_key0 == i] <- b3
        resC_stabD$Q4[resC_stabD$CY_key0 == i] <- b4
        resC_stabD$Q5[resC_stabD$CY_key0 == i] <- b5
        resC_stabD$Q6[resC_stabD$CY_key0 == i] <- b6
        resC_stabD$Q8[resC_stabD$CY_key0 == i] <- b7
        # Standard errors on coefficients
        resC_stabD$q1_se.b[resC_stabD$CY_key0 == i] <- se.b1
        resC_stabD$q2_se.b[resC_stabD$CY_key0 == i] <- se.b2
        resC_stabD$q3_se.b[resC_stabD$CY_key0 == i] <- se.b3
        resC_stabD$q4_se.b[resC_stabD$CY_key0 == i] <- se.b4
        resC_stabD$q5_se.b[resC_stabD$CY_key0 == i] <- se.b5
        resC_stabD$q6_se.b[resC_stabD$CY_key0 == i] <- se.b6
        resC_stabD$q8_se.b[resC_stabD$CY_key0 == i] <- se.b7
        # Share Reliabilities
        resC_stabD$reliab[resC_stabD$CY_key0 == i] <- reliab
        resC_stabD$reliab.fl[resC_stabD$CY_key0 == i] <- reliab.fl
        # Infits
        resC_stabD$I_q1[resC_stabD$CY_key0 == i] <- infit1
        resC_stabD$I_q2[resC_stabD$CY_key0 == i] <- infit2
        resC_stabD$I_q3[resC_stabD$CY_key0 == i] <- infit3
        resC_stabD$I_q4[resC_stabD$CY_key0 == i] <- infit4
        resC_stabD$I_q5[resC_stabD$CY_key0 == i] <- infit5
        resC_stabD$I_q6[resC_stabD$CY_key0 == i] <- infit6
        resC_stabD$I_q8[resC_stabD$CY_key0 == i] <- infit7
        # Outfits
        resC_stabD$O_q1[resC_stabD$CY_key0 == i] <- outfit1 
        resC_stabD$O_q2[resC_stabD$CY_key0 == i] <- outfit2
        resC_stabD$O_q3[resC_stabD$CY_key0 == i] <- outfit3
        resC_stabD$O_q4[resC_stabD$CY_key0 == i] <- outfit4
        resC_stabD$O_q5[resC_stabD$CY_key0 == i] <- outfit5 
        resC_stabD$O_q6[resC_stabD$CY_key0 == i] <- outfit6 
        resC_stabD$O_q8[resC_stabD$CY_key0 == i] <- outfit7
      }, error = function(e){})
    }
  }
  # Malta - missing 1
  {
    MLT_2014 <- data.frame(subset(m_FULL, (ISO3=="MLT") & (YEAR_WAVE==2014), select=c(HEALTHY:WHLDAY, wt)))   
    MLT_2015 <- data.frame(subset(m_FULL, (ISO3=="MLT") & (YEAR_WAVE==2015), select=c(HEALTHY:WHLDAY, wt)))   
    MLT_2016 <- data.frame(subset(m_FULL, (ISO3=="MLT") & (YEAR_WAVE==2016), select=c(HEALTHY:WHLDAY, wt)))   
    MLT_2017 <- data.frame(subset(m_FULL, (ISO3=="MLT") & (YEAR_WAVE==2017), select=c(HEALTHY:WHLDAY, wt)))   
    MLT_2018 <- data.frame(subset(m_FULL, (ISO3=="MLT") & (YEAR_WAVE==2018), select=c(HEALTHY:WHLDAY, wt)))   
    MLT_v <- c('MLT_2014', 'MLT_2015', 'MLT_2016', 'MLT_2017', 'MLT_2018')
    MLT_v <- data.frame(MLT_v)
    for (i in MLT_v$MLT_v) {
      tryCatch ( {
        temp_w <- subset(get(i), select = c(wt))
        temp <- subset(get(i), select = c(HEALTHY:WHLDAY))
        res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
        for (n in 1:7) {
          tryCatch ( {
            x <- res$b[[n]]
            assign(paste("b", n, sep=""),x) 
            y <- res$se.b[[n]]
            assign(paste("se.b", n, sep=""),y) 
          }, error = function(e){})
        }
        
        # Save Reliabilities 
        reliab <- res$reliab
        reliab.fl <- res$reliab.fl
        # Save Non-standardized Rasch scores 
        resC_stabD$Q2[resC_stabD$CY_key0 == i] <- b1
        resC_stabD$Q3[resC_stabD$CY_key0 == i] <- b2
        resC_stabD$Q4[resC_stabD$CY_key0 == i] <- b3
        resC_stabD$Q5[resC_stabD$CY_key0 == i] <- b4
        resC_stabD$Q6[resC_stabD$CY_key0 == i] <- b5
        resC_stabD$Q7[resC_stabD$CY_key0 == i] <- b6
        resC_stabD$Q8[resC_stabD$CY_key0 == i] <- b7
        # Standard errors on coefficients
        resC_stabD$q2_se.b[resC_stabD$CY_key0 == i] <- se.b1
        resC_stabD$q3_se.b[resC_stabD$CY_key0 == i] <- se.b2
        resC_stabD$q4_se.b[resC_stabD$CY_key0 == i] <- se.b3
        resC_stabD$q5_se.b[resC_stabD$CY_key0 == i] <- se.b4
        resC_stabD$q6_se.b[resC_stabD$CY_key0 == i] <- se.b5
        resC_stabD$q7_se.b[resC_stabD$CY_key0 == i] <- se.b6
        resC_stabD$q8_se.b[resC_stabD$CY_key0 == i] <- se.b7
        # Share Reliabilities
        resC_stabD$reliab[resC_stabD$CY_key0 == i] <- reliab
        resC_stabD$reliab.fl[resC_stabD$CY_key0 == i] <- reliab.fl
        # Infits
        resC_stabD$I_q2[resC_stabD$CY_key0 == i] <- infit1
        resC_stabD$I_q3[resC_stabD$CY_key0 == i] <- infit2
        resC_stabD$I_q4[resC_stabD$CY_key0 == i] <- infit3
        resC_stabD$I_q5[resC_stabD$CY_key0 == i] <- infit4
        resC_stabD$I_q6[resC_stabD$CY_key0 == i] <- infit5
        resC_stabD$I_q7[resC_stabD$CY_key0 == i] <- infit6
        resC_stabD$I_q8[resC_stabD$CY_key0 == i] <- infit7
        # Outfits
        resC_stabD$O_q2[resC_stabD$CY_key0 == i] <- outfit1
        resC_stabD$O_q3[resC_stabD$CY_key0 == i] <- outfit2
        resC_stabD$O_q4[resC_stabD$CY_key0 == i] <- outfit3
        resC_stabD$O_q5[resC_stabD$CY_key0 == i] <- outfit4 
        resC_stabD$O_q6[resC_stabD$CY_key0 == i] <- outfit5 
        resC_stabD$O_q7[resC_stabD$CY_key0 == i] <- outfit6 
        resC_stabD$O_q8[resC_stabD$CY_key0 == i] <- outfit7 
      }, error = function(e){})
    }
  }
  # Netherlands - missing 4,5,6
  {
    NLD_2014 <- data.frame(subset(m_FULL, (ISO3=="NLD") & (YEAR_WAVE==2014), select=c(WORRIED:FEWFOOD,HUNGRY,WHLDAY, wt))) 
    NLD_2015 <- data.frame(subset(m_FULL, (ISO3=="NLD") & (YEAR_WAVE==2015), select=c(WORRIED:FEWFOOD,HUNGRY,WHLDAY, wt))) 
    NLD_2016 <- data.frame(subset(m_FULL, (ISO3=="NLD") & (YEAR_WAVE==2016), select=c(WORRIED:FEWFOOD,HUNGRY,WHLDAY, wt))) 
    NLD_2017 <- data.frame(subset(m_FULL, (ISO3=="NLD") & (YEAR_WAVE==2017), select=c(WORRIED:FEWFOOD,HUNGRY,WHLDAY, wt))) 
    NLD_2018 <- data.frame(subset(m_FULL, (ISO3=="NLD") & (YEAR_WAVE==2018), select=c(WORRIED:FEWFOOD,HUNGRY,WHLDAY, wt))) 
    NLD_v <- c('NLD_2014', 'NLD_2015', 'NLD_2016', 'NLD_2017', 'NLD_2018')
    NLD_v <- data.frame(NLD_v)
    
    for (i in NLD_v$NLD_v) {
      tryCatch ( {
        temp_w <- subset(get(i), select = c(wt))
        temp <- subset(get(i), select = c(WORRIED:FEWFOOD,HUNGRY,WHLDAY))
        res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
        for (n in 1:5) {
          tryCatch ( {
            x <- res$b[[n]]
            assign(paste("b", n, sep=""),x) 
            y <- res$se.b[[n]]
            assign(paste("se.b", n, sep=""),y) 
          }, error = function(e){})
        }
        reliab <- res$reliab
        reliab.fl <- res$reliab.fl
        # Save Non-standardized Rasch scores 
        resC_stabD$Q1[resC_stabD$CY_key0 == i] <- b1
        resC_stabD$Q2[resC_stabD$CY_key0 == i] <- b2
        resC_stabD$Q3[resC_stabD$CY_key0 == i] <- b3
        resC_stabD$Q7[resC_stabD$CY_key0 == i] <- b4
        resC_stabD$Q8[resC_stabD$CY_key0 == i] <- b5
        # Standard errors on coefficients
        resC_stabD$q1_se.b[resC_stabD$CY_key0 == i] <- se.b1
        resC_stabD$q2_se.b[resC_stabD$CY_key0 == i] <- se.b2
        resC_stabD$q3_se.b[resC_stabD$CY_key0 == i] <- se.b3
        resC_stabD$q7_se.b[resC_stabD$CY_key0 == i] <- se.b4
        resC_stabD$q8_se.b[resC_stabD$CY_key0 == i] <- se.b5
        # Share Reliabilities
        resC_stabD$reliab[resC_stabD$CY_key0 == i] <- reliab
        resC_stabD$reliab.fl[resC_stabD$CY_key0 == i] <- reliab.fl
        # Infits
        resC_stabD$I_q1[resC_stabD$CY_key0 == i] <- infit1
        resC_stabD$I_q2[resC_stabD$CY_key0 == i] <- infit2
        resC_stabD$I_q3[resC_stabD$CY_key0 == i] <- infit3
        resC_stabD$I_q7[resC_stabD$CY_key0 == i] <- infit4
        resC_stabD$I_q8[resC_stabD$CY_key0 == i] <- infit5
        # Outfits
        resC_stabD$O_q1[resC_stabD$CY_key0 == i] <- outfit1 
        resC_stabD$O_q2[resC_stabD$CY_key0 == i] <- outfit2
        resC_stabD$O_q3[resC_stabD$CY_key0 == i] <- outfit3
        resC_stabD$O_q7[resC_stabD$CY_key0 == i] <- outfit7 
        resC_stabD$O_q8[resC_stabD$CY_key0 == i] <- outfit8 
      }, error = function(e){})
    }
  }
  # Clean up a bit
  rm(DNK_2014, JPN_2014, MLT_2014, NLD_2014,DNK_2015, JPN_2015, MLT_2015, NLD_2015,DNK_2016, JPN_2016, MLT_2016, NLD_2016,DNK_2017, JPN_2017, MLT_2017, NLD_2017,DNK_2018, JPN_2018, MLT_2018, NLD_2018)
  rm(DNK_v, JPN_v, MLT_v, NLD_v)
}

## STEP 10: Cleaning up resC_stabF file and scalars
{
  resC_stabD <- subset(resC_stabD, resC_stabD$Q8 != "",)
  rm(a1,a2,a3,a4,a5,a6,a7,a8,b1,b2,b3,b4,b5,b6,b7,b8,c1,c2,c3,c4,c5,c6,c7,c8,d1,d2,d3,d4,d5,d6,d7,d8,i)
  rm(se.b1,se.b2,se.b3,se.b4,se.b5,se.b6,se.b7,se.b8)
  rm(temp, temp_w, x, y, n)
}

## STEP 11:  (C-FULL) Creating table of standardized item coefficients (Baseline)
{
  # Creating a subset of only CML coefficients from Rasch
  scale_stabD <- subset(resC_stabD,select = c('WP5','YEAR_WAVE','CY_key0','Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8'))
  
  # Add standardized columns
  v1 <- data.frame(matrix(ncol = 8, nrow = nrow(resC_stabD)))
  colnames(v1) <- c('S1','S2','S3','S4','S5','S6','S7','S8')
  scale_stabD <- cbind(scale_stabD, v1)
  
  # Add tolerated columns
  v1 <- data.frame(matrix(ncol = 8, nrow = nrow(resC_stabD)))
  colnames(v1) <- c('T1','T2','T3','T4','T5','T6','T7','T8')
  scale_stabD <- cbind(scale_stabD, v1)
  
  
  # First round standardize scores by country
  row.names(scale_stabD) <- 1:nrow(scale_stabD)
  for (i in 1:nrow(resC_stabD)) { 
    n  <- (8 - rowSums( is.na( scale_stabD[i,4:11])))     # number of items, by C
    x  <- rowMeans(scale_stabD[i , c(4:11)], na.rm=TRUE)  # CHANGED/FIXED
    v  <- data.frame(matrix(ncol = 1, nrow = 8))
    colnames(v) <- c('D')
    v[1,1] <- (scale_stabD[i,4] - x)
    v[2,1] <- (scale_stabD[i,5] - x)
    v[3,1] <- (scale_stabD[i,6] - x)
    v[4,1] <- (scale_stabD[i,7] - x)
    v[5,1] <- (scale_stabD[i,8] - x)
    v[6,1] <- (scale_stabD[i,9] - x)
    v[7,1] <- (scale_stabD[i,10] - x)
    v[8,1] <- (scale_stabD[i,11] - x)
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
    scale_stabD[i,12] <- e1/sd
    scale_stabD[i,13] <- e2/sd
    scale_stabD[i,14] <- e3/sd
    scale_stabD[i,15] <- e4/sd
    scale_stabD[i,16] <- e5/sd
    scale_stabD[i,17] <- e6/sd
    scale_stabD[i,18] <- e7/sd
    scale_stabD[i,19] <- e8/sd
  }
  
  # Column medians in a data frame - PROVISIONAL GLOBAL 8-ITEM SCALE
  s <- data.frame(lapply(scale_stabD[,12:19], median, na.rm = TRUE))
  
  # Generate binaries on common items by specified tolerance levels
  # tol <- 0.4
  
  # Question 1 Tolerance 
  for (i in 1:nrow(resC_stabD)) {
    tryCatch ( {
      if(abs(scale_stabD[i,12]-s[[1]]) <= tol) {  # =1 if common
        scale_stabD[i,20] = 1
      } else {
        scale_stabD[i,20] = 0
      }
    }, error = function(e){})
  }
  # Question 2 Tolerance 
  for (i in 1:nrow(resC_stabD)) {
    tryCatch ( {
      if(abs(scale_stabD[i,13]-s[[2]]) <= tol) {  # =1 if common
        scale_stabD[i,21] = 1
      } else {
        scale_stabD[i,21] = 0
      }
    }, error = function(e){})
  }
  # Question 3 Tolerance 
  for (i in 1:nrow(resC_stabD)) {
    tryCatch ( {
      if(abs(scale_stabD[i,14]-s[[3]]) <= tol) {  # =1 if common
        scale_stabD[i,22] = 1
      } else {
        scale_stabD[i,22] = 0
      }
    }, error = function(e){})
  }
  # Question 4 Tolerance 
  for (i in 1:nrow(resC_stabD)) {
    tryCatch ( {
      if(abs(scale_stabD[i,15]-s[[4]]) <= tol) {  # =1 if common
        scale_stabD[i,23] = 1
      } else {
        scale_stabD[i,23] = 0
      }
    }, error = function(e){})
  }
  # Question 5 Tolerance 
  for (i in 1:nrow(resC_stabD)) {
    tryCatch ( {
      if(abs(scale_stabD[i,16]-s[[5]]) <= tol) {  # =1 if common
        scale_stabD[i,24] = 1
      } else {
        scale_stabD[i,24] = 0
      }
    }, error = function(e){})
  }
  # Question 6 Tolerance
  for (i in 1:nrow(resC_stabD)) {
    tryCatch ( {
      if(abs(scale_stabD[i,17]-s[[6]]) <= tol) {  # =1 if common
        scale_stabD[i,25] = 1
      } else {
        scale_stabD[i,25] = 0
      }
    }, error = function(e){})
  }
  # Question 7 Tolerance
  for (i in 1:nrow(resC_stabD)) {
    tryCatch ( {
      if(abs(scale_stabD[i,18]-s[[7]]) <= tol) {  # =1 if common
        scale_stabD[i,26] = 1
      } else {
        scale_stabD[i,26] = 0
      }
    }, error = function(e){})
  }
  # Question 8 Tolerance
  for (i in 1:nrow(resC_stabD)) {
    tryCatch ( {
      if(abs(scale_stabD[i,19]-s[[8]]) <= tol) {  # =1 if common
        scale_stabD[i,27] = 1
      } else {
        scale_stabD[i,27] = 0
      }
    }, error = function(e){})
  }
  
  # Column counting common items per country
  v1 <- data.frame(matrix(ncol = 1, nrow = nrow(resC_stabD)))
  colnames(v1) <- c('check')
  scale_stabD <- cbind(scale_stabD, v1)
  
  # Sum number of unique items per country
  for (i in 1:nrow(resC_stabD)) {
    scale_stabD$check[i] <- rowSums(scale_stabD[i,20:27],na.rm=TRUE)
  }
  # Tabulate number of unique items per country (as in Tab. 2: Cafiero et al 2018)
  table(scale_stabD$check) # for tabulation of common items for a given tolerance level
  scale_stabD <- scale_stabD[c(1:28)] # drops superfluous "check" columns
} 

## STEP 12: (C-FULL) "WHILE" so updates create no unique items (Iterative step)
{
  # Conditional values to be updated:
  common_n <- sum(scale_stabD$check, na.rm=TRUE) # total number common items
  NA_n     <- sum(is.na(scale_stabD$Q1))+sum(is.na(scale_stabD$Q2))+sum(is.na(scale_stabD$Q3))+sum(is.na(scale_stabD$Q4))+sum(is.na(scale_stabD$Q5))+sum(is.na(scale_stabD$Q6))+sum(is.na(scale_stabD$Q7))+sum(is.na(scale_stabD$Q8)) 
  unique_n <- (8*nrow(resC_stabD)) - NA_n - common_n  #total number unique items
  
  # WHILE there is a positive number of unique items in the data
  while (0 < unique_n) {
    {
      # Set unique items to "NA" to for iterative re-standardization
      {
        # Question 1
        for (i in 1:nrow(resC_stabD)) {
          tryCatch ( {
            if(scale_stabD[i,20] == 0) {  
              scale_stabD[i,4] = NA
            } 
          }, error = function(e){})
        }
        # Question 2
        for (i in 1:nrow(resC_stabD)) {
          tryCatch ( {
            if(scale_stabD[i,21] == 0) {  
              scale_stabD[i,5] = NA
            } 
          }, error = function(e){})
        }
        # Question 3
        for (i in 1:nrow(resC_stabD)) {
          tryCatch ( {
            if(scale_stabD[i,22] == 0) {  
              scale_stabD[i,6] = NA
            } 
          }, error = function(e){})
        }
        # Question 4
        for (i in 1:nrow(resC_stabD)) {
          tryCatch ( {
            if(scale_stabD[i,23] == 0) {  
              scale_stabD[i,7] = NA
            } 
          }, error = function(e){})
        }
        # Question 5
        for (i in 1:nrow(resC_stabD)) {
          tryCatch ( {
            if(scale_stabD[i,24] == 0) {  
              scale_stabD[i,8] = NA
            } 
          }, error = function(e){})
        }
        # Question 6
        for (i in 1:nrow(resC_stabD)) {
          tryCatch ( {
            if(scale_stabD[i,25] == 0) {  
              scale_stabD[i,9] = NA
            } 
          }, error = function(e){})
        }
        # Question 7
        for (i in 1:nrow(resC_stabD)) {
          tryCatch ( {
            if(scale_stabD[i,26] == 0) {  
              scale_stabD[i,10] = NA
            } 
          }, error = function(e){})
        }
        # Question 8
        for (i in 1:nrow(resC_stabD)) {
          tryCatch ( {
            if(scale_stabD[i,27] == 0) {  
              scale_stabD[i,11] = NA
            } 
          }, error = function(e){})
        }
      }
      # Iterative step standardize scores by country
      for (i in 1:nrow(resC_stabD)) {
        n  <- (8 - rowSums(is.na( scale_stabD[i,4:11]))) # sums # of NA's in items
        x  <- rowMeans(scale_stabD[i , c(4:11)], na.rm=TRUE) 
        v <- data.frame(matrix(ncol = 1, nrow = 8)) # creates differences vector, 8x1
        colnames(v) <- c('D') # renames column vector 
        v[1,1] <- (scale_stabD[i,4] - x) # replacing values in the differences vector
        v[2,1] <- (scale_stabD[i,5] - x)
        v[3,1] <- (scale_stabD[i,6] - x)
        v[4,1] <- (scale_stabD[i,7] - x)
        v[5,1] <- (scale_stabD[i,8] - x)
        v[6,1] <- (scale_stabD[i,9] - x)
        v[7,1] <- (scale_stabD[i,10] - x)
        v[8,1] <- (scale_stabD[i,11] - x)  
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
        scale_stabD[i,12] <- e1/sd
        scale_stabD[i,13] <- e2/sd
        scale_stabD[i,14] <- e3/sd
        scale_stabD[i,15] <- e4/sd
        scale_stabD[i,16] <- e5/sd
        scale_stabD[i,17] <- e6/sd
        scale_stabD[i,18] <- e7/sd
        scale_stabD[i,19] <- e8/sd
      }    
      # Create a table of medians
      s <- data.frame(lapply(scale_stabD[,12:19], median, na.rm = TRUE))
      # Tabulate by tolerance
      tol <- 0.4 # Tolerance level where =1 if "common" (higher number is better)
      # Question 1 Tolerance
      for (i in 1:nrow(resC_stabD)) {
        tryCatch ( {
          if(abs(scale_stabD[i,12]-s[[1]]) <= tol) {  # =1 if common
            scale_stabD[i,20] = 1
          } else {
            scale_stabD[i,20] = 0
          }
        }, error = function(e){})
      }  
      # Question 2 Tolerance
      for (i in 1:nrow(resC_stabD)) {
        tryCatch ( {
          if(abs(scale_stabD[i,13]-s[[2]]) <= tol) {  # =1 if common
            scale_stabD[i,21] = 1
          } else {
            scale_stabD[i,21] = 0
          }
        }, error = function(e){})
      }  
      # Question 3 Tolerance
      for (i in 1:nrow(resC_stabD)) {
        tryCatch ( {
          if(abs(scale_stabD[i,14]-s[[3]]) <= tol) {  # =1 if common
            scale_stabD[i,22] = 1
          } else {
            scale_stabD[i,22] = 0
          }
        }, error = function(e){})
      }  
      # Question 4 Tolerance
      for (i in 1:nrow(resC_stabD)) {
        tryCatch ( {
          if(abs(scale_stabD[i,15]-s[[4]]) <= tol) {  # =1 if common
            scale_stabD[i,23] = 1
          } else {
            scale_stabD[i,23] = 0
          }
        }, error = function(e){})
      }  
      # Question 5 Tolerance
      for (i in 1:nrow(resC_stabD)) {
        tryCatch ( {
          if(abs(scale_stabD[i,16]-s[[5]]) <= tol) {  # =1 if common
            scale_stabD[i,24] = 1
          } else {
            scale_stabD[i,24] = 0
          }
        }, error = function(e){})
      }  
      # Question 6 Tolerance
      for (i in 1:nrow(resC_stabD)) {
        tryCatch ( {
          if(abs(scale_stabD[i,17]-s[[6]]) <= tol) {  # =1 if common
            scale_stabD[i,25] = 1
          } else {
            scale_stabD[i,25] = 0
          }
        }, error = function(e){})
      }  
      # Question 7 Tolerance
      for (i in 1:nrow(resC_stabD)) {
        tryCatch ( {
          if(abs(scale_stabD[i,18]-s[[7]]) <= tol) {  # =1 if common
            scale_stabD[i,26] = 1
          } else {
            scale_stabD[i,26] = 0
          }
        }, error = function(e){})
      }  
      # Question 8 Tolerance
      for (i in 1:nrow(resC_stabD)) {
        tryCatch ( {
          if(abs(scale_stabD[i,19]-s[[8]]) <= tol) {  # =1 if common
            scale_stabD[i,27] = 1
          } else {
            scale_stabD[i,27] = 0
          }
        }, error = function(e){})
      } 
      # Replace T_i to "NA" when standardized values are equal to "NA"
      # Question 1
      for (i in 1:nrow(resC_stabD)) {
        tryCatch ( {
          if(is.na(scale_stabD[i,12])) {
            scale_stabD[i,20] = NA
          } 
        }, error = function(e){})
      }
      # Question 2
      for (i in 1:nrow(resC_stabD)) {
        tryCatch ( {
          if(is.na(scale_stabD[i,13])) {
            scale_stabD[i,21] = NA
          } 
        }, error = function(e){})
      }
      # Question 3
      for (i in 1:nrow(resC_stabD)) {
        tryCatch ( {
          if(is.na(scale_stabD[i,14])) {
            scale_stabD[i,22] = NA
          } 
        }, error = function(e){})
      }
      # Question 4
      for (i in 1:nrow(resC_stabD)) {
        tryCatch ( {
          if(is.na(scale_stabD[i,15])) {
            scale_stabD[i,23] = NA
          } 
        }, error = function(e){})
      }
      # Question 5
      for (i in 1:nrow(resC_stabD)) {
        tryCatch ( {
          if(is.na(scale_stabD[i,16])) {
            scale_stabD[i,24] = NA
          } 
        }, error = function(e){})
      }    
      # Question 6
      for (i in 1:nrow(resC_stabD)) {
        tryCatch ( {
          if(is.na(scale_stabD[i,17])) {
            scale_stabD[i,25] = NA
          } 
        }, error = function(e){})
      }    
      # Question 7
      for (i in 1:nrow(resC_stabD)) {
        tryCatch ( {
          if(is.na(scale_stabD[i,18])) {
            scale_stabD[i,26] = NA
          } 
        }, error = function(e){})
      }    
      # Question 8
      for (i in 1:nrow(resC_stabD)) {
        tryCatch ( {
          if(is.na(scale_stabD[i,19])) {
            scale_stabD[i,27] = NA
          } 
        }, error = function(e){})
      }    
      # Sum number of common items per country
      for (i in 1:nrow(resC_stabD)) {
        scale_stabD$check[i] <- rowSums(scale_stabD[i,20:27],na.rm=TRUE)
      }
      # Fixing NaN values
      scale_stabD$S1[is.nan(scale_stabD$S1)]<-NA
      scale_stabD$S2[is.nan(scale_stabD$S2)]<-NA
      scale_stabD$S3[is.nan(scale_stabD$S3)]<-NA
      scale_stabD$S4[is.nan(scale_stabD$S4)]<-NA
      scale_stabD$S5[is.nan(scale_stabD$S5)]<-NA
      scale_stabD$S6[is.nan(scale_stabD$S6)]<-NA
      scale_stabD$S7[is.nan(scale_stabD$S7)]<-NA
      scale_stabD$S8[is.nan(scale_stabD$S8)]<-NA    
      # Safe-guard
      A <- scale_stabD                                   # safe-guard
      # WHILE CONDITIONS UPDATE
      common_n <- sum(scale_stabD$check, na.rm=TRUE) # total number common items
      NA_n     <- sum(is.na(scale_stabD$Q1))+sum(is.na(scale_stabD$Q2))+sum(is.na(scale_stabD$Q3))+sum(is.na(scale_stabD$Q4))+sum(is.na(scale_stabD$Q5))+sum(is.na(scale_stabD$Q6))+sum(is.na(scale_stabD$Q7))+sum(is.na(scale_stabD$Q8)) 
      unique_n <- sum(scale_stabD$T1 == 0, na.rm=TRUE)+sum(scale_stabD$T2 == 0, na.rm=TRUE)+sum(scale_stabD$T3 == 0, na.rm=TRUE)+sum(scale_stabD$T4 == 0, na.rm=TRUE)+sum(scale_stabD$T5 == 0, na.rm=TRUE)+sum(scale_stabD$T6 == 0, na.rm=TRUE)+sum(scale_stabD$T7 == 0, na.rm=TRUE)+sum(scale_stabD$T8 == 0, na.rm=TRUE)
      
    }
  }
}

## STEP 13: Printing Scale and Cleaning up
{
  # Dropping countries with less than four common items
  for (i in 1:nrow(resC_stabD)) {
    tryCatch ( {
      if(scale_stabD[i,28] < 4) {  
        scale_stabD[i,12] = NA
        scale_stabD[i,13] = NA
        scale_stabD[i,14] = NA
        scale_stabD[i,15] = NA
        scale_stabD[i,16] = NA
        scale_stabD[i,17] = NA
        scale_stabD[i,18] = NA
        scale_stabD[i,19] = NA      
      } 
    }, error = function(e){})
  } 
  # The completed global scale
  b <- data.frame(lapply(scale_stabD[,12:19], median, na.rm = TRUE))
}

## STEP 14: Equating for all countries to global scale
{
  # Create a place to save results
  equate_stabD <- subset(resC_stabD,select = c('WP5','YEAR_WAVE','CY_key0','Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8'))
  v1 <- data.frame(matrix(ncol = 23, nrow = nrow(resC_stabD)))
  colnames(v1) <- c('T1','T2','T3','T4','T5','T6','T7','T8','scale','shift','common1','common2','common3','common4','common5','common6','common7','common8','prev_FImod+','prev_FIsev','corr_comm','adj_FImod+','adj_FIsev')
  equate_stabD <- cbind(equate_stabD, v1)
  # Run loop over Rasch model then extract equated values to the data frame
  b <- as.numeric(b) # to make the global loop accessible by FAO's e.e function
  for (i in scale_stabD$CY_key0) {
    tryCatch ( {
      temp_w <- subset(get(i), select = c(wt))
      temp <- subset(get(i), select = c(WORRIED:WHLDAY))
      res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
      ee1 = equating.fun(res, st = b, tol = 0.4, spec.com1 = c(1:8), spec.com2 = c(1:8)) 
      # Extracting values
      n <- which(equate_stabD$ISO3 == i)
      equate_stabD[n,20] <- ee1$scale
      equate_stabD[n,21] <- ee1$shift
      equate_stabD[n,22] <- ee1$common[1]
      equate_stabD[n,23] <- ee1$common[2]
      equate_stabD[n,24] <- ee1$common[3]
      equate_stabD[n,25] <- ee1$common[4]
      equate_stabD[n,26] <- ee1$common[5]
      equate_stabD[n,27] <- ee1$common[6]
      equate_stabD[n,28] <- ee1$common[7]
      equate_stabD[n,29] <- ee1$common[8]
      equate_stabD[n,30] <- ee1$prevs[1]
      equate_stabD[n,31] <- ee1$prevs[2]
      equate_stabD[n,32] <- ee1$cor.comm.items
      equate_stabD[n,33] <- ee1$adj.thres[1]
      equate_stabD[n,34] <- ee1$adj.thres[2]
    }, error = function(e){})
  }
  # Fill in questions with "NA" if deemed unique by e.e function
  equate_stabD$Q1[equate_stabD$common1 == "Unique"] <- NA
  equate_stabD$Q2[equate_stabD$common2 == "Unique"] <- NA
  equate_stabD$Q3[equate_stabD$common3 == "Unique"] <- NA
  equate_stabD$Q4[equate_stabD$common4 == "Unique"] <- NA
  equate_stabD$Q5[equate_stabD$common5 == "Unique"] <- NA
  equate_stabD$Q6[equate_stabD$common6 == "Unique"] <- NA
  equate_stabD$Q7[equate_stabD$common7 == "Unique"] <- NA
  equate_stabD$Q8[equate_stabD$common8 == "Unique"] <- NA
  names(equate_stabD)[names(equate_stabD) == 'Freq'] <- 'cond'   # OPTION: if needed
  
  # Global and country scale characteristics toward adjustment
  for (i in 1:nrow(resC_stabD)) {
    s1 <- s
    s1$S1[equate_stabD[i,22] == "Unique"] <- NA
    s1$S2[equate_stabD[i,23] == "Unique"] <- NA
    s1$S3[equate_stabD[i,24] == "Unique"] <- NA
    s1$S4[equate_stabD[i,25] == "Unique"] <- NA
    s1$S5[equate_stabD[i,26] == "Unique"] <- NA
    s1$S6[equate_stabD[i,27] == "Unique"] <- NA
    s1$S7[equate_stabD[i,28] == "Unique"] <- NA
    s1$S8[equate_stabD[i,29] == "Unique"] <- NA
    mean_g <- rowMeans(s1, na.rm=TRUE)                        # 1.2213 test
    sd_g   <- apply(s1, 1, sd,na.rm=TRUE)                     # -0.165 test
    # Country scale characteristics                             
    mean_c <- rowMeans(equate_stabD[i,c(4:11)], na.rm = TRUE)  # -0.1393
    sd_c <- apply(equate_stabD[i,c(4:11)], 1, sd, na.rm=TRUE)  # 1.33817
    # Fill the things back in
    equate_stabD[i,4][is.na(equate_stabD[i,4])] <- resC_stabD[i,4]
    equate_stabD[i,5][is.na(equate_stabD[i,5])] <- resC_stabD[i,5]
    equate_stabD[i,6][is.na(equate_stabD[i,6])] <- resC_stabD[i,6]
    equate_stabD[i,7][is.na(equate_stabD[i,7])] <- resC_stabD[i,7]
    equate_stabD[i,8][is.na(equate_stabD[i,8])] <- resC_stabD[i,8]
    equate_stabD[i,9][is.na(equate_stabD[i,9])] <- resC_stabD[i,9]
    equate_stabD[i,10][is.na(equate_stabD[i,10])] <- resC_stabD[i,10]
    equate_stabD[i,11][is.na(equate_stabD[i,11])] <- resC_stabD[i,11]
    # SHIFT THE THINGS
    equate_stabD[i,12] <- (equate_stabD[i,4] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
    equate_stabD[i,13] <- (equate_stabD[i,5] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
    equate_stabD[i,14] <- (equate_stabD[i,6] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
    equate_stabD[i,15] <- (equate_stabD[i,7] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
    equate_stabD[i,16] <- (equate_stabD[i,8] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
    equate_stabD[i,17] <- (equate_stabD[i,9] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
    equate_stabD[i,18] <- (equate_stabD[i,10] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
    equate_stabD[i,19] <- (equate_stabD[i,11] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
  }
}

