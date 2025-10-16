############################################
## Loop over Country-Years for RM.weights ## -- COVER 6
############################################
## Clear everything
rm(list=ls())

## STEPS: 
# 1.      Load relevant packages
# 2.      Load relevant data
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
############################################################
## STEP 1: Preliminary packages
{
library(RM.weights)
library(haven)      
library(expss)
library(ggplot2)
library(dplyr)
}

## STEP 2: Uploading data & Dropping Turkmenistan
{
  # Data
  setwd("C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code")
  FULL = read.csv("cover6.csv")
  ISO3 = read.csv("ISO3.csv")
  # Drop Turkmenistan
  FULL = FULL[FULL$WP5 != "Turkmenistan", ]
}

## STEP 3: ISO3 Merge file and clean it
{
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
}

## STEP 4: Create subset keys for to (C) and (C-Y) sets
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

## STEP 5: Creating the subsets (C)
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

## STEP 6: Creating and saving the data frame for parameter storage
{
# (C-cover6) Create data frame in which to store RM results
resC_cover6 <- data.frame(matrix(ncol = 34, nrow = nrow(c1_ISO3)))
colnames(resC_cover6) <- c('Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8','q1_se.b','q2_se.b','q3_se.b','q4_se.b','q5_se.b','q6_se.b','q7_se.b','q8_se.b','reliab','reliab.fl','O_q1','O_q2','O_q3','O_q4','O_q5','O_q6','O_q7','O_q8','I_q1','I_q2','I_q3','I_q4','I_q5','I_q6','I_q7','I_q8')
v1 <- data.frame(matrix(ncol=2,nrow=nrow(c)))
v1 <- data.frame(c$Var1,c$Freq)
colnames(v1) <- c('WP5','Freq')
v1 <- merge(c1_ISO3,v1,by="WP5")
resC_cover6 <- merge(v1,resC_cover6)
resC_cover6 <- resC_cover6[!duplicated(resC_cover6$WP5),]
}

## STEP 7: (C-cover6) Estimate Rasch model and extract values to data frame
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
    resC_cover6$Q1[resC_cover6$ISO3 == i] <- b1
    resC_cover6$Q2[resC_cover6$ISO3 == i] <- b2
    resC_cover6$Q3[resC_cover6$ISO3 == i] <- b3
    resC_cover6$Q4[resC_cover6$ISO3 == i] <- b4
    resC_cover6$Q5[resC_cover6$ISO3 == i] <- b5
    resC_cover6$Q6[resC_cover6$ISO3 == i] <- b6
    resC_cover6$Q7[resC_cover6$ISO3 == i] <- b7
    resC_cover6$Q8[resC_cover6$ISO3 == i] <- b8
    # Standard errors on coefficients
    resC_cover6$q1_se.b[resC_cover6$ISO3 == i] <- se.b1
    resC_cover6$q2_se.b[resC_cover6$ISO3 == i] <- se.b2
    resC_cover6$q3_se.b[resC_cover6$ISO3 == i] <- se.b3
    resC_cover6$q4_se.b[resC_cover6$ISO3 == i] <- se.b4
    resC_cover6$q5_se.b[resC_cover6$ISO3 == i] <- se.b5
    resC_cover6$q6_se.b[resC_cover6$ISO3 == i] <- se.b6
    resC_cover6$q7_se.b[resC_cover6$ISO3 == i] <- se.b7
    resC_cover6$q8_se.b[resC_cover6$ISO3 == i] <- se.b8
    # Reliabilities
    resC_cover6$reliab[resC_cover6$ISO3 == i] <- reliab
    resC_cover6$reliab.fl[resC_cover6$ISO3 == i] <- reliab.fl
    # Infits
    resC_cover6$I_q1[resC_cover6$ISO3 == i] <- infit1
    resC_cover6$I_q2[resC_cover6$ISO3 == i] <- infit2
    resC_cover6$I_q3[resC_cover6$ISO3 == i] <- infit3
    resC_cover6$I_q4[resC_cover6$ISO3 == i] <- infit4
    resC_cover6$I_q5[resC_cover6$ISO3 == i] <- infit5
    resC_cover6$I_q6[resC_cover6$ISO3 == i] <- infit6
    resC_cover6$I_q7[resC_cover6$ISO3 == i] <- infit7
    resC_cover6$I_q8[resC_cover6$ISO3 == i] <- infit8
    # Outfits
    resC_cover6$O_q1[resC_cover6$ISO3 == i] <- outfit1 
    resC_cover6$O_q2[resC_cover6$ISO3 == i] <- outfit2
    resC_cover6$O_q3[resC_cover6$ISO3 == i] <- outfit3
    resC_cover6$O_q4[resC_cover6$ISO3 == i] <- outfit4
    resC_cover6$O_q5[resC_cover6$ISO3 == i] <- outfit5 
    resC_cover6$O_q6[resC_cover6$ISO3 == i] <- outfit6 
    resC_cover6$O_q7[resC_cover6$ISO3 == i] <- outfit7 
    resC_cover6$O_q8[resC_cover6$ISO3 == i] <- outfit8 
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

## STEP 8: (C-cover6) Correcting for missing questions
{
# Countries skipping a question
resC_fix <- subset(resC_cover6, is.na(resC_cover6$Q1))
cou_fix <- data.frame(resC_fix$ISO3)
}

## STEP 9:  (C-cover6) Creating table of standardized item coefficients (Baseline)
{
  # Creating a subset of only CML coefficients from Rasch
  scale_cover6 <- subset(resC_cover6,select = c('WP5','ISO3','Freq','Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8'))
  # Add standardized columns
  v1 <- data.frame(matrix(ncol = 8, nrow = nrow(c1_ISO3)))
  colnames(v1) <- c('S1','S2','S3','S4','S5','S6','S7','S8')
  scale_cover6 <- cbind(scale_cover6, v1)
  # Add tolerated columns
  v1 <- data.frame(matrix(ncol = 8, nrow = nrow(c1_ISO3)))
  colnames(v1) <- c('T1','T2','T3','T4','T5','T6','T7','T8')
  scale_cover6 <- cbind(scale_cover6, v1)
  # First round standardize scores by country
  for (i in 1:length(c1$ISO3)) { 
    n  <- (8 - rowSums( is.na( scale_cover6[i,4:11])))     # number of items, by C
    x  <- rowMeans(scale_cover6[i , c(4:11)], na.rm=TRUE)  # CHANGED/FIXED
    v  <- data.frame(matrix(ncol = 1, nrow = 8))
    colnames(v) <- c('D')
    v[1,1] <- (scale_cover6[i,4] - x)
    v[2,1] <- (scale_cover6[i,5] - x)
    v[3,1] <- (scale_cover6[i,6] - x)
    v[4,1] <- (scale_cover6[i,7] - x)
    v[5,1] <- (scale_cover6[i,8] - x)
    v[6,1] <- (scale_cover6[i,9] - x)
    v[7,1] <- (scale_cover6[i,10] - x)
    v[8,1] <- (scale_cover6[i,11] - x)
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
    scale_cover6[i,12] <- e1/sd
    scale_cover6[i,13] <- e2/sd
    scale_cover6[i,14] <- e3/sd
    scale_cover6[i,15] <- e4/sd
    scale_cover6[i,16] <- e5/sd
    scale_cover6[i,17] <- e6/sd
    scale_cover6[i,18] <- e7/sd
    scale_cover6[i,19] <- e8/sd
  }
  # Column medians in a data frame - PROVISIONAL GLOBAL 8-ITEM SCALE
  s <- data.frame(lapply(scale_cover6[,12:19], median, na.rm = TRUE))
  # Generate binaries on common items by specified tolerance levels 
  tol <- 0.4 # Tolerance level where =1 if "common" (higher number is better)
  # Question 1 Tolerance 
  for (i in 1:length(c1$ISO3)) {
    tryCatch ( {
      if(abs(scale_cover6[i,12]-s[[1]]) <= tol) {  # =1 if common
        scale_cover6[i,20] = 1
      } else {
        scale_cover6[i,20] = 0
      }
    }, error = function(e){})
  }
  # Question 2 Tolerance 
  for (i in 1:length(c1$ISO3)) {
    tryCatch ( {
      if(abs(scale_cover6[i,13]-s[[2]]) <= tol) {  # =1 if common
        scale_cover6[i,21] = 1
      } else {
        scale_cover6[i,21] = 0
      }
    }, error = function(e){})
  }
  # Question 3 Tolerance 
  for (i in 1:length(c1$ISO3)) {
    tryCatch ( {
      if(abs(scale_cover6[i,14]-s[[3]]) <= tol) {  # =1 if common
        scale_cover6[i,22] = 1
      } else {
        scale_cover6[i,22] = 0
      }
    }, error = function(e){})
  }
  # Question 4 Tolerance 
  for (i in 1:length(c1$ISO3)) {
    tryCatch ( {
      if(abs(scale_cover6[i,15]-s[[4]]) <= tol) {  # =1 if common
        scale_cover6[i,23] = 1
      } else {
        scale_cover6[i,23] = 0
      }
    }, error = function(e){})
  }
  # Question 5 Tolerance 
  for (i in 1:length(c1$ISO3)) {
    tryCatch ( {
      if(abs(scale_cover6[i,16]-s[[5]]) <= tol) {  # =1 if common
        scale_cover6[i,24] = 1
      } else {
        scale_cover6[i,24] = 0
      }
    }, error = function(e){})
  }
  # Question 6 Tolerance
  for (i in 1:length(c1$ISO3)) {
    tryCatch ( {
      if(abs(scale_cover6[i,17]-s[[6]]) <= tol) {  # =1 if common
        scale_cover6[i,25] = 1
      } else {
        scale_cover6[i,25] = 0
      }
    }, error = function(e){})
  }
  # Question 7 Tolerance
  for (i in 1:length(c1$ISO3)) {
    tryCatch ( {
      if(abs(scale_cover6[i,18]-s[[7]]) <= tol) {  # =1 if common
        scale_cover6[i,26] = 1
      } else {
        scale_cover6[i,26] = 0
      }
    }, error = function(e){})
  }
  # Question 8 Tolerance
  for (i in 1:length(c1$ISO3)) {
    tryCatch ( {
      if(abs(scale_cover6[i,19]-s[[8]]) <= tol) {  # =1 if common
        scale_cover6[i,27] = 1
      } else {
        scale_cover6[i,27] = 0
      }
    }, error = function(e){})
  }
  # Column counting common items per country
  v1 <- data.frame(matrix(ncol = 1, nrow = nrow(c1_ISO3)))
  colnames(v1) <- c('check')
  scale_cover6 <- cbind(scale_cover6, v1)
  # Sum number of unique items per country
  for (i in 1:length(c1$ISO3)) {
    scale_cover6$check[i] <- rowSums(scale_cover6[i,20:27],na.rm=TRUE)
  }
  
  # Tabulate number of unique items per country (as in Tab. 2: Cafiero et al 2018)
  table(scale_cover6$check) # for tabulation of common items for a given tolerance level
  scale_cover6 <- scale_cover6[c(1:28)] # drops superfluous "check" columns
} 

## STEP 10: (C-cover6) "WHILE" so updates create no unique items (Iterative step)
{
  # Conditional values to be updated:
  common_n <- sum(scale_cover6$check, na.rm=TRUE) # total number common items
  NA_n     <- sum(is.na(scale_cover6$Q1))+sum(is.na(scale_cover6$Q2))+sum(is.na(scale_cover6$Q3))+sum(is.na(scale_cover6$Q4))+sum(is.na(scale_cover6$Q5))+sum(is.na(scale_cover6$Q6))+sum(is.na(scale_cover6$Q7))+sum(is.na(scale_cover6$Q8)) 
  unique_n <- (8*length(c1$ISO3)) - NA_n - common_n  #total number unique items
  
  # WHILE there is a positive number of unique items in the data
  while (0 < unique_n) {
    {
      # Set unique items to "NA" to for iterative re-standardization
      {
        # Question 1
        for (i in 1:length(c1$ISO3)) {
          tryCatch ( {
            if(scale_cover6[i,20] == 0) {  
              scale_cover6[i,4] = NA
            } 
          }, error = function(e){})
        }
        # Question 2
        for (i in 1:length(c1$ISO3)) {
          tryCatch ( {
            if(scale_cover6[i,21] == 0) {  
              scale_cover6[i,5] = NA
            } 
          }, error = function(e){})
        }
        # Question 3
        for (i in 1:length(c1$ISO3)) {
          tryCatch ( {
            if(scale_cover6[i,22] == 0) {  
              scale_cover6[i,6] = NA
            } 
          }, error = function(e){})
        }
        # Question 4
        for (i in 1:length(c1$ISO3)) {
          tryCatch ( {
            if(scale_cover6[i,23] == 0) {  
              scale_cover6[i,7] = NA
            } 
          }, error = function(e){})
        }
        # Question 5
        for (i in 1:length(c1$ISO3)) {
          tryCatch ( {
            if(scale_cover6[i,24] == 0) {  
              scale_cover6[i,8] = NA
            } 
          }, error = function(e){})
        }
        # Question 6
        for (i in 1:length(c1$ISO3)) {
          tryCatch ( {
            if(scale_cover6[i,25] == 0) {  
              scale_cover6[i,9] = NA
            } 
          }, error = function(e){})
        }
        # Question 7
        for (i in 1:length(c1$ISO3)) {
          tryCatch ( {
            if(scale_cover6[i,26] == 0) {  
              scale_cover6[i,10] = NA
            } 
          }, error = function(e){})
        }
        # Question 8
        for (i in 1:length(c1$ISO3)) {
          tryCatch ( {
            if(scale_cover6[i,27] == 0) {  
              scale_cover6[i,11] = NA
            } 
          }, error = function(e){})
        }
      }
      # Iterative step standardize scores by country
      for (i in 1:length(c1$ISO3)) {
        n  <- (8 - rowSums(is.na( scale_cover6[i,4:11]))) # sums # of NA's in items
        x  <- rowMeans(scale_cover6[i , c(4:11)], na.rm=TRUE) 
        v <- data.frame(matrix(ncol = 1, nrow = 8)) # creates differences vector, 8x1
        colnames(v) <- c('D') # renames column vector 
        v[1,1] <- (scale_cover6[i,4] - x) # replacing values in the differences vector
        v[2,1] <- (scale_cover6[i,5] - x)
        v[3,1] <- (scale_cover6[i,6] - x)
        v[4,1] <- (scale_cover6[i,7] - x)
        v[5,1] <- (scale_cover6[i,8] - x)
        v[6,1] <- (scale_cover6[i,9] - x)
        v[7,1] <- (scale_cover6[i,10] - x)
        v[8,1] <- (scale_cover6[i,11] - x)  
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
        scale_cover6[i,12] <- e1/sd
        scale_cover6[i,13] <- e2/sd
        scale_cover6[i,14] <- e3/sd
        scale_cover6[i,15] <- e4/sd
        scale_cover6[i,16] <- e5/sd
        scale_cover6[i,17] <- e6/sd
        scale_cover6[i,18] <- e7/sd
        scale_cover6[i,19] <- e8/sd
      }    
      # Create a table of medians
      s <- data.frame(lapply(scale_cover6[,12:19], median, na.rm = TRUE))
      # Tabulate by tolerance
      tol <- 0.4 # Tolerance level where =1 if "common" (higher number is better)
      # Question 1 Tolerance
      for (i in 1:length(c1$ISO3)) {
        tryCatch ( {
          if(abs(scale_cover6[i,12]-s[[1]]) <= tol) {  # =1 if common
            scale_cover6[i,20] = 1
          } else {
            scale_cover6[i,20] = 0
          }
        }, error = function(e){})
      }  
      # Question 2 Tolerance
      for (i in 1:length(c1$ISO3)) {
        tryCatch ( {
          if(abs(scale_cover6[i,13]-s[[2]]) <= tol) {  # =1 if common
            scale_cover6[i,21] = 1
          } else {
            scale_cover6[i,21] = 0
          }
        }, error = function(e){})
      }  
      # Question 3 Tolerance
      for (i in 1:length(c1$ISO3)) {
        tryCatch ( {
          if(abs(scale_cover6[i,14]-s[[3]]) <= tol) {  # =1 if common
            scale_cover6[i,22] = 1
          } else {
            scale_cover6[i,22] = 0
          }
        }, error = function(e){})
      }  
      # Question 4 Tolerance
      for (i in 1:length(c1$ISO3)) {
        tryCatch ( {
          if(abs(scale_cover6[i,15]-s[[4]]) <= tol) {  # =1 if common
            scale_cover6[i,23] = 1
          } else {
            scale_cover6[i,23] = 0
          }
        }, error = function(e){})
      }  
      # Question 5 Tolerance
      for (i in 1:length(c1$ISO3)) {
        tryCatch ( {
          if(abs(scale_cover6[i,16]-s[[5]]) <= tol) {  # =1 if common
            scale_cover6[i,24] = 1
          } else {
            scale_cover6[i,24] = 0
          }
        }, error = function(e){})
      }  
      # Question 6 Tolerance
      for (i in 1:length(c1$ISO3)) {
        tryCatch ( {
          if(abs(scale_cover6[i,17]-s[[6]]) <= tol) {  # =1 if common
            scale_cover6[i,25] = 1
          } else {
            scale_cover6[i,25] = 0
          }
        }, error = function(e){})
      }  
      # Question 7 Tolerance
      for (i in 1:length(c1$ISO3)) {
        tryCatch ( {
          if(abs(scale_cover6[i,18]-s[[7]]) <= tol) {  # =1 if common
            scale_cover6[i,26] = 1
          } else {
            scale_cover6[i,26] = 0
          }
        }, error = function(e){})
      }  
      # Question 8 Tolerance
      for (i in 1:length(c1$ISO3)) {
        tryCatch ( {
          if(abs(scale_cover6[i,19]-s[[8]]) <= tol) {  # =1 if common
            scale_cover6[i,27] = 1
          } else {
            scale_cover6[i,27] = 0
          }
        }, error = function(e){})
      } 
      # Replace T_i to "NA" when standardized values are equal to "NA"
      # Question 1
      for (i in 1:length(c1$ISO3)) {
        tryCatch ( {
          if(is.na(scale_cover6[i,12])) {
            scale_cover6[i,20] = NA
          } 
        }, error = function(e){})
      }
      # Question 2
      for (i in 1:length(c1$ISO3)) {
        tryCatch ( {
          if(is.na(scale_cover6[i,13])) {
            scale_cover6[i,21] = NA
          } 
        }, error = function(e){})
      }
      # Question 3
      for (i in 1:length(c1$ISO3)) {
        tryCatch ( {
          if(is.na(scale_cover6[i,14])) {
            scale_cover6[i,22] = NA
          } 
        }, error = function(e){})
      }
      # Question 4
      for (i in 1:length(c1$ISO3)) {
        tryCatch ( {
          if(is.na(scale_cover6[i,15])) {
            scale_cover6[i,23] = NA
          } 
        }, error = function(e){})
      }
      # Question 5
      for (i in 1:length(c1$ISO3)) {
        tryCatch ( {
          if(is.na(scale_cover6[i,16])) {
            scale_cover6[i,24] = NA
          } 
        }, error = function(e){})
      }    
      # Question 6
      for (i in 1:length(c1$ISO3)) {
        tryCatch ( {
          if(is.na(scale_cover6[i,17])) {
            scale_cover6[i,25] = NA
          } 
        }, error = function(e){})
      }    
      # Question 7
      for (i in 1:length(c1$ISO3)) {
        tryCatch ( {
          if(is.na(scale_cover6[i,18])) {
            scale_cover6[i,26] = NA
          } 
        }, error = function(e){})
      }    
      # Question 8
      for (i in 1:length(c1$ISO3)) {
        tryCatch ( {
          if(is.na(scale_cover6[i,19])) {
            scale_cover6[i,27] = NA
          } 
        }, error = function(e){})
      }    
      # Sum number of common items per country
      for (i in 1:length(c1$ISO3)) {
        scale_cover6$check[i] <- rowSums(scale_cover6[i,20:27],na.rm=TRUE)
      }
      # Fixing NaN values
      scale_cover6$S1[is.nan(scale_cover6$S1)]<-NA
      scale_cover6$S2[is.nan(scale_cover6$S2)]<-NA
      scale_cover6$S3[is.nan(scale_cover6$S3)]<-NA
      scale_cover6$S4[is.nan(scale_cover6$S4)]<-NA
      scale_cover6$S5[is.nan(scale_cover6$S5)]<-NA
      scale_cover6$S6[is.nan(scale_cover6$S6)]<-NA
      scale_cover6$S7[is.nan(scale_cover6$S7)]<-NA
      scale_cover6$S8[is.nan(scale_cover6$S8)]<-NA    
      # Safe-guard
      A <- scale_cover6                                   # safe-guard
      # WHILE CONDITIONS UPDATE
      common_n <- sum(scale_cover6$check, na.rm=TRUE) # total number common items
      NA_n     <- sum(is.na(scale_cover6$Q1))+sum(is.na(scale_cover6$Q2))+sum(is.na(scale_cover6$Q3))+sum(is.na(scale_cover6$Q4))+sum(is.na(scale_cover6$Q5))+sum(is.na(scale_cover6$Q6))+sum(is.na(scale_cover6$Q7))+sum(is.na(scale_cover6$Q8)) 
      unique_n <- sum(scale_cover6$T1 == 0, na.rm=TRUE)+sum(scale_cover6$T2 == 0, na.rm=TRUE)+sum(scale_cover6$T3 == 0, na.rm=TRUE)+sum(scale_cover6$T4 == 0, na.rm=TRUE)+sum(scale_cover6$T5 == 0, na.rm=TRUE)+sum(scale_cover6$T6 == 0, na.rm=TRUE)+sum(scale_cover6$T7 == 0, na.rm=TRUE)+sum(scale_cover6$T8 == 0, na.rm=TRUE)
      
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
  for (i in 1:length(c1$ISO3)) {
    tryCatch ( {
      if(scale_cover6[i,28] < 4) {  
        scale_cover6[i,12] = NA
        scale_cover6[i,13] = NA
        scale_cover6[i,14] = NA
        scale_cover6[i,15] = NA
        scale_cover6[i,16] = NA
        scale_cover6[i,17] = NA
        scale_cover6[i,18] = NA
        scale_cover6[i,19] = NA      
      } 
    }, error = function(e){})
  } 
  # The completed global scale
  b <- data.frame(lapply(scale_cover6[,12:19], median, na.rm = TRUE))
}

## STEP 12: Equating for all countries to global scale
{
  # Create a place to save results
  equate_cover6 <- subset(resC_cover6,select = c('WP5','ISO3','Freq','Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8'))
  v1 <- data.frame(matrix(ncol = 23, nrow = nrow(c1_ISO3)))
  colnames(v1) <- c('T1','T2','T3','T4','T5','T6','T7','T8','scale','shift','common1','common2','common3','common4','common5','common6','common7','common8','prev_FImod+','prev_FIsev','corr_comm','adj_FImod+','adj_FIsev')
  equate_cover6 <- cbind(equate_cover6, v1)
  # Run loop over Rasch model then extract equated values to the data frame
  b <- as.numeric(b) # to make the global loop accessible by FAO's e.e function
  for (i in c1$ISO3) {
    tryCatch ( {
      temp_w <- subset(get(i), select = c(wt))
      temp <- subset(get(i), select = c(WORRIED:WHLDAY))
      res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
      ee1 = equating.fun(res, st = b, tol = 0.4, spec.com1 = c(1:8), spec.com2 = c(1:8)) 
      # Extracting values
      n <- which(equate_cover6$ISO3 == i)
      equate_cover6[n,20] <- ee1$scale
      equate_cover6[n,21] <- ee1$shift
      equate_cover6[n,22] <- ee1$common[1]
      equate_cover6[n,23] <- ee1$common[2]
      equate_cover6[n,24] <- ee1$common[3]
      equate_cover6[n,25] <- ee1$common[4]
      equate_cover6[n,26] <- ee1$common[5]
      equate_cover6[n,27] <- ee1$common[6]
      equate_cover6[n,28] <- ee1$common[7]
      equate_cover6[n,29] <- ee1$common[8]
      equate_cover6[n,30] <- ee1$prevs[1]
      equate_cover6[n,31] <- ee1$prevs[2]
      equate_cover6[n,32] <- ee1$cor.comm.items
      equate_cover6[n,33] <- ee1$adj.thres[1]
      equate_cover6[n,34] <- ee1$adj.thres[2]
    }, error = function(e){})
  }
  # Fill in questions with "NA" if deemed unique by e.e function
  equate_cover6$Q1[equate_cover6$common1 == "Unique"] <- NA
  equate_cover6$Q2[equate_cover6$common2 == "Unique"] <- NA
  equate_cover6$Q3[equate_cover6$common3 == "Unique"] <- NA
  equate_cover6$Q4[equate_cover6$common4 == "Unique"] <- NA
  equate_cover6$Q5[equate_cover6$common5 == "Unique"] <- NA
  equate_cover6$Q6[equate_cover6$common6 == "Unique"] <- NA
  equate_cover6$Q7[equate_cover6$common7 == "Unique"] <- NA
  equate_cover6$Q8[equate_cover6$common8 == "Unique"] <- NA
  names(equate_cover6)[names(equate_cover6) == 'Freq'] <- 'cond'   # OPTION: if needed
  # Global and country scale characteristics toward adjustment
  
  for (i in 1:length(c1$ISO3)) {
    s1 <- s
    s1$S1[equate_cover6[i,22] == "Unique"] <- NA
    s1$S2[equate_cover6[i,23] == "Unique"] <- NA
    s1$S3[equate_cover6[i,24] == "Unique"] <- NA
    s1$S4[equate_cover6[i,25] == "Unique"] <- NA
    s1$S5[equate_cover6[i,26] == "Unique"] <- NA
    s1$S6[equate_cover6[i,27] == "Unique"] <- NA
    s1$S7[equate_cover6[i,28] == "Unique"] <- NA
    s1$S8[equate_cover6[i,29] == "Unique"] <- NA
    mean_g <- rowMeans(s1, na.rm=TRUE)                        # 1.2213 test
    sd_g   <- apply(s1, 1, sd,na.rm=TRUE)                     # -0.165 test
    # Country scale characteristics                             
    mean_c <- rowMeans(equate_cover6[i,c(4:11)], na.rm = TRUE)  # -0.1393
    sd_c <- apply(equate_cover6[i,c(4:11)], 1, sd, na.rm=TRUE)  # 1.33817
    # Fill the things back in
    equate_cover6[i,4][is.na(equate_cover6[i,4])] <- resC_cover6[i,4]
    equate_cover6[i,5][is.na(equate_cover6[i,5])] <- resC_cover6[i,5]
    equate_cover6[i,6][is.na(equate_cover6[i,6])] <- resC_cover6[i,6]
    equate_cover6[i,7][is.na(equate_cover6[i,7])] <- resC_cover6[i,7]
    equate_cover6[i,8][is.na(equate_cover6[i,8])] <- resC_cover6[i,8]
    equate_cover6[i,9][is.na(equate_cover6[i,9])] <- resC_cover6[i,9]
    equate_cover6[i,10][is.na(equate_cover6[i,10])] <- resC_cover6[i,10]
    equate_cover6[i,11][is.na(equate_cover6[i,11])] <- resC_cover6[i,11]
    # SHIFT THE THINGS
    equate_cover6[i,12] <- (equate_cover6[i,4] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
    equate_cover6[i,13] <- (equate_cover6[i,5] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
    equate_cover6[i,14] <- (equate_cover6[i,6] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
    equate_cover6[i,15] <- (equate_cover6[i,7] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
    equate_cover6[i,16] <- (equate_cover6[i,8] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
    equate_cover6[i,17] <- (equate_cover6[i,9] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
    equate_cover6[i,18] <- (equate_cover6[i,10] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
    equate_cover6[i,19] <- (equate_cover6[i,11] - (mean_c - mean_g*(sd_c/sd_g)))*(sd_g/sd_c)
  }
}

## FINAL: Output a table of translated values
{
  # Full set
  equated_cover6 <- subset(equate_cover6,select = c('WP5','ISO3','T1','T2','T3','T4','T5','T6','T7','T8','corr_comm'))
  View(equated_cover6)
  
  # Relevant CSVs
  # CSV for 1.b
    write.csv(resC_cover6, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\resC_cover6_1b.csv")
    write.csv(scale_cover6, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\scale_cover6_1b.csv")
    write.csv(equated_cover6, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\equated_cover6_1b.csv")
    write.csv(s, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\global_scale_cover6_1b.csv")
    write.csv(resC_cover6, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\resC_cover6.csv")
    write.csv(scale_cover6, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\scale_cover6.csv")
    write.csv(equated_cover6, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\equated_cover6.csv")
    write.csv(s, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\global_scale_cover6.csv")
  }













