############################################
## Country-Year Wald Statistics via Rasch ## 
############################################
## Clear everything
rm(list=ls())

## STEPS: 
# 1.      Load relevant packages
# 2.      Uploading data 
# 3.      ISO3 Merge file and clean it
# 4.      Creating C-Y key for subsets
# 5.      Creating names for the subsets
# 6.      Create data frame to store Wald statistics 
# 7.      Creating a (C) key
# 8.      Creating a (C-Y) key and estimating Rasch Model by C-Y
# 9.      Fixing (C-Y) pairs missing items and estimating Rasch Model
# 10.     Create a Place to store Wald results
# 11.     Clean up

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

## STEP 6:  Create data frame to store Wald statistics
{
  # (C-drop19) Create data frame in which to store RM results
  WALD_A <- data.frame(CY_key)
  WALD_B <- data.frame(matrix(ncol = 32, nrow = nrow(CY_key)))
  colnames(WALD_B) <- c('Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8','q1_se.b','q2_se.b','q3_se.b','q4_se.b','q5_se.b','q6_se.b','q7_se.b','q8_se.b','W1_stat','W2_stat','W3_stat','W4_stat','W5_stat','W6_stat','W7_stat','W8_stat','p1_val','p2_val','p3_val','p4_val','p5_val','p6_val','p7_val','p8_val')
  WALD <- merge(WALD_A,WALD_B)
  WALD <- WALD[!duplicated(WALD$CY_key0),]
  rm(WALD_A,WALD_B)
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
  # (C-Y) Create 2014 and populate the data set
  CY_key_2014 <- subset(CY_key, YEAR_WAVE==2014, select=c(WP5:CY_key0))
  for (i in CY_key_2014$ISO3) {
    df <- data.frame(subset(m_FULL, (ISO3==i) & (YEAR_WAVE==2014), select=c(WORRIED:WHLDAY, wt)))
    assign(paste(i,2014,sep = "_", collapse = NULL),df )
  }
  for (i in CY_key_2014$CY_key0) {
    tryCatch ( {
      temp_w <- subset(get(i), select = c(wt))
      temp <- subset(get(i), select = c(WORRIED:WHLDAY))
      res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
      for (n in 1:8) {
        tryCatch ( {
          x <- res$b[[n]]
          assign(paste("b", n, sep=""),x) 
          y <- res$se.b[[n]]
          assign(paste("se.b", n, sep=""),y) 
        }, error = function(e){})
      }
      # Coefficients
      WALD$Q1[CY_key$CY_key0 == i] <- b1
      WALD$Q2[CY_key$CY_key0 == i] <- b2
      WALD$Q3[CY_key$CY_key0 == i] <- b3
      WALD$Q4[CY_key$CY_key0 == i] <- b4
      WALD$Q5[CY_key$CY_key0 == i] <- b5
      WALD$Q6[CY_key$CY_key0 == i] <- b6
      WALD$Q7[CY_key$CY_key0 == i] <- b7
      WALD$Q8[CY_key$CY_key0 == i] <- b8
      
      # Standard errors on coefficients
      WALD$q1_se.b[CY_key$CY_key0 == i] <- se.b1
      WALD$q2_se.b[CY_key$CY_key0 == i] <- se.b2
      WALD$q3_se.b[CY_key$CY_key0 == i] <- se.b3
      WALD$q4_se.b[CY_key$CY_key0 == i] <- se.b4
      WALD$q5_se.b[CY_key$CY_key0 == i] <- se.b5
      WALD$q6_se.b[CY_key$CY_key0 == i] <- se.b6
      WALD$q7_se.b[CY_key$CY_key0 == i] <- se.b7
      WALD$q8_se.b[CY_key$CY_key0 == i] <- se.b8
      rm(i)
      }, error = function(e){})
    }
  for (i in CY_key_2014$CY_key0) {
    rm(list=paste0(i))
  }

  # (C-Y) Create 2015 and populate the data set
  CY_key_2015 <- subset(CY_key, YEAR_WAVE==2015, select=c(WP5:CY_key0))
  for (i in CY_key_2015$ISO3) {
    df <- data.frame(subset(m_FULL, (ISO3==i) & (YEAR_WAVE==2015), select=c(WORRIED:WHLDAY, wt)))
    assign(paste(i,2015,sep = "_", collapse = NULL),df )
  }
  for (i in CY_key_2015$CY_key0) {
    tryCatch ( {
      temp_w <- subset(get(i), select = c(wt))
      temp <- subset(get(i), select = c(WORRIED:WHLDAY))
      res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
      for (n in 1:8) {
        tryCatch ( {
          x <- res$b[[n]]
          assign(paste("b", n, sep=""),x) 
          y <- res$se.b[[n]]
          assign(paste("se.b", n, sep=""),y) 
        }, error = function(e){})
      }
      # Coefficients
      WALD$Q1[CY_key$CY_key0 == i] <- b1
      WALD$Q2[CY_key$CY_key0 == i] <- b2
      WALD$Q3[CY_key$CY_key0 == i] <- b3
      WALD$Q4[CY_key$CY_key0 == i] <- b4
      WALD$Q5[CY_key$CY_key0 == i] <- b5
      WALD$Q6[CY_key$CY_key0 == i] <- b6
      WALD$Q7[CY_key$CY_key0 == i] <- b7
      WALD$Q8[CY_key$CY_key0 == i] <- b8
      # Standard errors on coefficients
      WALD$q1_se.b[CY_key$CY_key0 == i] <- se.b1
      WALD$q2_se.b[CY_key$CY_key0 == i] <- se.b2
      WALD$q3_se.b[CY_key$CY_key0 == i] <- se.b3
      WALD$q4_se.b[CY_key$CY_key0 == i] <- se.b4
      WALD$q5_se.b[CY_key$CY_key0 == i] <- se.b5
      WALD$q6_se.b[CY_key$CY_key0 == i] <- se.b6
      WALD$q7_se.b[CY_key$CY_key0 == i] <- se.b7
      WALD$q8_se.b[CY_key$CY_key0 == i] <- se.b8
    }, error = function(e){})
  }
  for (i in CY_key_2015$CY_key0) {
    rm(list=paste0(i))
  }
  
  # (C-Y) Create 2016 and populate the data set
  CY_key_2016 <- subset(CY_key, YEAR_WAVE==2016, select=c(WP5:CY_key0))
  for (i in CY_key_2016$ISO3) {
    df <- data.frame(subset(m_FULL, (ISO3==i) & (YEAR_WAVE==2016), select=c(WORRIED:WHLDAY, wt)))
    assign(paste(i,2016,sep = "_", collapse = NULL),df )
  }
  for (i in CY_key_2016$CY_key0) {
    tryCatch ( {
      temp_w <- subset(get(i), select = c(wt))
      temp <- subset(get(i), select = c(WORRIED:WHLDAY))
      res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
      for (n in 1:8) {
        tryCatch ( {
          x <- res$b[[n]]
          assign(paste("b", n, sep=""),x) 
          y <- res$se.b[[n]]
          assign(paste("se.b", n, sep=""),y) 
        }, error = function(e){})
      }
      # Coefficients
      WALD$Q1[CY_key$CY_key0 == i] <- b1
      WALD$Q2[CY_key$CY_key0 == i] <- b2
      WALD$Q3[CY_key$CY_key0 == i] <- b3
      WALD$Q4[CY_key$CY_key0 == i] <- b4
      WALD$Q5[CY_key$CY_key0 == i] <- b5
      WALD$Q6[CY_key$CY_key0 == i] <- b6
      WALD$Q7[CY_key$CY_key0 == i] <- b7
      WALD$Q8[CY_key$CY_key0 == i] <- b8
      # Standard errors on coefficients
      WALD$q1_se.b[CY_key$CY_key0 == i] <- se.b1
      WALD$q2_se.b[CY_key$CY_key0 == i] <- se.b2
      WALD$q3_se.b[CY_key$CY_key0 == i] <- se.b3
      WALD$q4_se.b[CY_key$CY_key0 == i] <- se.b4
      WALD$q5_se.b[CY_key$CY_key0 == i] <- se.b5
      WALD$q6_se.b[CY_key$CY_key0 == i] <- se.b6
      WALD$q7_se.b[CY_key$CY_key0 == i] <- se.b7
      WALD$q8_se.b[CY_key$CY_key0 == i] <- se.b8
    }, error = function(e){})
  }
  for (i in CY_key_2016$CY_key0) {
    rm(list=paste0(i))
  }

  # (C-Y) Create 2017 and populate the data set
  CY_key_2017 <- subset(CY_key, YEAR_WAVE==2017, select=c(WP5:CY_key0))
  for (i in CY_key_2017$ISO3) {
    df <- data.frame(subset(m_FULL, (ISO3==i) & (YEAR_WAVE==2017), select=c(WORRIED:WHLDAY, wt)))
    assign(paste(i,2017,sep = "_", collapse = NULL),df )
  }
  for (i in CY_key_2017$CY_key0) {
    tryCatch ( {
      temp_w <- subset(get(i), select = c(wt))
      temp <- subset(get(i), select = c(WORRIED:WHLDAY))
      res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
      for (n in 1:8) {
        tryCatch ( {
          x <- res$b[[n]]
          assign(paste("b", n, sep=""),x) 
          y <- res$se.b[[n]]
          assign(paste("se.b", n, sep=""),y) 
        }, error = function(e){})
      }
      # Coefficients
      WALD$Q1[CY_key$CY_key0 == i] <- b1
      WALD$Q2[CY_key$CY_key0 == i] <- b2
      WALD$Q3[CY_key$CY_key0 == i] <- b3
      WALD$Q4[CY_key$CY_key0 == i] <- b4
      WALD$Q5[CY_key$CY_key0 == i] <- b5
      WALD$Q6[CY_key$CY_key0 == i] <- b6
      WALD$Q7[CY_key$CY_key0 == i] <- b7
      WALD$Q8[CY_key$CY_key0 == i] <- b8
      # Standard errors on coefficients
      WALD$q1_se.b[CY_key$CY_key0 == i] <- se.b1
      WALD$q2_se.b[CY_key$CY_key0 == i] <- se.b2
      WALD$q3_se.b[CY_key$CY_key0 == i] <- se.b3
      WALD$q4_se.b[CY_key$CY_key0 == i] <- se.b4
      WALD$q5_se.b[CY_key$CY_key0 == i] <- se.b5
      WALD$q6_se.b[CY_key$CY_key0 == i] <- se.b6
      WALD$q7_se.b[CY_key$CY_key0 == i] <- se.b7
      WALD$q8_se.b[CY_key$CY_key0 == i] <- se.b8
    }, error = function(e){})
  }
  for (i in CY_key_2017$CY_key0) {
    rm(list=paste0(i))
  }   
        
  # (C-Y) Create 2018 and populate the data set
  CY_key_2018 <- subset(CY_key, YEAR_WAVE==2018, select=c(WP5:CY_key0))
  for (i in CY_key_2018$ISO3) {
    df <- data.frame(subset(m_FULL, (ISO3==i) & (YEAR_WAVE==2018), select=c(WORRIED:WHLDAY, wt)))
    assign(paste(i,2018,sep = "_", collapse = NULL),df )
  }
  for (i in CY_key_2018$CY_key0) {
    tryCatch ( {
      temp_w <- subset(get(i), select = c(wt))
      temp <- subset(get(i), select = c(WORRIED:WHLDAY))
      res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
      for (n in 1:8) {
        tryCatch ( {
          x <- res$b[[n]]
          assign(paste("b", n, sep=""),x) 
          y <- res$se.b[[n]]
          assign(paste("se.b", n, sep=""),y) 
        }, error = function(e){})
      }
      # Coefficients
      WALD$Q1[CY_key$CY_key0 == i] <- b1
      WALD$Q2[CY_key$CY_key0 == i] <- b2
      WALD$Q3[CY_key$CY_key0 == i] <- b3
      WALD$Q4[CY_key$CY_key0 == i] <- b4
      WALD$Q5[CY_key$CY_key0 == i] <- b5
      WALD$Q6[CY_key$CY_key0 == i] <- b6
      WALD$Q7[CY_key$CY_key0 == i] <- b7
      WALD$Q8[CY_key$CY_key0 == i] <- b8
      # Standard errors on coefficients
      WALD$q1_se.b[CY_key$CY_key0 == i] <- se.b1
      WALD$q2_se.b[CY_key$CY_key0 == i] <- se.b2
      WALD$q3_se.b[CY_key$CY_key0 == i] <- se.b3
      WALD$q4_se.b[CY_key$CY_key0 == i] <- se.b4
      WALD$q5_se.b[CY_key$CY_key0 == i] <- se.b5
      WALD$q6_se.b[CY_key$CY_key0 == i] <- se.b6
      WALD$q7_se.b[CY_key$CY_key0 == i] <- se.b7
      WALD$q8_se.b[CY_key$CY_key0 == i] <- se.b8
    }, error = function(e){})
  }
  for (i in CY_key_2018$CY_key0) {
    rm(list=paste0(i))
  }
  
  # (C-Y) Create 2019 and populate the data set
  CY_key_2019 <- subset(CY_key, YEAR_WAVE==2019, select=c(WP5:CY_key0))
  for (i in CY_key_2019$ISO3) {
    df <- data.frame(subset(m_FULL, (ISO3==i) & (YEAR_WAVE==2019), select=c(WORRIED:WHLDAY, wt)))
    assign(paste(i,2019,sep = "_", collapse = NULL),df )
  }
  for (i in CY_key_2019$CY_key0) {
    tryCatch ( {
      temp_w <- subset(get(i), select = c(wt))
      temp <- subset(get(i), select = c(WORRIED:WHLDAY))
      res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
      for (n in 1:8) {
        tryCatch ( {
          x <- res$b[[n]]
          assign(paste("b", n, sep=""),x) 
          y <- res$se.b[[n]]
          assign(paste("se.b", n, sep=""),y) 
        }, error = function(e){})
      }
      # Coefficients
      WALD$Q1[CY_key$CY_key0 == i] <- b1
      WALD$Q2[CY_key$CY_key0 == i] <- b2
      WALD$Q3[CY_key$CY_key0 == i] <- b3
      WALD$Q4[CY_key$CY_key0 == i] <- b4
      WALD$Q5[CY_key$CY_key0 == i] <- b5
      WALD$Q6[CY_key$CY_key0 == i] <- b6
      WALD$Q7[CY_key$CY_key0 == i] <- b7
      WALD$Q8[CY_key$CY_key0 == i] <- b8
      # Standard errors on coefficients
      WALD$q1_se.b[CY_key$CY_key0 == i] <- se.b1
      WALD$q2_se.b[CY_key$CY_key0 == i] <- se.b2
      WALD$q3_se.b[CY_key$CY_key0 == i] <- se.b3
      WALD$q4_se.b[CY_key$CY_key0 == i] <- se.b4
      WALD$q5_se.b[CY_key$CY_key0 == i] <- se.b5
      WALD$q6_se.b[CY_key$CY_key0 == i] <- se.b6
      WALD$q7_se.b[CY_key$CY_key0 == i] <- se.b7
      WALD$q8_se.b[CY_key$CY_key0 == i] <- se.b8
    }, error = function(e){})
  }
  for (i in CY_key_2019$CY_key0) {
    rm(list=paste0(i))
  }        
}     
        
## STEP 9:  Fixing (C-Y) pairs missing items and estimating Rasch Model
{
  # Creating relevant subsets
    # Denmark - missing 1 & 7
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
          for (n in 1:8) {
            tryCatch ( {
              x <- res$b[[n]]
              assign(paste("b", n, sep=""),x) 
              y <- res$se.b[[n]]
              assign(paste("se.b", n, sep=""),y) 
            }, error = function(e){})
          }
          
          # Coefficients
          
          WALD$Q2[CY_key$CY_key0 == i] <- b1
          WALD$Q3[CY_key$CY_key0 == i] <- b2
          WALD$Q4[CY_key$CY_key0 == i] <- b3
          WALD$Q5[CY_key$CY_key0 == i] <- b4
          WALD$Q6[CY_key$CY_key0 == i] <- b5
        
          WALD$Q8[CY_key$CY_key0 == i] <- b6
          # Standard errors on coefficients
          
          WALD$q2_se.b[CY_key$CY_key0 == i] <- se.b1
          WALD$q3_se.b[CY_key$CY_key0 == i] <- se.b2
          WALD$q4_se.b[CY_key$CY_key0 == i] <- se.b3
          WALD$q5_se.b[CY_key$CY_key0 == i] <- se.b4
          WALD$q6_se.b[CY_key$CY_key0 == i] <- se.b5
          
          WALD$q8_se.b[CY_key$CY_key0 == i] <- se.b6
        }, error = function(e){})
      }
       
    # Japan - missing 7
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
          for (n in 1:8) {
            tryCatch ( {
              x <- res$b[[n]]
              assign(paste("b", n, sep=""),x) 
              y <- res$se.b[[n]]
              assign(paste("se.b", n, sep=""),y) 
            }, error = function(e){})
          }
          # Coefficients
          WALD$Q1[CY_key$CY_key0 == i] <- b1
          WALD$Q2[CY_key$CY_key0 == i] <- b2
          WALD$Q3[CY_key$CY_key0 == i] <- b3
          WALD$Q4[CY_key$CY_key0 == i] <- b4
          WALD$Q5[CY_key$CY_key0 == i] <- b5
          WALD$Q6[CY_key$CY_key0 == i] <- b6
          
          WALD$Q8[CY_key$CY_key0 == i] <- b7
          # Standard errors on coefficients
          WALD$q1_se.b[CY_key$CY_key0 == i] <- se.b1
          WALD$q2_se.b[CY_key$CY_key0 == i] <- se.b2
          WALD$q3_se.b[CY_key$CY_key0 == i] <- se.b3
          WALD$q4_se.b[CY_key$CY_key0 == i] <- se.b4
          WALD$q5_se.b[CY_key$CY_key0 == i] <- se.b5
          WALD$q6_se.b[CY_key$CY_key0 == i] <- se.b6
          
          WALD$q8_se.b[CY_key$CY_key0 == i] <- se.b7
        }, error = function(e){})
      }
      
    # Malta - missing 1
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
          for (n in 1:8) {
            tryCatch ( {
              x <- res$b[[n]]
              assign(paste("b", n, sep=""),x) 
              y <- res$se.b[[n]]
              assign(paste("se.b", n, sep=""),y) 
            }, error = function(e){})
          }
          # Coefficients
          WALD$Q1[CY_key$CY_key0 == i] <- b1
          WALD$Q2[CY_key$CY_key0 == i] <- b2
          WALD$Q3[CY_key$CY_key0 == i] <- b3
          WALD$Q4[CY_key$CY_key0 == i] <- b4
          WALD$Q5[CY_key$CY_key0 == i] <- b5
          WALD$Q6[CY_key$CY_key0 == i] <- b6
          
          WALD$Q8[CY_key$CY_key0 == i] <- b7
          # Standard errors on coefficients
          WALD$q1_se.b[CY_key$CY_key0 == i] <- se.b1
          WALD$q2_se.b[CY_key$CY_key0 == i] <- se.b2
          WALD$q3_se.b[CY_key$CY_key0 == i] <- se.b3
          WALD$q4_se.b[CY_key$CY_key0 == i] <- se.b4
          WALD$q5_se.b[CY_key$CY_key0 == i] <- se.b5
          WALD$q6_se.b[CY_key$CY_key0 == i] <- se.b6
          
          WALD$q8_se.b[CY_key$CY_key0 == i] <- se.b7
        }, error = function(e){})
      }
      
    # Netherlands - missing 4,5,6
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
          temp <- subset(get(i), select = c(ORRIED:FEWFOOD,HUNGRY,WHLDAY))
          res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
          for (n in 1:8) {
            tryCatch ( {
              x <- res$b[[n]]
              assign(paste("b", n, sep=""),x) 
              y <- res$se.b[[n]]
              assign(paste("se.b", n, sep=""),y) 
            }, error = function(e){})
          }
          # Coefficients
          WALD$Q1[CY_key$CY_key0 == i] <- b1
          WALD$Q2[CY_key$CY_key0 == i] <- b2
          WALD$Q3[CY_key$CY_key0 == i] <- b3
          
          
          WALD$Q7[CY_key$CY_key0 == i] <- b4
          WALD$Q8[CY_key$CY_key0 == i] <- b5
          # Standard errors on coefficients
          WALD$q1_se.b[CY_key$CY_key0 == i] <- se.b1
          WALD$q2_se.b[CY_key$CY_key0 == i] <- se.b2
          WALD$q3_se.b[CY_key$CY_key0 == i] <- se.b3

          
          WALD$q7_se.b[CY_key$CY_key0 == i] <- se.b4
          WALD$q8_se.b[CY_key$CY_key0 == i] <- se.b5
        }, error = function(e){})
      }
  
      # Clean up a bit
      rm(DNK_2014, JPN_2014, MLT_2014, NLD_2014,DNK_2015, JPN_2015, MLT_2015, NLD_2015,DNK_2016, JPN_2016, MLT_2016, NLD_2016,DNK_2017, JPN_2017, MLT_2017, NLD_2017,DNK_2018, JPN_2018, MLT_2018, NLD_2018)
      rm(DNK_v, JPN_v, MLT_v, NLD_v)
  
}
 
## STEP 10: Create a Place to store Wald results
{
  # 2014 - 2015
    for (i in c1_ISO3$ISO3) {
      tryCatch ( {
        a1 <-  WALD$Q1[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)]
        a2 <-  WALD$Q2[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)]
        a3 <-  WALD$Q3[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)]
        a4 <-  WALD$Q4[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)]
        a5 <-  WALD$Q5[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)]
        a6 <-  WALD$Q6[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)]
        a7 <-  WALD$Q7[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)]
        a8 <-  WALD$Q8[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)]
        
        b1 <-  WALD$Q1[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)]
        b2 <-  WALD$Q2[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)]
        b3 <-  WALD$Q3[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)]
        b4 <-  WALD$Q4[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)]
        b5 <-  WALD$Q5[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)]
        b6 <-  WALD$Q6[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)]
        b7 <-  WALD$Q7[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)]
        b8 <-  WALD$Q8[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)]
        
        c1 <-  WALD$q1_se.b[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)]
        c2 <-  WALD$q2_se.b[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)]
        c3 <-  WALD$q3_se.b[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)]
        c4 <-  WALD$q4_se.b[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)]
        c5 <-  WALD$q5_se.b[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)]
        c6 <-  WALD$q6_se.b[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)]
        c7 <-  WALD$q7_se.b[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)]
        c8 <-  WALD$q8_se.b[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)]
        
        d1 <-  WALD$q1_se.b[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)]
        d2 <-  WALD$q2_se.b[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)]
        d3 <-  WALD$q3_se.b[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)]
        d4 <-  WALD$q4_se.b[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)]
        d5 <-  WALD$q5_se.b[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)]
        d6 <-  WALD$q6_se.b[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)]
        d7 <-  WALD$q7_se.b[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)]
        d8 <-  WALD$q8_se.b[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)]
        
        # Estimate Wald Statistics
        wald1  = EWaldtest(a1, b1, c1, d1)$tab
        wald2  = EWaldtest(a2, b2, c2, d2)$tab
        wald3  = EWaldtest(a3, b3, c3, d3)$tab
        wald4  = EWaldtest(a4, b4, c4, d4)$tab
        wald5  = EWaldtest(a5, b5, c5, d5)$tab
        wald6  = EWaldtest(a6, b6, c6, d6)$tab
        wald7  = EWaldtest(a7, b7, c7, d7)$tab
        wald8  = EWaldtest(a8, b8, c8, d8)$tab

        WALD$W1_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)] <- wald1[[1]]
        WALD$W2_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)] <- wald2[[1]]
        WALD$W3_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)] <- wald3[[1]]
        WALD$W4_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)] <- wald4[[1]]
        WALD$W5_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)] <- wald5[[1]]
        WALD$W6_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)] <- wald6[[1]]
        WALD$W7_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)] <- wald7[[1]]
        WALD$W8_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)] <- wald8[[1]]
        
        WALD$p1_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)] <- wald1[[2]]
        WALD$p2_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)] <- wald2[[2]]
        WALD$p3_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)] <- wald3[[2]]
        WALD$p4_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)] <- wald4[[2]]
        WALD$p5_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)] <- wald5[[2]]
        WALD$p6_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)] <- wald6[[2]]
        WALD$p7_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)] <- wald7[[2]]
        WALD$p8_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2014)] <- wald8[[2]]
      
      }, error = function(e){})    
    }

  # 2015 - 2016
    for (i in c1_ISO3$ISO3) {
      tryCatch ( {
        a1 <-  WALD$Q1[WALD$ISO3 == i & WALD$YEAR_WAVE==2015]
        a2 <-  WALD$Q2[WALD$ISO3 == i & WALD$YEAR_WAVE==2015]
        a3 <-  WALD$Q3[WALD$ISO3 == i & WALD$YEAR_WAVE==2015]
        a4 <-  WALD$Q4[WALD$ISO3 == i & WALD$YEAR_WAVE==2015]
        a5 <-  WALD$Q5[WALD$ISO3 == i & WALD$YEAR_WAVE==2015]
        a6 <-  WALD$Q6[WALD$ISO3 == i & WALD$YEAR_WAVE==2015]
        a7 <-  WALD$Q7[WALD$ISO3 == i & WALD$YEAR_WAVE==2015]
        a8 <-  WALD$Q8[WALD$ISO3 == i & WALD$YEAR_WAVE==2015]
        
        b1 <-  WALD$Q1[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        b2 <-  WALD$Q2[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        b3 <-  WALD$Q3[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        b4 <-  WALD$Q4[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        b5 <-  WALD$Q5[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        b6 <-  WALD$Q6[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        b7 <-  WALD$Q7[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        b8 <-  WALD$Q8[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        
        c1 <-  WALD$q1_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2015]
        c2 <-  WALD$q2_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2015]
        c3 <-  WALD$q3_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2015]
        c4 <-  WALD$q4_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2015]
        c5 <-  WALD$q5_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2015]
        c6 <-  WALD$q6_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2015]
        c7 <-  WALD$q7_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2015]
        c8 <-  WALD$q8_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2015]
        
        d1 <-  WALD$q1_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        d2 <-  WALD$q2_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        d3 <-  WALD$q3_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        d4 <-  WALD$q4_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        d5 <-  WALD$q5_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        d6 <-  WALD$q6_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        d7 <-  WALD$q7_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        d8 <-  WALD$q8_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        
        # Estimate Wald Statistics
        wald1  = EWaldtest(a1, b1, c1, d1)$tab
        wald2  = EWaldtest(a2, b2, c2, d2)$tab
        wald3  = EWaldtest(a3, b3, c3, d3)$tab
        wald4  = EWaldtest(a4, b4, c4, d4)$tab
        wald5  = EWaldtest(a5, b5, c5, d5)$tab
        wald6  = EWaldtest(a6, b6, c6, d6)$tab
        wald7  = EWaldtest(a7, b7, c7, d7)$tab
        wald8  = EWaldtest(a8, b8, c8, d8)$tab
        
        WALD$W1_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)] <- wald1[[1]]
        WALD$W2_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)] <- wald2[[1]]
        WALD$W3_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)] <- wald3[[1]]
        WALD$W4_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)] <- wald4[[1]]
        WALD$W5_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)] <- wald5[[1]]
        WALD$W6_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)] <- wald6[[1]]
        WALD$W7_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)] <- wald7[[1]]
        WALD$W8_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)] <- wald8[[1]]
        
        WALD$p1_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)] <- wald1[[2]]
        WALD$p2_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)] <- wald2[[2]]
        WALD$p3_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)] <- wald3[[2]]
        WALD$p4_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)] <- wald4[[2]]
        WALD$p5_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)] <- wald5[[2]]
        WALD$p6_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)] <- wald6[[2]]
        WALD$p7_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)] <- wald7[[2]]
        WALD$p8_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2015)] <- wald8[[2]]
    
      }, error = function(e){})    
    }

  # 2016 - 2017
    for (i in c1_ISO3$ISO3) {
      tryCatch ( {
        a1 <-  WALD$Q1[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        a2 <-  WALD$Q2[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        a3 <-  WALD$Q3[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        a4 <-  WALD$Q4[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        a5 <-  WALD$Q5[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        a6 <-  WALD$Q6[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        a7 <-  WALD$Q7[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        a8 <-  WALD$Q8[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        
        b1 <-  WALD$Q1[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        b2 <-  WALD$Q2[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        b3 <-  WALD$Q3[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        b4 <-  WALD$Q4[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        b5 <-  WALD$Q5[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        b6 <-  WALD$Q6[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        b7 <-  WALD$Q7[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        b8 <-  WALD$Q8[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        
        c1 <-  WALD$q1_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        c2 <-  WALD$q2_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        c3 <-  WALD$q3_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        c4 <-  WALD$q4_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        c5 <-  WALD$q5_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        c6 <-  WALD$q6_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        c7 <-  WALD$q7_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        c8 <-  WALD$q8_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2016]
        
        d1 <-  WALD$q1_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        d2 <-  WALD$q2_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        d3 <-  WALD$q3_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        d4 <-  WALD$q4_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        d5 <-  WALD$q5_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        d6 <-  WALD$q6_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        d7 <-  WALD$q7_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        d8 <-  WALD$q8_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        
        # Estimate Wald Statistics
        wald1  = EWaldtest(a1, b1, c1, d1)$tab
        wald2  = EWaldtest(a2, b2, c2, d2)$tab
        wald3  = EWaldtest(a3, b3, c3, d3)$tab
        wald4  = EWaldtest(a4, b4, c4, d4)$tab
        wald5  = EWaldtest(a5, b5, c5, d5)$tab
        wald6  = EWaldtest(a6, b6, c6, d6)$tab
        wald7  = EWaldtest(a7, b7, c7, d7)$tab
        wald8  = EWaldtest(a8, b8, c8, d8)$tab
        
        WALD$W1_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2016)] <- wald1[[1]]
        WALD$W2_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2016)] <- wald2[[1]]
        WALD$W3_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2016)] <- wald3[[1]]
        WALD$W4_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2016)] <- wald4[[1]]
        WALD$W5_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2016)] <- wald5[[1]]
        WALD$W6_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2016)] <- wald6[[1]]
        WALD$W7_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2016)] <- wald7[[1]]
        WALD$W8_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2016)] <- wald8[[1]]
        
        WALD$p1_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2016)] <- wald1[[2]]
        WALD$p2_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2016)] <- wald2[[2]]
        WALD$p3_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2016)] <- wald3[[2]]
        WALD$p4_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2016)] <- wald4[[2]]
        WALD$p5_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2016)] <- wald5[[2]]
        WALD$p6_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2016)] <- wald6[[2]]
        WALD$p7_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2016)] <- wald7[[2]]
        WALD$p8_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2016)] <- wald8[[2]]
        
      }, error = function(e){})    
    }

  # 2017 - 2018
    for (i in c1_ISO3$ISO3) {
      tryCatch ( {
        a1 <-  WALD$Q1[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        a2 <-  WALD$Q2[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        a3 <-  WALD$Q3[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        a4 <-  WALD$Q4[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        a5 <-  WALD$Q5[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        a6 <-  WALD$Q6[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        a7 <-  WALD$Q7[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        a8 <-  WALD$Q8[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        
        b1 <-  WALD$Q1[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        b2 <-  WALD$Q2[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        b3 <-  WALD$Q3[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        b4 <-  WALD$Q4[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        b5 <-  WALD$Q5[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        b6 <-  WALD$Q6[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        b7 <-  WALD$Q7[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        b8 <-  WALD$Q8[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        
        c1 <-  WALD$q1_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        c2 <-  WALD$q2_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        c3 <-  WALD$q3_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        c4 <-  WALD$q4_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        c5 <-  WALD$q5_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        c6 <-  WALD$q6_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        c7 <-  WALD$q7_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        c8 <-  WALD$q8_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2017]
        
        d1 <-  WALD$q1_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        d2 <-  WALD$q2_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        d3 <-  WALD$q3_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        d4 <-  WALD$q4_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        d5 <-  WALD$q5_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        d6 <-  WALD$q6_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        d7 <-  WALD$q7_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        d8 <-  WALD$q8_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        
        # Estimate Wald Statistics
        wald1  = EWaldtest(a1, b1, c1, d1)$tab
        wald2  = EWaldtest(a2, b2, c2, d2)$tab
        wald3  = EWaldtest(a3, b3, c3, d3)$tab
        wald4  = EWaldtest(a4, b4, c4, d4)$tab
        wald5  = EWaldtest(a5, b5, c5, d5)$tab
        wald6  = EWaldtest(a6, b6, c6, d6)$tab
        wald7  = EWaldtest(a7, b7, c7, d7)$tab
        wald8  = EWaldtest(a8, b8, c8, d8)$tab
        
        WALD$W1_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2017)] <- wald1[[1]]
        WALD$W2_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2017)] <- wald2[[1]]
        WALD$W3_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2017)] <- wald3[[1]]
        WALD$W4_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2017)] <- wald4[[1]]
        WALD$W5_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2017)] <- wald5[[1]]
        WALD$W6_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2017)] <- wald6[[1]]
        WALD$W7_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2017)] <- wald7[[1]]
        WALD$W8_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2017)] <- wald8[[1]]
        
        WALD$p1_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2017)] <- wald1[[2]]
        WALD$p2_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2017)] <- wald2[[2]]
        WALD$p3_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2017)] <- wald3[[2]]
        WALD$p4_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2017)] <- wald4[[2]]
        WALD$p5_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2017)] <- wald5[[2]]
        WALD$p6_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2017)] <- wald6[[2]]
        WALD$p7_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2017)] <- wald7[[2]]
        WALD$p8_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2017)] <- wald8[[2]]
        
      }, error = function(e){})    
    }

  # 2018 - 2019
    for (i in c1_ISO3$ISO3) {
      tryCatch ( {
        a1 <-  WALD$Q1[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        a2 <-  WALD$Q2[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        a3 <-  WALD$Q3[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        a4 <-  WALD$Q4[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        a5 <-  WALD$Q5[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        a6 <-  WALD$Q6[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        a7 <-  WALD$Q7[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        a8 <-  WALD$Q8[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        
        b1 <-  WALD$Q1[WALD$ISO3 == i & WALD$YEAR_WAVE==2019]
        b2 <-  WALD$Q2[WALD$ISO3 == i & WALD$YEAR_WAVE==2019]
        b3 <-  WALD$Q3[WALD$ISO3 == i & WALD$YEAR_WAVE==2019]
        b4 <-  WALD$Q4[WALD$ISO3 == i & WALD$YEAR_WAVE==2019]
        b5 <-  WALD$Q5[WALD$ISO3 == i & WALD$YEAR_WAVE==2019]
        b6 <-  WALD$Q6[WALD$ISO3 == i & WALD$YEAR_WAVE==2019]
        b7 <-  WALD$Q7[WALD$ISO3 == i & WALD$YEAR_WAVE==2019]
        b8 <-  WALD$Q8[WALD$ISO3 == i & WALD$YEAR_WAVE==2019]
        
        c1 <-  WALD$q1_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        c2 <-  WALD$q2_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        c3 <-  WALD$q3_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        c4 <-  WALD$q4_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        c5 <-  WALD$q5_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        c6 <-  WALD$q6_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        c7 <-  WALD$q7_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        c8 <-  WALD$q8_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2018]
        
        d1 <-  WALD$q1_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2019]
        d2 <-  WALD$q2_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2019]
        d3 <-  WALD$q3_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2019]
        d4 <-  WALD$q4_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2019]
        d5 <-  WALD$q5_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2019]
        d6 <-  WALD$q6_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2019]
        d7 <-  WALD$q7_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2019]
        d8 <-  WALD$q8_se.b[WALD$ISO3 == i & WALD$YEAR_WAVE==2019]
        
        # Estimate Wald Statistics
        wald1  = EWaldtest(a1, b1, c1, d1)$tab
        wald2  = EWaldtest(a2, b2, c2, d2)$tab
        wald3  = EWaldtest(a3, b3, c3, d3)$tab
        wald4  = EWaldtest(a4, b4, c4, d4)$tab
        wald5  = EWaldtest(a5, b5, c5, d5)$tab
        wald6  = EWaldtest(a6, b6, c6, d6)$tab
        wald7  = EWaldtest(a7, b7, c7, d7)$tab
        wald8  = EWaldtest(a8, b8, c8, d8)$tab
        
        WALD$W1_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2018)] <- wald1[[1]]
        WALD$W2_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2018)] <- wald2[[1]]
        WALD$W3_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2018)] <- wald3[[1]]
        WALD$W4_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2018)] <- wald4[[1]]
        WALD$W5_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2018)] <- wald5[[1]]
        WALD$W6_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2018)] <- wald6[[1]]
        WALD$W7_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2018)] <- wald7[[1]]
        WALD$W8_stat[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2018)] <- wald8[[1]]
        
        WALD$p1_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2018)] <- wald1[[2]]
        WALD$p2_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2018)] <- wald2[[2]]
        WALD$p3_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2018)] <- wald3[[2]]
        WALD$p4_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2018)] <- wald4[[2]]
        WALD$p5_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2018)] <- wald5[[2]]
        WALD$p6_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2018)] <- wald6[[2]]
        WALD$p7_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2018)] <- wald7[[2]]
        WALD$p8_val[(WALD$ISO3 == i) & (WALD$YEAR_WAVE==2018)] <- wald8[[2]]
 
      }, error = function(e){})  
    }
}
    
## STEP 11: Clean up and export
{
  # Clean up
  rm(a1,a2,a3,a4,a5,a6,a7,a8,b1,b2,b3,b4,b5,b6,b7,b8,c1,c2,c3,c4,c5,c6,c7,c8,d1,d2,d3,d4,d5,d6,d7,d8,i)
  rm(se.b1,se.b2,se.b3,se.b4,se.b5,se.b6,se.b7,se.b8)
  rm(wald1,wald2,wald3,wald4,wald5,wald6,wald7,wald8)
  rm(res,CY_key_2014,CY_key_2015,CY_key_2016,CY_key_2017,CY_key_2018,CY_key_2019)
  rm(temp, temp_w, x, y, n)
  # Export
  write.csv(WALD, "C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code\\R Output\\FIES_CY_WALD.csv")
  
}
    
    


   

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    





















