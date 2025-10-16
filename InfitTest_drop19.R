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
  tol <- 0.4 # Tolerance level where =1 if "common" (higher number is better)
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
  # Sum number of unique items per country
  for (i in 1:nrow(resC_drop19)) {
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
  unique_n <- (8*nrow(resC_drop19)) - NA_n - common_n  #total number unique items
  
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
      tol <- 0.4 # Tolerance level where =1 if "common" (higher number is better)
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
  for (i in c1$ISO3) {
    tryCatch ( {
      temp_w <- subset(get(i), select = c(wt))
      temp <- subset(get(i), select = c(WORRIED:WHLDAY))
      res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
      ee1 = equating.fun(res, st = b, tol = 0.4, spec.com1 = c(1:8), spec.com2 = c(1:8)) 
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

## FINAL: Output a table of translated values
{
  # Full set
  equated_drop19 <- subset(equate_drop19,select = c('WP5','ISO3','T1','T2','T3','T4','T5','T6','T7','T8','corr_comm'))
  View(equated_drop19)

  
}