############################################
## Country-Year Wald Statistics via Rasch ## 
############################################
## Clear everything
rm(list=ls())

## STEP 1:  Preliminary packages
{
  library(haven)      
  library(expss)
  library(ggplot2)
  library(dplyr)
  install.packages("quantreg")
  library(quantreg)
  install.packages("sjmisc")
  library(sjmisc)
}

## STEP 2:  Uploading data 
{
  setwd("C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code")
  # Set the data
  FULL = read.csv("data9.csv")
}

## STEP 3:  Subsetting the relevant data (Y, X, COU, YR, censor points)
{
  # Drop19 subset
    drop19 <- FULL[!is.na(FULL$c_drop19), ]
  # Outcome variable with/without censor point
    y_f_alt <- subset(FULL, select = c(RS_global_FULL_alt))
    y_d19_alt <- subset(drop19, select = c(RS_global_drop19_alt))
    y_f <- subset(FULL, select = c(RS_global_FULL))
    y_d19 <- subset(drop19, select = c(RS_global_drop19))
  # Creating the sets of explanatory variables
    x1_f <- subset(FULL, select = c(INCOME_IHS, age_mis, age2_mis, gender_f, educ_b_mis, rural_mis, b_mar_sing, b_mar_marr, b_mar_sep, b_mar_div, b_mar_wid, b_mar_DK, b_mar_Refused, b_mar_Domestic, WPID_RANDOM))
    x1_d19 <- subset(drop19, select = c(INCOME_IHS, age_mis, age2_mis, gender_f, educ_b_mis, rural_mis, b_mar_sing, b_mar_marr, b_mar_sep, b_mar_div, b_mar_wid, b_mar_DK, b_mar_Refused, b_mar_Domestic, WPID_RANDOM))    
  # Creating set of years
    x2_f <- subset(FULL, select = c(y14, y15, y16, y17, y18, y19, WPID_RANDOM))
    x2_d19 <- subset(drop19, select = c(y14, y15, y16, y17, y18, y19, WPID_RANDOM))
  # Creating set of country binaries
    x3_f <- subset(FULL, select = c(36:193))
    x3_d19 <- subset(drop19, select = c(36:193)) 
  # Creating DF for Quantiles
    cq_f <- subset(FULL, select = c(WP5, c_FULL, ISO3))
    cq_f <- cq_f[!duplicated(cq_f), ]
    cq_f <- cbind(cq_f,d=0)
    cq_d19 <- subset(drop19, select = c(WP5, c_drop19, ISO3))
    cq_d19 <- cq_d19[!duplicated(cq_d19), ]
    cq_d19 <- cbind(cq_d19,d=0)
  # Assigning Quantiles
    q_f <-quantile(cq_f$c_FULL, probs = seq(.1, .9, by = .1))
    q_d <- quantile(cq_d19$c_drop19, probs = seq(.1, .9, by = .1))
  # Quantiles by FULL sample
      cq_f$d[cq_f$c_FULL < 10 ] <- 10 
      cq_f$d[cq_f$c_FULL < q_f[9] ] <- 9
      cq_f$d[cq_f$c_FULL < q_f[8] ] <- 8
      cq_f$d[cq_f$c_FULL < q_f[7] ] <- 7
      cq_f$d[cq_f$c_FULL < q_f[6] ] <- 6
      cq_f$d[cq_f$c_FULL < q_f[5] ] <- 5
      cq_f$d[cq_f$c_FULL < q_f[4] ] <- 4
      cq_f$d[cq_f$c_FULL < q_f[3] ] <- 3
      cq_f$d[cq_f$c_FULL < q_f[2] ] <- 2
      cq_f$d[cq_f$c_FULL < q_f[1] ] <- 1
    # Quantiles by drop19 sample
      cq_d19$d[cq_d19$c_drop19 < 10 ] <- 10 
      cq_d19$d[cq_d19$c_drop19 < q_d[9] ] <- 9
      cq_d19$d[cq_d19$c_drop19 < q_d[8] ] <- 8
      cq_d19$d[cq_d19$c_drop19 < q_d[7] ] <- 7
      cq_d19$d[cq_d19$c_drop19 < q_d[6] ] <- 6
      cq_d19$d[cq_d19$c_drop19 < q_d[5] ] <- 5
      cq_d19$d[cq_d19$c_drop19 < q_d[4] ] <- 4
      cq_d19$d[cq_d19$c_drop19 < q_d[3] ] <- 3
      cq_d19$d[cq_d19$c_drop19 < q_d[2] ] <- 2
      cq_d19$d[cq_d19$c_drop19 < q_d[1] ] <- 1
    # Generate lists of WP5's by quantile
      q10_f <- cq_f[cq_f$d == 10,]
      q9_f <- cq_f[cq_f$d == 9,]
      q8_f <- cq_f[cq_f$d == 8,]
      q7_f <- cq_f[cq_f$d == 7,]
      q6_f <- cq_f[cq_f$d == 6,]
      q5_f <- cq_f[cq_f$d == 5,]
      q4_f <- cq_f[cq_f$d == 4,]
      q3_f <- cq_f[cq_f$d == 3,]
      q2_f <- cq_f[cq_f$d == 2,]
      q1_f <- cq_f[cq_f$d == 1,]
    # Creating DF's of quantile minimums
      q10_f$d <- q_d[9]
      q9_f$d <- q_d[8]
      q8_f$d <- q_d[7]
      q7_f$d <- q_d[6]
      q6_f$d <- q_d[5]
      q5_f$d <- q_d[4]
      q4_f$d <- q_d[3]
      q3_f$d <- q_d[2]
      q2_f$d <- q_d[1]
      q1_f$d <- min(q1$c_FULL)
}

## STEP 4: 
# Creating CRQ level explanatory variable sets
# Reg 1 - 
yc <- FULL$c_FULL
y_f <- FULL$RS_global_FULL_alt
x1_f <- subset(x1_f, select = c(1,6))

crq(Curv(y_f,yc) ~ x1_f$INCOME_IHS x1_f$rural_mis, method = "Powell")



    taus, 
    data, 
    subset, 
    weights = wt, 
    na.action, 
    method = c("Powell"), 
    contrasts = NULL, ...)




## Example - Running Powell
{
  set.seed(2345) # setting random number generator
  x <- sqrt(rnorm(100)^2) # setting random x
  y <-  -0.5 + x +(.25 + .25*x)*rnorm(100) # setting random y
  s <- (y > 0) # setting cut off threshold
} (PRIORS)
yLatent <- y
y <- pmax(0,y) # ensuring non-zero
yc <- rep(0,100)


f <- crq(Curv(y,yc) ~ x, tau = tau, method = "Pow")



# crq example with left censoring
set.seed(1968)
n <- 200
x <-rnorm(n)
y <- 5 + x + rnorm(n)
plot(x,y,cex = .5)
c <- 4 + x + rnorm(n)
d <- (y > c)
points(x[!d],y[!d],cex = .5, col = 2)

f <- crq(survival::Surv(pmax(y,c), d, type = "left") ~ x, method = "Portnoy")

g <- summary(f)



























