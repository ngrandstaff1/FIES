###############################
## Standardizing FIES Scores ##   -   FOR THE <testfile.csv> FULL SET
###############################
# In reference to the <FIES_Calc.R> file

## STEPS:
# (1) Upload packages, relevant data, and set workspace
# (2) Create question and weight files by country, Estimate Rasch Model
# (3) Obtain a global scale
# (4) Set scale in reference to the 2014-2016 Global Scale
# (5) Re-write  for a 2014-2018 Global Scale
# (6) Wald, ICCs, prob.assignment, and Rasch descriptives in another R-Script


## STEP 1: Upload packages, relevant data, and set work space
{
library(RM.weights)
library(haven)      
library(expss)
library(ggplot2)
library(dplyr)
setwd("C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code")
FULL = read.csv("testfile.csv")
ISO3 = read.csv("ISO3.csv")
colnames(ISO3) <- c('WP5','ISO3','X')
ISO3 = subset(ISO3, select = -c(X))
ISO3 <- ISO3 %>%  na.omit()
ISO3 <- subset(ISO3, nchar(as.character(ISO3)) <= 3)
ISO3 <- ISO3[!(ISO3$ISO3 == ""), ]
rownames(ISO3) <- 1:nrow(ISO3)
table(FULL$WP5)
c <- table(FULL$WP5)
c = data.frame(table(FULL$WP5))
c1 <- c[order(c$Var1),]
colnames(c1) <- c('WP5','X')
c1_ISO3 <- merge(c1,ISO3,by="WP5", all=TRUE)
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
c1_ISO3$ISO3[c1_ISO3$WP5 == "Kosovo"] <- "XXK"
c1_ISO3$ISO3[c1_ISO3$WP5 == "Montenegro"] <- "MNE"
c1_ISO3$ISO3[c1_ISO3$WP5 == "Palestinian Territories"] <- "PSE"
c1_ISO3$ISO3[c1_ISO3$WP5 == "Serbia"] <- "SRB"
c1_ISO3$ISO3[c1_ISO3$WP5 == "South Sudan"] <- "SSD"
c1_ISO3$ISO3[c1_ISO3$WP5 == "Northern Cyprus"] <- "TCY"
c1_ISO3 <- subset(c1_ISO3, c1_ISO3$X>0)
c1_ISO3 <- subset(c1_ISO3, select = -c(X))
table(FULL$WP5)
c <- table(FULL$WP5)
c = data.frame(table(FULL$WP5))
table(FULL$YEAR_WAVE)
y <- table(FULL$YEAR_WAVE)
y = data.frame(table(FULL$YEAR_WAVE))
cy <- subset(FULL, select = c(YEAR_WAVE, WP5))
duplicated(cy)
cy[duplicated(cy),]
cy <- cy[!duplicated(cy),]
cy <- subset(cy, select = c(YEAR_WAVE, WP5))
rownames(cy) <- 1:nrow(cy)
m_FULL <- merge(FULL,c1_ISO3,by="WP5")
sum(is.na(m_FULL$ISO3))
c1 <- subset(c1_ISO3,select = c(ISO3))
c1 <- c1 %>% mutate(row_number = 1:n())
for (i in c1$ISO3) {
  df <- data.frame(subset(m_FULL, ISO3==i, select=c(WORRIED:WHLDAY, wt)))
  assign(i,df )
}
rm(i)
}


## STEP 2: Create question and weight files by country, Estimate Rasch Model
for (i in c1$ISO3) {
  tryCatch ( {
    df <- data.frame(subset(get(i), select = c(wt)))
    assign(paste("wt",i, sep = "_"), df)
    df1 <- data.frame(subset(get(i), select = c(WORRIED:WHLDAY)))
    assign(i, df1)
    temp_w <- assign(i,df1)
    df2 <- RM.w(get(i), .w = rep(1, nrow(temp_w)))
    assign(paste("res",i, sep = "_"), df2)
  } , error = function(e){})
}     # NOTE: Could also get cond.correl. matrices here


## STEP 3: Obtain a global scale



# Create a file to save output
ee_ALL <- data.frame(matrix(ncol = 21, nrow = nrow(c1_ISO3)))
colnames(ee_ALL) <-c('U1','U2','U3','U4','U5','U6','U7','U8','prev_ms','prev_s',
                    'CCI','adj_thres_ms','adj_thres_s','S1','S2','S3','S4','S5',
                    'S6','S7','S8')
v1 <- data.frame(matrix(ncol=2,nrow=nrow(c)))
v1 <- data.frame(c$Var1,c$Freq)
colnames(v1) <- c('WP5','Freq')
v1 <- merge(c1_ISO3,v1,by="WP5")
ee_ALL <- merge(v1,ee_ALL)
ee_ALL <- ee_ALL[!duplicated(ee_ALL$WP5),]

# Set a loop over all countries to fill in the table
ee_AFG = equating.fun(res_AFG, st = res_AFG$)






################################################################
#FOR STEP 3: EQUATING ITEMS - By standardized scale

# ee1 = equating.fun(rr1, st=NULL, tol = .35, spec.com1 = 1:8, spec.com2=1:8, thres = NULL, maxuniq=3, write.file=FALSE, plot=FALSE, iterative=TRUE, excl.prior1=NULL,excl.prior2=NULL, wt.spec=NUL
  
  # Iterative equating: no items are unique
  ee1 = equating.fun(rrc1,                  # Base Rasch 
                     st = rrc2$b,           # Std. Rasch
                     tol = 0.35,            # Default: 0.35
                     thres = c(-0.28, 1.85),# Thresholds
                     spec.com1 = c(1:8),    # Base items
                     spec.com2 = c(1:8))    # Std. items
  
  ## Non-iterative equating: fixing WORRIED as unique
  ee2 = equating.fun(rrc1,                  # Base Rasch
                     st = rrc2$b,           # Std. Rasch
                     tol = 0.35,            # Default: 0.35 
                     thres = c(-0.28, 1.85),# Thres: 5th,8th item
                     spec.com1 = c(1:8),    # Base items
                     spec.com2 = c(1:8),    # Std. items
                     iterative = FALSE,     # No search uniq.item
                     excl.prior1 = 1,       # Unique items, base
                     excl.prior2 = 1,       # Unique items, Std.
                     wt.spec = rrc1$wt1)    # Post-strat. wgts 
  
  ## Equivalence Outputs: same for iterative and non-iterative
  # E.g. Iterative, change suffix to `2' for ee2 results
  ee1                 # General output
  ee1$scale           # Scale parameter, on base (wide)
  ee1$shift           # Shift parameter, on base (R/L)
  ee1$common          # Vec. of common items => ee2 Q1 "unique"
  ee1$prevs           # Prevalence w.r.t. thres., base country
  ee1$probs.rs        # Prob. beyond thresholds, for each RS
  ee1$cor.comm.items  # Correlation between common items
  ee1$adj.thres       # Adjusted/equated thresholds by country
  ee1$standard        # Standard by which country is compared
  # Default "standard", FAO-FIES '14-'16







































