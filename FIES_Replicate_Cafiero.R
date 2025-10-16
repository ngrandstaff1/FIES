##########################################################
## FIES Coding in R - Replicating Cafiero et al. (2018) ## 
##########################################################
# Source: Cafiero, Viviani, Nord (2018)
  
## PRELIMINARIES
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
#
#
#
################################################################
## EQUATING ITEMS - By standardized scale
{
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
}
#
#
#
################################################################
## WALD TEST - Sample independence for item severity parameters
{
  # EWaldtest(b1, b2, se1, se2) 
  # z = (b1hat-b2hat)/[sqrt(Var(b1hat)+Var(b2hat)]
  # H_o: Parameter estimate equality; H_1: b1 != b2
  # => Item sev. not depend. on samples, but only items themselves
  
  # Formatting data and draw samples
  n1 = nrow(X1)                  # observations (n) by context
  n2 = nrow(X2)
  n3 = nrow(X3)
  n4 = nrow(X4)
  samp1a = sample(1:n1, n1/2)    # random samples drawn
  samp2a = sample(1:n2, n2/2)    
  samp3a = sample(1:n3, n3/2)
  samp4a = sample(1:n4, n4/2)
  samp1b = setdiff(1:n1, samp1a) # another sample drawn
  samp2b = setdiff(1:n2, samp2a)
  samp3b = setdiff(1:n3, samp3a)
  samp4b = setdiff(1:n4, samp4a)
  
  # Estimate two Rasch Models => Estimate Wald test
  rr3a = RM.w(X3[samp3a, ], wt3[samp3a]) # Rasch Model
  rr3b = RM.w(X3[samp3b, ], wt3[samp3b])
  Wald = EWaldtest(rr3a$b, rr3b$b, rr3a$se.b, rr3b$se.b)$tab # Wald test
  
  # Output
  Wald         # General output
  # Z-scores by item
  # Assoc. p-values
}
#
#
#
################################################################
## ITEM CHARACTERISTIC CURVES AND PLOTS (under CML)
{
  # ICC.fun(b, plot = FALSE)
  
  # Formatting data
  rrc1 = RM.w(X1,wt1)  # Estimate Rasch models
  rrc2 = RM.w(X2,wt2)
  rrc3 = RM.w(X3,wt3)
  rrc4 = RM.w(X4,wt4)
  b1 = rrc1$b          # Extract parameter estimates
  b2 = rrc2$b
  b3 = rrc3$b
  b4 = rrc4$b
  
  # Obtain ICC values and plot
  icc1 = ICC.fun(b1,plot=FALSE)   # ICC values by item
  # Cols := items, rows := sev.levels
  icc2 = ICC.fun(b1,plot=FALSE)
  icc1 = ICC.fun(b1, plot = TRUE) # ICC plot, formatted
  icc2 = ICC.fun(b2, plot = TRUE)
}
#
#
#
################################################################
# PROBABILISTIC ASSIGN OF CASES TO CLASSES OF SEVERITY IN TRAIT 
{
  # prob.assign(rr = NULL, 
  #              rwthres = NULL, 
  #              sthres = NULL, 
  #              eps.a = NULL, 
  #              flex = list(a=NULL,se.a=NULL, d=NULL, XX=NULL, wt=NULL)
  
  # Formatting the data for estimation
  rrc1 = RM.w(X1,wt1)                        # Estimate Rasch
  
  # Estimating Probabilistic Assignment
  pp1 = prob.assign(rrc1, rwthres = c(3, 7)) # Thresholds on latent trait
  # corresp. to RS's 3&7
  pp1$sprob  # Est. wt. probability, s.t. P(threshold_i < x) for 3&7
  pp1$thres  # Thresholds of trait w.r.t. thresholds by RS specified 
  # in rwthres() option
  pp1$f      # P(threshold_i < x) across RS's 
  pp1$p      # Empirical wgted dist. beyond the RS's specified in rwthres
  pp1$f_j    # Empitical wgted dist. across the RS's
  
  # Table for Prevalence and Thresholds
  tab = cbind("Raw score" = c(3, 7), 
              "Latent trait" = pp1$thres,
              "Prevalence" = colSums(pp1$f))
  rownames(tab) = c("Thres 1","Thres 2")
  tab    # Outputs table Kx3 for thresholds' (k) locations & prev.
  
  # Only 2 extremes
  # Pre-defined thresholds on the latent trait
  sthresh = c(-0.25, 1.81)
  pp1.2 = prob.assign(rrc1, sthres = sthresh)$sprob
  # Probability of being beyond -0.25 on the latent trait in country 1
  pp1.2[1]*100
  # Probability of being beyond 1.81 on the latent trait in country 1
  pp1.2[2]*100
  
  # More than 2 extremes - Fit the model
  rrc1.d = RM.w(X1, wt1, .d = c(0.5, 7.5, 7.7)) 
  # Probabilistic assignment
  pp1.d = prob.assign(rrc1.d, sthres = sthresh)$sprob
  # Probability of being beyond -0.25 on the latent trait in country 1
  # using upper assumption on the extreme raw score parameter 8
  pp1.d[[1]]*100
  # Probability of being beyond -0.25 on the latent trait in country 1
  # using lower assumption on the extreme raw score parameter 8
  pp1.d[[2]]*100
}
#
#
#
################################################################
# RASCH DESCRIPTITVES AND WEIGHTED TABLES
{
  # tab.weight(variab,        # one or more variable; <factor type>
  #           wt,             # Sampling weights s.t.
  #                           # length(variab) = length(wt)
  #           XX = NULL)      # 0/1 response data
  
  # Univariate weighted table
  gen1       = data.FAO_country1$gender
  urbanrural1 = data.FAO_country1$urbanrural
  tab.weight(gen1,wt1)$tab.ext.w   # prints 1x2
  
  # Bivariate weighted table
  tab.weight(list(gen1,urbanrural1),wt1)$tab.ext.w # prints 2x2
  
  # Fit Rasch descriptives
  fit.descr = tab.weight(wt = wt1, 
                         XX = X1)
  
  # Weighted distribution across raw-scores (absolute and relative)
  cbind("Abs.RS distrib." = fit.descr$RS.abs.w,
        "Rel.RS distrib." = fit.descr$RS.rel.w)
  
  # Weighted and unweighted percentage of Yes per item
  cbind("Weighted perc. of Yes" = fit.descr$Perc.Yes.w,
        "Unweighted perc. of Yes" = fit.descr$Perc.Yes)
}
#
#
#












































