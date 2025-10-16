####################################
## FIES Coding in R - CML Methods ## 
####################################
# Source: Cafiero, Viviani, Nord (2018)

## PRELIMINARIES
{
## Add relevant packages (RM.weights and Rtools)
library(RM.weights)
library(haven)      
library(expss)
library(ggplot2)
library(dplyr)
setwd("C:\\Users\\ngran\\Box\\JHodd - FIES\\R Code")

## Upload data
load("RM.weights/data/data.FAO_country1.rda")
load("RM.weights/data/data.FAO_country2.rda")
load("RM.weights/data/data.FAO_country3.rda")
load("RM.weights/data/data.FAO_country4.rda")

## Create data subsets
# Questions block
X1 = data.FAO_country1[,1:8]
X2 = data.FAO_country2[,1:8]
X3 = data.FAO_country3[,1:8]
X4 = data.FAO_country4[,1:8]
# Weights block
wt1 = data.FAO_country1$wt
wt2 = data.FAO_country2$wt
wt3 = data.FAO_country3$wt
wt4 = data.FAO_country4$wt

## Estimating weighted Rasch Model by country
# RM.w(.data, .w = NULL, .d=NULL, country=NULL, se.control = TRUE, quantile.seq = NULL, write.file = FALSE, max.it=100)
rrc1 = RM.w(X1,wt1)
rrc2 = RM.w(X2,wt2)
rrc3 = RM.w(X3,wt3)
rrc4 = RM.w(X4,wt4)
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
                   spec.com2 = c(1:8))    # Unique items, Std.
                                          # Std. items

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

















