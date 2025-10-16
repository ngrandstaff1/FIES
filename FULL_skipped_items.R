###############
## APPENDIX: ##
###############

# Hard-code fixes for countries skipping items: DNK, MLT, JPN, NLD, PRI
{
  # Denmark (DNK)
  View(DNK)
  i <- "DNK"
  rm(res)
  temp_w <- subset(get(i), select = c(wt))
  temp <- subset(get(i), select = c(HEALTHY:RUNOUT,WHLDAY))
  res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
  ee1 = equating.fun(res, st = b, tol = 0.4, spec.com1 = c(2:6,8), spec.com2 = c(1:8)) 
  n <- which(equate_FULL$ISO3 == i)
  equate_FULL[n,20] <- ee1$scale
  equate_FULL[n,21] <- ee1$scale
  equate_FULL[n,22] <- ee1$common[]
  equate_FULL[n,23] <- ee1$common[1]
  equate_FULL[n,24] <- ee1$common[2]
  equate_FULL[n,25] <- ee1$common[3]
  equate_FULL[n,26] <- ee1$common[4]
  equate_FULL[n,27] <- ee1$common[5]
  
  equate_FULL[n,29] <- ee1$common[6]
  equate_FULL[n,30] <- ee1$prevs[1]
  equate_FULL[n,31] <- ee1$prevs[2]
  equate_FULL[n,32] <- ee1$cor.comm.items
  equate_FULL[n,33] <- ee1$adj.thres[1]
  equate_FULL[n,34] <- ee1$adj.thres[2]
  
  # Malta (MLT)
  View(MLT)
  i <- "MLT"
  rm(res)
  temp_w <- subset(get(i), select = c(wt))
  temp <- subset(get(i), select = c(HEALTHY:WHLDAY))
  res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
  ee1 = equating.fun(res, st = b, tol = 0.4, spec.com1 = c(2:8), spec.com2 = c(1:8)) 
  n <- which(equate_FULL$ISO3 == i)
  equate_FULL[n,20] <- ee1$scale
  equate_FULL[n,21] <- ee1$scale
  equate_FULL[n,22] <- ee1$common[]
  equate_FULL[n,23] <- ee1$common[1]
  equate_FULL[n,24] <- ee1$common[2]
  equate_FULL[n,25] <- ee1$common[3]
  equate_FULL[n,26] <- ee1$common[4]
  equate_FULL[n,27] <- ee1$common[5]
  
  equate_FULL[n,29] <- ee1$common[6]
  equate_FULL[n,30] <- ee1$prevs[1]
  equate_FULL[n,31] <- ee1$prevs[2]
  equate_FULL[n,32] <- ee1$cor.comm.items
  equate_FULL[n,33] <- ee1$adj.thres[1]
  equate_FULL[n,34] <- ee1$adj.thres[2]
  
  # Japan (JPN)
  View(JPN)
  i <- "JPN"
  rm(res)
  temp_w <- subset(get(i), select = c(wt))
  temp <- subset(get(i), select = c(WORRIED:RUNOUT, WHLDAY))
  res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
  ee1 = equating.fun(res, st = b, tol = 0.4, spec.com1 = c(1:6,8), spec.com2 = c(1:8)) 
  n <- which(equate_FULL$ISO3 == i)
  equate_FULL[n,20] <- ee1$scale
  equate_FULL[n,21] <- ee1$scale
  equate_FULL[n,22] <- ee1$common[]
  equate_FULL[n,23] <- ee1$common[1]
  equate_FULL[n,24] <- ee1$common[2]
  equate_FULL[n,25] <- ee1$common[3]
  equate_FULL[n,26] <- ee1$common[4]
  equate_FULL[n,27] <- ee1$common[5]
  equate_FULL[n,29] <- ee1$common[6]
  equate_FULL[n,30] <- ee1$prevs[1]
  equate_FULL[n,31] <- ee1$prevs[2]
  equate_FULL[n,32] <- ee1$cor.comm.items
  equate_FULL[n,33] <- ee1$adj.thres[1]
  equate_FULL[n,34] <- ee1$adj.thres[2]
  
  # Netherlands (NLD)
  View(NLD)
  i <- "NLD"
  rm(res)
  temp_w <- subset(get(i), select = c(wt))
  temp <- subset(get(i), select = c(WORRIED:FEWFOOD,HUNGRY,WHLDAY))
  res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
  ee1 = equating.fun(res, st = b, tol = 0.4, spec.com1 = c(1,2,3,7,8), spec.com2 = c(1:8)) 
  n <- which(equate_FULL$ISO3 == i)
  equate_FULL[n,20] <- ee1$scale
  equate_FULL[n,21] <- ee1$scale
  equate_FULL[n,22] <- ee1$common[]
  equate_FULL[n,23] <- ee1$common[1]
  equate_FULL[n,24] <- ee1$common[2]
  equate_FULL[n,25] <- ee1$common[3]
  equate_FULL[n,26] <- ee1$common[4]
  equate_FULL[n,27] <- ee1$common[5]
  equate_FULL[n,29] <- ee1$common[6]
  equate_FULL[n,30] <- ee1$prevs[1]
  equate_FULL[n,31] <- ee1$prevs[2]
  equate_FULL[n,32] <- ee1$cor.comm.items
  equate_FULL[n,33] <- ee1$adj.thres[1]
  equate_FULL[n,34] <- ee1$adj.thres[2]
  
  # Puerto Rico (PRI)
  View(PRI)
  i <- "PRI"
  rm(res)
  temp_w <- subset(get(i), select = c(wt))
  temp <- subset(get(i), select = c(WORRIED,FEWFOOD:WHLDAY))
  res <- RM.w(temp, .w = rep(1, nrow(temp_w)))
  ee1 = equating.fun(res, st = b, tol = 0.4, spec.com1 = c(1,3:8), spec.com2 = c(1:8)) 
  n <- which(equate_FULL$ISO3 == i)
  equate_FULL[n,20] <- ee1$scale
  equate_FULL[n,21] <- ee1$scale
  equate_FULL[n,22] <- ee1$common[]
  equate_FULL[n,23] <- ee1$common[1]
  equate_FULL[n,24] <- ee1$common[2]
  equate_FULL[n,25] <- ee1$common[3]
  equate_FULL[n,26] <- ee1$common[4]
  equate_FULL[n,27] <- ee1$common[5]
  equate_FULL[n,29] <- ee1$common[6]
  equate_FULL[n,30] <- ee1$prevs[1]
  equate_FULL[n,31] <- ee1$prevs[2]
  equate_FULL[n,32] <- ee1$cor.comm.items
  equate_FULL[n,33] <- ee1$adj.thres[1]
  equate_FULL[n,34] <- ee1$adj.thres[2]