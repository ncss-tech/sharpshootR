# code based on R implementation of bucket.sim()
# https://github.com/josephguillaume/hydromad/blob/master/R/bucket.R


## identification of bug, testing
# https://github.com/ncss-tech/sharpshootR/issues/40

## small bug in accumulation of Et
# https://github.com/josephguillaume/hydromad/issues/188 [closed]


## local implementation of bucket.sim

# with change:
# ET[t] <- Eintc + min(S[t], (Etrans + Ebare))

## this is "model S2" from Bai et al., 2009
# soil moisture accounting "SMA_S2"
# routing "R1"
# SMA_S2 + R1 

## assumptions:
# at a monthly time-step, we reach field capacity so Sb -> AWS and a.ss has very little impact

.monthlyBucket <- function(DATA, Sb, S_0, fc = 1, a.ss = 0.01, M = 0, etmult = 1, a.ei = 0) {
  
  ## fc is expressed as a proportion of Sb
  Sfc <- fc * Sb
  ## as is S_0
  S_0 <- S_0 * Sb
  
  ## copy attributes
  inAttr <- attributes(DATA[, 1])
  
  ## convert to ts obj
  DATA <- as.ts(DATA)
  
  # local copies and ET multiplier
  P <- DATA[, "P"]
  E <- DATA[, "E"] * etmult
  
  ## TODO: test this
  ## skip over missing values (maintaining the state S)
  bad <- is.na(P) | is.na(E)
  P[bad] <- 0
  E[bad] <- 0
  
  ## this is the R implementation of C version
  # https://github.com/josephguillaume/hydromad/blob/master/src/bucket.c
  
  ## TODO: make explicit: vector(mode = 'numeric', length = length(P))
  # init vectors
  U <- S <- ET <- P
  
  ## TODO: consider matrix math and shifting rows
  # maintain state with last iteration
  S_prev <- S_0
  
  # iteration over time
  for (t in seq(1, length(P))) {
    
    ## evapo-transpiration
    Eintc <- a.ei * P[t]
    S[t] <- min(Sb, S_prev + P[t] - Eintc)
    Etrans <- M * min(1, S[t] / Sfc) * E[t]
    Ebare <- (1 - M) * (S[t] / Sb) * E[t]
    
    # 2021-05-03
    # original:
    # ET[t] <- Eintc + Etrans + Ebare
    
    # DEB: (ET_trans + ETbare) cannot exceed S[t]
    ET[t] <- Eintc + min(S[t], (Etrans + Ebare))
    
    ## mass balance
    S[t] <- max(0, S_prev + P[t] - ET[t])
    
    ## drainage (saturation excess)
    Use <- max(0, S[t] - Sb)
    S[t] <- max(0, S[t] - Use)
    
    ## drainage (sub-surface)
    Uss <- max(0, a.ss * (S[t] - Sfc))
    S[t] <- max(0, S[t] - Uss)
    U[t] <- Use + Uss
    
    S_prev <- S[t]
  }
  
  ## re-insert attributes
  attributes(U) <- inAttr
  
  ## re-insert missing values
  U[bad] <- NA
  
  attributes(S) <- attributes(ET) <- attributes(U)
  ans <- cbind(U = U, S = S, ET = ET)
  
  return(ans)
}

