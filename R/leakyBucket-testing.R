# https://github.com/josephguillaume/hydromad/issues/188

# internal version, based on 
# https://github.com/josephguillaume/hydromad/blob/master/R/bucket.R
# with change:
# ET[t] <- Eintc + min(S[t], Etrans) + min(S[t], Ebare)

.leakyBucket <- function(DATA, Sb, fc, S_0, a.ss, M, etmult, a.ei) {
  
  return_state <- TRUE
  
  ## fc is expressed as a proportion of Sb
  Sfc <- fc * Sb
  ## as is S_0
  S_0 <- S_0 * Sb
  
  inAttr <- attributes(DATA[, 1])
  DATA <- as.ts(DATA)
  P <- DATA[, "P"]
  E <- DATA[, "E"] * etmult
  
  ## skip over missing values (maintaining the state S)
  bad <- is.na(P) | is.na(E)
  P[bad] <- 0
  E[bad] <- 0
  
  ## implementation in R for cross-checking (slow)
  U <- S <- ET <- P
  S_prev <- S_0
  for (t in seq(1, length(P))) {
    ## evapo-transpiration
    Eintc <- a.ei * P[t]
    S[t] <- min(Sb, S_prev + P[t] - Eintc)
    Etrans <- M * min(1, S[t] / Sfc) * E[t]
    Ebare <- (1 - M) * (S[t] / Sb) * E[t]
    # DEB: modified
    ET[t] <- Eintc + min(S[t], Etrans) + min(S[t], Ebare)
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
  
  attributes(U) <- inAttr
  ## re-insert missing values
  U[bad] <- NA
  ans <- U
  if (return_state) {
    attributes(S) <- attributes(ET) <- attributes(U)
    ans <- cbind(U = U, S = S, ET = ET)
  }
  
  return(ans)
}

