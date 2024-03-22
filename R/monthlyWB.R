
## TODO: document asymptotic behavior with distribute = TRUE, and k -> 100

## TODO: allow specification of M argument to bucket.sim

## TODO: allow specification of field capacity

#' @title Monthly Water Balances
#' 
#' @description Perform a monthly water balance by "leaky bucket" model, inspired by code from `bucket.sim` of `hydromad` package, as defined in Bai et al., (2009) (model "SMA_S1"). The plant available water-holding storage (soil thickness * awc) is used as the "bucket capacity". All water in excess of this capacity is lumped into a single "surplus" term.
#' 
#' @details See the [monthly water balance tutorial](http://ncss-tech.github.io/AQP/sharpshootR/monthly-WB.html) for further examples and discussion.
#' 
#' A number of important assumptions are made by this style of water balance modeling:
#'    * the concept of field capacity is built into the specified bucket size
#'    * the influence of aquitards or local terrain cannot be integrated into this model
#'    * interception is not used in this model
#' 
#' @param AWC numeric, available water-holding capacity (mm), typically thickness (mm) * awc (fraction)
#' 
#' @param PPT numeric, time-series of monthly PPT (mm), calendar year ordering
#' 
#' @param PET numeric, time-series of monthly PET (mm), calendar year ordering
#' 
#' @param S_init numeric, initial fraction of `AWC` filled with water (values 0-1)
#' 
#' @param starting_month integer, starting month index, 1=January, 9=September
#' 
#' @param rep integer, number of cycles to run water balance
#' 
#' @param keep_last logical, keep only the last iteration of the water balance
#' 
#' @param distribute logical, distribute monthly data into `k` divisions within each month
#' 
#' @param method method for distributing PPT and PET into `k` divisions:
#'   * 'equal' divides PPT and PET into `k` equal amounts
#'   * 'random' divides PPT and PET into random proportions generated via multinominal simulation
#'   * 'gaussian' divides PPT and PET according to a bell-shaped curve centered in the middle of each month
#' 
#' @param k integer, number of divisions
#' 
#' 
#' @references 
#' 
#' Arkley R, Ulrich R. 1962. The use of calculated actual and potential evapotranspiration for estimating potential plant growth. Hilgardia 32(10):443-469.
#' 
#' Bai, Y., T. Wagener, P. Reed (2009). A top-down framework for watershed model evaluation and selection under uncertainty. Environmental Modelling and Software 24(8), pp. 901-916.
#' 
#' Farmer, D., M. Sivapalan, Farmer, D. (2003). Climate, soil and vegetation controls upon the variability of water balance in temperate and semiarid landscapes: downward approach to water balance analysis. Water Resources Research 39(2), p 1035.
#' 
#' @return a `data.frame` with the following elements:
#' 
#' \itemize{
#' \item{PPT: }{monthly PPT (mm)}
#' \item{PET: }{monthly PET (mm)}
#' \item{U: }{monthly surplus (mm)}
#' \item{S: }{monthly soil moisture storage (mm)}
#' \item{ET: }{monthly AET (mm)}
#' \item{D: }{monthly deficit (mm)}
#' \item{month: }{month number}
#' \item{mo: }{month label}   
#' }
#' 
monthlyWB <- function(AWC, PPT, PET, S_init = 1, starting_month = 1, rep = 1, keep_last = FALSE, distribute = FALSE, method = c('equal', 'random', 'gaussian'), k = 10) {
  
  # sanity check
  method <- match.arg(method)
  
  # number of time steps in the original series
  n <- length(PPT)
  
  # re-order monthly data according to starting month
  if(starting_month == 1) {
    idx <- seq(from = starting_month, to = 12, by = 1)
  } else {
    idx <- c(seq(from = starting_month, to = 12, by = 1), seq(from = 1, to = (starting_month - 1), by = 1))
  }
  
  # replicate as needed
  idx <- rep(idx, times = rep)
  
  # re-index months as needed
  PPT <- PPT[idx]
  PET <- PET[idx]
  
  # combine into format suitable for simulation
  # .sim keeps track of cycle
  d <- data.frame(P = PPT, E = PET, .sim = rep(1:rep, each = 12))
  
  # add month index
  d$month <- idx
  .months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  d$mo <- .months[idx]
  
  # convert to factor for proper sorting by split(), later when `distribute = TRUE`
  d$mo <- factor(d$mo, levels = .months[idx][1:12])
  
  # optionally spread-out PPT and PET over k bins within a month
  if(distribute) {
    # rows represent months
    # there may be more than one simulation cycle
    # i: current cycle/month
    dd <- lapply(1:nrow(d), function(i, .k = k) {
      
      # replicate row `i` within original data
      .idx <- rep(i, times = .k)
      .dr <- d[.idx, ]
      
      # evenly distribute PPT and PET over k bins
      if(method == 'equal') {
        .dr$P <- .dr$P / .k
        .dr$E <- .dr$E / .k
      }
      
      # multinominal simulation of proportions across k bins
      # maybe more variability than expected...
      if(method == 'random') {
        
        # simulate k-proportions with equal probability
        .p <- rep(1, times = .k)
        
        # PPT
        P.prop <- as.vector(rmultinom(n = 1, size = .k, prob = .p))
        P.prop <- P.prop / sum(P.prop)
        .dr$P <- P.prop * .dr$P[1]
        
        # PET
        E.prop <- as.vector(rmultinom(n = 1, size = .k, prob = .p))
        E.prop <- E.prop / sum(E.prop)
        .dr$E <- E.prop * .dr$E[1]
      }
      
      # distribute total PPT and PET following a Gaussian curve, centered at mean(1:k)
      if(method == 'gaussian') {
        
        # use Gaussian proportions, peaking at center of k-bins
        # adjust steepness of peak with `sd` argument
        k.s <- 1:.k
        .p <- dnorm(k.s, mean = mean(k.s), sd = .k/5)
        .p <- .p / sum(.p)
        
        # distribute PPT
        # all the same until overwritten
        .dr$P <- .p * .dr$P[1]
        
        # distribute PET
        # all the same until overwritten
        .dr$E <- .p * .dr$E[1]
      }
      
      
      ## TODO: additional method using constant +/- fuzz
      
      
      # keep track of simulation cycle
      .dr$.sim <- .dr$.sim[1]
      
      return(.dr)
    }
    )
    
    d <- do.call('rbind', dd)
    
  }
  
  
  
  ## hydromad interface
  # note that first two columns are P, E
  
  ## TODO: check these assumptions:
  # at the monthly time-step: a.ss = 0.01
  # at the monthly time-step: M = 0
  
  m <- hydromad::hydromad(d[, 1:2], sma = "bucket", routing = NULL)
  m <- update(m, Sb = AWC, fc = 1, S_0 = S_init, a.ss = 0.01, M = 0, etmult = 1, a.ei = 0)
  res <- predict(m, return_state = TRUE)
  
  # ## custom implementation of "model S2" from Bai et al., 2009
  # # soil moisture accounting "SMA_S2"
  # # routing "R1"
  # # SMA_S2 + R1 
  # #
  # # important change:
  # # ET[t] <- Eintc + min(S[t], (Etrans + Ebare))
  # res <- .monthlyBucket(d, Sb = AWC, S_0 = S_init, fc = 1, a.ss = 0.01, M = 0, etmult = 1, a.ei = 0)
  # 
  
  # combine original PPT, PET with results
  res <- data.frame(d, res)
  
  # cleanup names
  names(res) <- c('PPT', 'PET', '.sim', 'month', 'mo', 'U', 'S', 'ET')
  
  # compute deficit: AET - PET
  res$D <- with(res, ET - PET)
  
  # re-arrange
  res <- res[, c('PPT', 'PET', 'U', 'S', 'ET', 'D', 'month', 'mo', '.sim')]
  
  if(distribute) {
    # flatten n simulations * k bins per month -> 12 months / simulation
    # names and ordering as: sim.month
    s <- split(res, list(res$.sim, res$mo), lex.order = TRUE)
    
    s <- lapply(s, function(i, .k = k) {
      
      # re-assemble into data.frame
      .res <- data.frame(
        # total PPT, PET, U
        PPT = sum(i$PPT),
        PET = sum(i$PET),
        U = sum(i$U),
        # final storage value
        S = i$S[.k],
        # total AET
        ET = sum(i$ET),
        # compute D last
        D = 0,
        # these are constant, keep first
        month = i$month[1],
        mo = i$mo[1],
        .sim = i$.sim[1]
      )
      
      return(.res)
    })
    
    res <- do.call('rbind', s)
    
    # compute Deficit after summing PPT and ET separately 
    # deficit is a negative value
    res$D <- res$ET - res$PET
  }
  
  
  # check overall water balance for missing mass
  # rounding to 0.01 mm should be sufficient precision
  #
  # P = U + ET + dS
  # for all time steps: .in = .out + dS
  # final mass balance: sum(.in - (.out + .dS))
  
  # PPT
  .in <- res$PPT
  
  # AET + U
  .out <- res$ET + res$U
  
  # dS
  # more complicated than seems, when S_init < 1
  # S at time 0 = AWC * S_init 
  .dS <- diff(c(AWC * S_init, res$S))
  
  # mass balance
  .mb <- sum(.in - (.out + .dS))
  
  # save for later use
  attr(res, 'mass.balance') <- round(.mb, 0.01)
  
  ## TODO: think about what an error in the mass balance means...
  if(abs(.mb) > 5) {
    message('Mass balance not closed')
  }
  
  # optionally keep the last simulation cycle
  .last <- max(res$.sim)
  if(keep_last) {
    res <- res[which(res$.sim == .last), ]
  }
  
  # reset row names
  row.names(res) <- NULL
  
  # add original AWC used as an attribute
  attr(res, 'AWC') <- AWC
  
  # done
  return(res)
}


