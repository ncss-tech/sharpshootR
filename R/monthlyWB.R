## TODO: need a monthly + daily WB summary

## TODO: maybe not a bug: https://github.com/josephguillaume/hydromad/issues/190

## TODO: document asymptotic behavior with distribute = TRUE, and k -> 100

## TODO: allow specification of M argument to bucket.sim


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
monthlyWB <- function(AWC, PPT, PET, S_init = 1, starting_month = 1, rep = 1, keep_last = FALSE, distribute = FALSE, method = c('equal', 'random'), k = 10) {
  
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
    dd <- lapply(1:nrow(d), function(i, .k = k) {
      
      # replicate original data
      .idx <- rep(i, times = .k)
      .dr <- d[.idx, ]
      
      # evenly distribute PPT and PET over k bins
      if(method == 'equal') {
        .dr$P <- .dr$P / .k
        .dr$E <- .dr$E / .k
      }
      
      # multinominal simulation of proportions across k bins
      if(method == 'random') {
        
        ## TODO: this can lead to some unexpected results
        ##       --> double-check proportions
        
        # simulate k-proportions with equal probability
        
        # PPT
        P.prop <- as.vector(rmultinom(n = 1, size = .k, prob = rep(1, times = .k)))
        P.prop <- P.prop / sum(P.prop)
        .dr$P <- P.prop * .dr$P[1]
        
        # PET
        E.prop <- as.vector(rmultinom(n = 1, size = .k, prob = rep(1, times = .k)))
        E.prop <- E.prop / sum(E.prop)
        .dr$E <- E.prop * .dr$E[1]
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
    
    ## TODO: investigate deficit beyond AWC in some cases
    
    # compute Deficit after summing PPT and ET separately 
    # deficit is a negative value
    res$D <- res$ET - res$PET
  }
  
  
  # optionally keep the last simulation cycle
  .last <- max(res$.sim)
  if(keep_last) {
    # keep.idx <- seq(from = nrow(res) - (n-1), to = nrow(res), by = 1)
    res <- res[which(res$.sim == .last), ]
  }
  
  # reset row names
  row.names(res) <- NULL
  
  # add original AWC used as an attribute
  attr(res, 'AWC') <- AWC
  
  # done
  return(res)
}

## TODO: this needs information about PWP, FC, and SAT
## TODO: w must be in sync with the water-year if appropriate (e.g. xeric SMR)

#' @param w used for for `monthlyWB_summary()`: a data.frame, such as result of `monthlyWB()`; 
#' @rdname monthlyWB
#' @return `monthlyWB_summary()`: a `data.frame` containing:
#' 
#'   * cumulative (`dry`, `moist`, `wet`) days 
#'   * consecutive (`dry_con`, `moist_con`, `wet_con`) days 
#'   * total deficit (`total_deficit`) in mm
#'   * total surplus (`total_surplus`) in mm
#'   * total actual evapotranspiration (`total_AET`) in mm
#'   * annual actual evapotranspiration to potential evapotranspiration ratio (`annual_AET_PET_ratio`)
#' 
#' @export
monthlyWB_summary <- function(w) {
  
  # convert months -> days
  .months2days <- function(m) {
    round(m * (365.25 / 12))
  }
  
  # get count of max consecutive days where condition is TRUE
  # m: RLE object
  .rle_max_true <- function(m) {
    
    # index to TRUE condition
    # may not be present
    idx <- which(m$values)
    
    # if there was a TRUE condition
    if(length(idx) > 0) {
      # return max consecutive days
      res <- max(m$lengths[idx])
    } else {
      # otherwise 0 days
      res <- 0
    }
    
    return(res)
  }
  
  
  ## rough estimate of soil moisture states
  
  # dry: storage < 0.1mm
  dry.rules <- w$S < 0.1
  # moist: storage >= 0.1mm AND excess < 0.1mm
  moist.rules <- w$S >= 0.1 & w$U < 0.1
  # wet: excess >= 0.1m
  wet.rules <- w$U >= 0.1
  
  ## months at given states
  .dry <- which(dry.rules)
  .moist <- which(moist.rules)
  .wet <- which(wet.rules)
  
  ## RLE of states
  .dry_conn <- rle(dry.rules)
  .moist_conn <- rle(moist.rules)
  .wet_conn <- rle(wet.rules)
  
  ## consecutive summary
  res.consecutive <- data.frame(
    dry_con = .months2days(.rle_max_true(.dry_conn)),
    moist_con = .months2days(.rle_max_true(.moist_conn)),
    wet_con = .months2days(.rle_max_true(.wet_conn))
  )
  
  ## cumulative summary
  res.cumulative <- data.frame(
    dry = .months2days(length(.dry)),
    moist = .months2days(length(.moist)),
    wet = .months2days(length(.wet))
  )
  
  ## combine
  res <- data.frame(
    res.cumulative, 
    res.consecutive, 
    total_deficit = sum(w$D), 
    total_surplus = sum(w$U), 
    total_AET = sum(w$ET),
    annual_AET_PET_ratio = sum(w$ET) / sum(w$PET)
  )
  
  return(res)
}

