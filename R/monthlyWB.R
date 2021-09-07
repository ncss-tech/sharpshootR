
#' @title Monthly Water Balances
#' 
#' @description Perform a monthly water balance by "leaky bucket" model, inspired by code from `bucket.sim` of `hydromad` package, as defined in Bai et al., (2009) (model "SMA_S1"). The plant available water-holding storage (soil thickness * awc) is used as the "bucket capacity". All water in excess of this capacity is lumped into a single "surplus" term.
#' 
#' @details A number of important assumptions are made by this approach:
#'    * the concept of field capacity is built into the specified bucket size
#'    * the influence of aquitards or local terrain cannot be integrated into this model
#'    * interception is not used in this model
#' 
#' @param AWC available water-holding capacity (mm), typically thickness (mm) * awc (fraction)
#' 
#' @param PPT time-series of monthly PPT (mm), calendar year ordering
#' 
#' @param PET time-series of monthly PET (mm), calendar year ordering
#' 
#' @param S_init initial fraction of `AWC` filled with water
#' 
#' @param starting_month starting month index, 1=January, 9=September
#' 
#' @param rep number of cycles to run water balance
#' 
#' @param keep_last keep only the last iteration of the water balance
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
#' 
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
#' @examples 
#' 
#' # 4" water storage ~ 100mm
#' 
#' # AWC in mm
#' AWC <- 100
#' 
#' # monthly PET and PPT in mm
#' PET <- c(0,0,5,80,90,120,130,140,110,90,20,5)
#' PPT <- c(0, 150, 200, 120, 20, 0, 0, 0, 10, 20, 30, 60)
#' 
#' # run water balance
#' # start with soil AWC "empty"
#' (x.wb <- monthlyWB(AWC, PPT, PET, S_init = 0))
#' 
#' # plot the results
#' op <- par(no.readonly = TRUE)
#' 
#' par(mar=c(4,4,2,1), bg = 'white')
#' plotWB(WB = x.wb)
#' 
#' # compute fraction of AWC filled after the last month of simulation
#' (last.S <- x.wb$S[12] / AWC)
#' 
#' # re-run the water balance with this value
#' (x.wb <- monthlyWB(AWC, PPT, PET, S_init = last.S))
#' 
#' # interesting...
#' par(mar=c(4,4,2,1), bg = 'white')
#' plotWB(WB = x.wb)
#' 
#' # note: consider using `rep = 3, keep_last = TRUE` 
#' # to "warm-up" the water balance first
#' 
#' par(op)
#' 
#' 
monthlyWB <- function(AWC, PPT, PET, S_init = AWC, starting_month = 1, rep = 1, keep_last = FALSE) {
  
  # number of time steps in the original series
  n <- length(PPT)
  
  # re-order monthly data according to starting month
  if(starting_month == 1) {
    idx <- seq(from=starting_month, to=12, by = 1)
  } else {
    idx <- c(seq(from=starting_month, to=12, by=1), seq(from=1, to=(starting_month - 1), by=1))
  }
  
  # replicate as needed
  idx <- rep(idx, times=rep)
  
  # re-index months as needed
  PPT <- PPT[idx]
  PET <- PET[idx]
  
  # combine into format suitable for simulation
  d <- data.frame(P = PPT, E = PET)
  
  ## hydromad interface
  m <- hydromad::hydromad(d, sma = "bucket", routing = NULL)
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
  
  # combine original PPT,PET with results
  res <- data.frame(d, res)
  
  # cleanup names
  names(res) <- c('PPT', 'PET', 'U', 'S', 'ET')
  # compute deficit: AET - PET
  res$D <- with(res, ET - PET)
  
  # add month index
  res$month <- idx
  res$mo <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')[idx]
  
  # optionally keep the last cycle
  if(keep_last) {
    keep.idx <- seq(from = nrow(res) - (n-1), to = nrow(res), by = 1)
    res <- res[keep.idx, ]
  }
  
  # reset row names
  row.names(res) <- NULL
  
  # add original AWC used as an attribute
  attr(res, 'AWC') <- AWC
  
  # done
  return(res)
}
