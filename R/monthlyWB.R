
## TODO: document assumptions / considerations for monthly vs. daily WB

#' @title Monthly Water Balances
#' 
#' @description Perform a monthly water balance by "leaky bucket" model, provided by the `hydromad` package.
#' 
#' @note This function depends on the \href{http://hydromad.catchment.org/}{hydromad package}.
#' 
#' @author D.E. Beaudette
#' 
#' @param AWC available water-holding capacity (mm), typically thickness (mm) * awc
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
#' @param fc fraction of `AWC` representing field capacity (see details)
#' 
#' @param a.ss recession coefficients for subsurface flow from saturated zone, should be > 0 but very small (see details)
#' 
#' @details At a monthly time step, `fc` and `a.ss` have very little impact on results. See `?bucket.sim` for details.
#' 
#' @references 
#' 
#' Farmer, D., M. Sivapalan, Farmer, D. (2003). Climate, soil and vegetation controls upon the variability of water balance in temperate and semiarid landscapes: downward approach to water balance analysis. Water Resources Research 39(2), p 1035.
#' 
#' @return a `data.frame` with the following elements:
#' 
#' \itemize{
#' \item{PPT: }{monthly PPT values}
#' \item{PET: }{monthly PET values}
#' \item{U: }{monthly U values}
#' \item{S: }{monthly S values}
#' \item{ET: }{monthly ET values}
#' \item{D: }{monthly D values}
#' \item{month: }{month number}
#' \item{mo: }{month label}   
#' }
#' 
#' @examples 
#' 
#' if(requireNamespace('hydromad')) {
#' 
#' # 4" water storage ~ 100mm
#' 
#' # AWC in mm
#' AWC <- 200
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
#' }
#' 
monthlyWB <- function(AWC, PPT, PET, S_init = AWC, starting_month = 1, rep = 1, keep_last = FALSE, fc = 1, a.ss = 0.001) {
  
  # sanity check: package requirements
  if(!requireNamespace('hydromad'))
    stop('please install the hydromad package', call. = FALSE)
  
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
  
  ## Note: not using simpleWB() at this time
  
  # Sb: total water storage (mm), this is the awc at monthly timestep
  # fc field capacity fraction: fraction of Sb, 1 for a monthly timestep seems reasonable
  # S_0 initial moisture content as fraction of Sb 
  # a.ss should always be > 0, but very small at this time step
  m <- hydromad::hydromad(d, sma = "bucket", routing = NULL)
  m <- update(m, Sb = AWC, fc = fc, S_0 = S_init, a.ss = a.ss, M = 0, etmult = 1, a.ei = 0)
  res <- predict(m, return_state = TRUE)
  
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
  
  # reset rownames
  row.names(res) <- NULL
  
  # add original AWC used as an attribute
  attr(res, 'AWC') <- AWC
  
  # done
  return(res)
}
