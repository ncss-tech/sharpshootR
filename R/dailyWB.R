



#' @title Simple Daily Water Balance
#' 
#' @description Simple interface to the hydromad "leaky bucket" soil moisture model, with accommodation for typical inputs from common soil data and climate sources. Critical points along the water retention curve are specified using volumetric water content (VWC): satiation (saturation), field capacity (typically 1/3 bar suction), and permanent wilting point (typically 15 bar suction).
#'
#' @param x `data.frame`, required columns include:
#'   * `sat`: VWC at satiation
#'   * `fc`: VWC at field capacity
#'   * `pwp`: VWC at permanent wilting point
#'   * `thickness`: soil material thickness in cm
#'   * `a.ss`: recession coefficients for subsurface flow from saturated zone, should be > 0 (range: 0-1)
#'   * "id"
#'   
#' @param daily.data `data.frame`, required columns include:
#'    * `date`: `Date` class representation of dates
#'    * `PPT`: daily total, precipitation in mm
#'    * `PET`: daily total, potential ET in mm
#' 
#' @param id character, name of column in `x` that is used to identify records
#' 
#' @param MS.style moisture state classification style, see [`estimateSoilMoistureState`]
#' 
#' @param S_0 fraction of water storage filled at time = 0 (range: 0-1)
#'
#' @param M fraction of area covered by deep-rooted vegetation
#' @param etmult multiplier for PET
#'
#' @return a `data.frame`
#' 
#' @references 
#' 
#' Farmer, D., M. Sivapalan, Farmer, D. (2003). Climate, soil and vegetation controls upon the variability of water balance in temperate and semiarid landscapes: downward approach to water balance analysis. Water Resources Research 39(2), p 1035.
#' 
#' Bai, Y., T. Wagener, P. Reed (2009). A top-down framework for watershed model evaluation and selection under uncertainty. Environmental Modelling and Software 24(8), pp. 901-916.
#' 
#' @export
#'
dailyWB <- function(x, daily.data, id, MS.style = 'default', S_0 = 0.5, M = 0, etmult = 1) {
  
  # sanity check: package requirements
  if(!requireNamespace('hydromad'))
    stop('please install the hydromad package', call. = FALSE)
  
  # sanity checks
  stopifnot(inherits(x, 'data.frame'))
  stopifnot(inherits(daily.data, 'data.frame'))
  
  # required variables
  req.vars.x <- c(id, 'sat', 'fc', 'pwp', 'thickness', 'a.ss')
  req.vars.dd <- c('date', 'PPT', 'PET')
  
  if(!all(req.vars.x %in% names(x))) {
    stop(sprintf('`x` must contain columns: %s', paste(req.vars.x, collapse = ', ')))
  }
  
  if(!all(req.vars.dd %in% names(daily.data))) {
    stop(sprintf('`daily.data` must contain columns: %s', paste(req.vars.dd, collapse = ', ')))
  }
  
  # records are defined by rows in `x`
  z <- lapply(1:nrow(x), function(i) {
    
    # current record
    x.i <- x[i, ]
    
    wb <- simpleWB(
      PPT = daily.data$PPT, 
      PET = daily.data$PET, 
      D = daily.data$date, 
      thickness = x.i$thickness, 
      sat = x.i$sat, 
      fc = x.i$fc, 
      a.ss = x.i$a.ss,
      S_0 = S_0
    )
    
    # add contextual data
    wb[[id]] <- factor(x.i[[id]])
    wb$sat <- x.i$sat
    wb$fc <- x.i$fc
    wb$pwp <- x.i$pwp
    
    # possibly merge daily.data for context?
    
    return(wb)
  })
  
  z <- do.call('rbind', z)
  
  # moisture state, based on interpretation of water retention curve
  z$state <- with(z, estimateSoilMoistureState(VWC, U, sat, fc, pwp, style = MS.style))
  
  # months for grouping
  z$month <- months(z$date, abbreviate = TRUE)
  z$month <- factor(
    z$month, 
    levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  )
  
  # years for grouping
  z$year <- factor(format(z$date, '%Y'))
  
  # weeks for grouping
  z$week <- factor(format(z$date, '%U'))
  
  # days for grouping
  z$doy <- factor(format(z$date, '%j'))
  
  return(z)
  
}
