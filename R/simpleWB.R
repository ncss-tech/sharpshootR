
#' @title Simple interface to the hydromad "leaky bucket" soil moisture model
#' 
#' @description Simple interface to the hydromad "leaky bucket" soil moisture model.
#'
#' @param PPT precipitation series (mm)
#' @param PET potential ET series (mm)
#' @param D dates
#' @param thickness soil thickness (cm)
#' @param sat volumetric water content at saturation (satiated water content)
#' @param fc volumetric water content at field capacity (typically 1/3 bar suction)
#' @param pwp volumetric water content at permanent wilting point (typically 15 bar suction)
#' 
#' @param S_0 initial soil moisture as a fraction of total water storage (mm)
#' @param a.ss recession coefficients for subsurface flow from saturated zone, should be > 0
#' 
#' @param M fraction of area covered by deep-rooted vegetation
#' @param etmult multiplier for PET
#'
#' @details Adjustments for coarse fragments should be made by reducing `thickness`.
#' 
#' @references 
#' 
#' Farmer, D., M. Sivapalan, Farmer, D. (2003). Climate, soil and vegetation controls upon the variability of water balance in temperate and semiarid landscapes: downward approach to water balance analysis. Water Resources Research 39(2), p 1035.
#' 
#' Bai, Y., T. Wagener, P. Reed (2009). A top-down framework for watershed model evaluation and selection under uncertainty. Environmental Modelling and Software 24(8), pp. 901-916.
#' 
#' @return a `data.frame`
#' @export
#'
#' 
## TODO: investigate realistic a.ss values for various drainage classes

## TODO: double-check specification of `Sb.fc`

## source:
# https://github.com/josephguillaume/hydromad/blob/master/R/bucket.R
# https://github.com/josephguillaume/hydromad/blob/master/src/bucket.c

simpleWB <- function(PPT, PET, D, thickness, sat, fc, pwp, S_0 = 0.5, a.ss = 0.05, M = 0, etmult = 1) {
  
  # sanity check: package requirements
  if(!requireNamespace('hydromad'))
    stop('please install the hydromad package', call. = FALSE)
  
  # awc and fc must be within 0-1
  
  # prepare soil hydraulic parameters
  # total water storage (mm) = thickness (cm) * 10 mm/cm * saturated VWC
  Sb <- thickness * 10 * sat
  
  ## TODO: verify this
  # field capacity as a fraction of Sb
  # Model S1/S2 of Bai et al., 2009
  # see Appendix A1
  Sb.fc <- (fc - pwp) / (sat - pwp)
  
  # prep input / output data for model
  z <- data.frame(P = PPT, E = PET)
  
  # init model: leaky-bucket SMA, no routing component
  m <- hydromad::hydromad(z, sma = "bucket", routing = NULL)
  # add soil hydraulic parameters
  m <- update(
    m,
    Sb = Sb,
    fc = Sb.fc,
    S_0 = S_0,
    a.ss = a.ss,
    M = M,
    etmult = etmult,
    a.ei = 0
  )

  # predictions
  res <- predict(m, return_state = TRUE)
  
  # combine date, inputs (z), predictions (res)
  res <- data.frame(
    date = D, 
    z, 
    res
  )
  
  # volumetric water content (VWC)
  # VWC = soil water (mm) / total thickness (mm)
  res$VWC <- res$S / (thickness * 10)
  
  return(res)
}

