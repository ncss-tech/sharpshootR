


#' @title A very simple estimation of soil moisture state based on volumetric water content
#' 
#' @description This is a very simple classification of volumetric water content (VWC) into 5 "moisture states", based on an interpretation of water retention thresholds. Classification is performed using VWC at satiation, field capacity (typically 1/3 bar suction), permanent wilting point (typically 15 bar suction), and water surplus in mm. The inputs to this function are closely aligned with the assuptions and output from hydromad::hydromad(sma = 'bucket', ...).
#' 
#' Soil moisture classification rules are as follows:
#'   * VWC <= `pwp`: "very dry"
#'   * VWC > `pwp` and <= (mid-point between `fc` and `pwp`): "dry"
#'   * VWC > (mid-point between `fc` and `pwp`) and <= `fc`: "moist"
#'   * VWC > `fc`: "very moist"
#'   * `U` (surplus) > 2mm: "wet / runoff"
#'
#' @param VWC vector of volumetric water content (VWC), range is 0-1
#' @param U vector of surplus water (mm)
#' @param sat satiation water content, range is 0-1
#' @param fc field capacity water content, range is 0-1
#' @param pwp permanent wilting point water content, range is 0-1
#'
#' @author D.E. Beaudette
#'
#' @return vector of moisture states (ordered factor)
#' 
#' @export
#'
#' @examples
#' 
#' # "very moist"
#' estimateSoilMoistureState(VWC = 0.3, U = 0, sat = 0.35, fc = 0.25, pwp = 0.15)
#' 
estimateSoilMoistureState <- function(VWC, U, sat, fc, pwp) {
  
  # vector of results
  ms <- rep(NA, times=length(VWC))
  
  # dry = midpoint between fc and pwp
  dry.thresh <- (fc + pwp) / 2
  
  # between FC and saturation
  ms[VWC > fc] <- 'very moist'
  
  # between dry and FC
  ms[VWC <= fc & VWC > dry.thresh] <- 'moist'
  
  # dry
  ms[VWC <= dry.thresh & VWC > pwp] <- 'dry'
  
  # at or below PWP
  ms[VWC <= pwp] <- 'very dry'
  
  # surplus > 2mm -> saturation or flooded
  ms[U > 2] <- 'wet / runoff'
  
  # set levels
  ms <- factor(
    ms, 
    levels = c('very dry', 'dry', 'moist', 'very moist', 'wet / runoff'), 
    ordered = TRUE
    )
  
  return(ms)
}



