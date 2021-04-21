## leaky bucket model
# PPT: precipitation series
# PET: potential ET series
# D: Dates
# Sb: total storage in mm (satiated WC * thickness * soil_fraction)
# fc: field capacity fraction
# thick_mm: effective thickness for computing VWC
# ...: additional arguments to model



#' Title
#'
#' @param PPT precipitation series (mm)
#' @param PET potential ET series (mm)
#' @param D dates (???)
#' @param thickness soil thickness (cm)
#' @param sat volumeric water content at saturation (satiated water content)
#' @param fc volumetric water content at field capacity (typically 1/3 bar suction)
#' 
#' @param S_0
#' @param a.ss
#'
#' @details If `awc` has not been adjusted for coarse fragments, then the adjustment can be made to `thickness`.
#'
#' @return
#' @export
#'
#' @examples
simpleWB <- function(PPT, PET, D, thickness, sat, fc, S_0 = 0.5, a.ss = 0.03) {
  
  # sanity checks:
  
  # awc and fc must be within 0-1
  
  # prepare soil hydraulic parameters
  # total water storage (mm) = thickness (cm) * 10 mm/cm * saturated VWC
  Sb <- thickness * 10 * sat
  
  # field capacity as a fraction of Sb
  # Sb.fc = field capacity AWC / saturated VWC 
  Sb.fc <- fc / sat
  
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
    M = 0, 
    etmult = 1, 
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


# 
# ## daily functions currently use this... abstract into simpleWB() above
# 
# ## leaky bucket model
# # PPT: precipitation series
# # PET: potential ET series
# # D: Dates
# # Sb: total storage in mm (satiated WC * thickness * soil_fraction)
# # fc: field capacity fraction
# # thick_mm: effective thickness for computing VWC
# # ...: additional arguments to model
# .simpleWB <- function(PPT, PET, D, Sb, fc, thick_mm, ...) {
#   
#   # prep input / output data for model
#   z <- data.frame(P = PPT, E = PET)
#   
#   # init model
#   m <- hydromad::hydromad(z, sma = "bucket", routing = NULL)
#   m <- update(m, Sb = Sb, fc = fc, ...)
#   
#   # predictions
#   res <- predict(m, return_state = TRUE)
#   # combine date, inputs (z), predictions (res)
#   res <- data.frame(
#     date = D, 
#     z, 
#     res
#   )
#   
#   # rough approximation of VWC
#   res$VWC <- res$S / thick_mm
#   
#   return(res)
#   
# }

