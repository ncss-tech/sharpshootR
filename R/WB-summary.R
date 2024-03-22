
## TODO: this needs information about PWP, FC, and SAT
## TODO: w must be in sync with the water-year if appropriate (e.g. xeric SMR)

#' @title Water Balance Summaries
#' 
#' @description A summary of a monthly water balance, including estimates of total and consecutive "dry", "moist", "wet" conditions, total surplus, deficit, and AET, and annual AET/PET ratio. 
#' 
#' @note Work in progress: AWC, PWP, FC, and SAT arguments are currently ignored!
#' 
#'
#'

#' @param w used for for `monthlyWB_summary()`: a data.frame, such as result of `monthlyWB()`; 
#' 
#' @param AWC numeric, optional plant-available water storage (mm)
#' @param PWP numeric, optional permanent wilting point (volumetric water content)
#' @param FC numeric, optional field capacity (volumetric water content)
#' @param SAT numeric, optional saturation capacity (volumetric water content)
#' 
#' 
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
monthlyWB_summary <- function(w, AWC = NULL, PWP = NULL, FC = NULL, SAT = NULL) {
  
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
  
  # dry: storage < 1mm
  dry.rules <- w$S < 1
  # moist: storage >= 1mm AND excess < 1mm
  moist.rules <- w$S >= 1 & w$U < 1
  # wet: excess >= 1mm
  wet.rules <- w$U >= 1
  
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

