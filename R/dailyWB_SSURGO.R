
# x: SpatialPoint with single feature
# bufferRadiusMeters: radius in meters
.getSSURGO_at_point <- function(x, bufferRadiusMeters) {
  
  
  # SSURGO data
  # transform to planar coordinate system for buffering
  x.aea <- spTransform(x, CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs '))
  
  # buffer
  x.aea <- rgeos::gBuffer(x.aea, width = bufferRadiusMeters)
  # transform back to WGS84 GCS
  x.buff <- spTransform(x.aea, CRS('+proj=longlat +datum=WGS84'))
  # convert to WKT
  x.wkt <- rgeos::writeWKT(x.buff)
  
  q <- paste0("SELECT mapunit.mukey, cokey, comppct_r, compkind, compname
FROM 
mapunit
JOIN component ON mapunit.mukey = component.mukey
WHERE 
majcompflag = 'Yes'
AND mapunit.mukey IN (
SELECT * from SDA_Get_Mukey_from_intersection_with_WktWgs84('", x.wkt, "')
)")
  
  res <- suppressMessages(SDA_query(q))
  
  # only unique instances of each component
  cokeys <- unique(res$cokey)
  
  return(cokeys)
}








#' @title Perform daily water balance modeling using SSURGO and DAYMET
#' 
#'
#' @param x `SpatialPoints` object representing a single point
#' @param cokeys vector of component keys to use
#' @param start starting year (limited to DAYMET holdings)
#' @param end ending year (limited to DAYMET holdings)
#' @param modelDepth soil depth used for water balance, see details
#' @param MS.style moisture state classification style, see [`estimateSoilMoistureState`]
#' @param a.ss recession coefficients for subsurface flow from saturated zone, should be > 0 (range: 0-1)
#' @param S_0 fraction of water storage filled at time = 0 (range: 0-1)
#' @param bufferRadiusMeters spatial buffer (meters) applied to `x` for the look-up of SSURGO data
#' 
#' 
#' @author D.E. Beaudette
#' 
#' @references 
#' 
#' Farmer, D., M. Sivapalan, Farmer, D. (2003). Climate, soil and vegetation controls upon the variability of water balance in temperate and semiarid landscapes: downward approach to water balance analysis. Water Resources Research 39(2), p 1035.
#' 
#' 
#' 
#' @return `data.frame` of daily water balance results
#' 
#' @export
#'
dailyWB_SSURGO <- function(x, cokeys = NULL, start = 1988, end = 2018, modelDepth = 100, MS.style = 'default', a.ss = 0.1, S_0 = 0.5, bufferRadiusMeters = 1) {
  
  # required packages
  if(!requireNamespace('daymetr', quietly=TRUE) |
     !requireNamespace('elevatr', quietly=TRUE) |
     !requireNamespace('Evapotranspiration', quietly=TRUE) |
     !requireNamespace('rgeos', quietly=TRUE)
  ) {
    stop('this function requires the following packages: daymetr, elevatr, Evapotranspiration', call.=FALSE)
  }
  
  ## TODO: relax constraints: other object types / iteration over features
  # sanity checks
  stopifnot(class(x) == 'SpatialPoints')
  stopifnot(length(x) == 1)
  
  ## TODO: this contains a lot more data than we actually need
  # get daily input data as list
  daily.data <- prepareDailyClimateData(x, start = start, end = end, onlyWB = TRUE)
  
  # use component keys at `x` if none provided
  if(is.null(cokeys)) {
    cokeys <- .getSSURGO_at_point(x, bufferRadiusMeters = bufferRadiusMeters)
  }
  
  
  # get SSURGO hydraulic data for select components
  s <- suppressMessages(
    prepare_SSURGO_hydro_data(cokeys = cokeys, max.depth = modelDepth)
  )
  
  # extract required variables
  vars <- c('compname', 'sat', 'fc', 'pwp', 'corrected_depth')
  s <- s$agg[, vars]
  
  ## TODO: more flexible / intelligent specification of a.ss and S_0
  #        * interpretation of drainage class
  #        * empirical values from SCAN network
  
  # soil-specific thickness and recession coef.
  s$thickness <- s$corrected_depth
  s$a.ss <- a.ss
  
  # daily water balance and moisture state classification
  wb <- dailyWB(s, daily.data, id = 'compname', S_0 = S_0, MS.style = MS.style)
  
  return(wb)
}





