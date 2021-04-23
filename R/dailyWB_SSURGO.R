







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
#' @param bufferRadiusMeters spatial buffer (meters) applied to `x` for the lookup of SSURGO data
#' @param ... additional arguments to [`simpleWB`]
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
dailyWB_SSURGO <- function(x, cokeys = NULL, start = 1988, end = 2018, modelDepth = 100, bufferRadiusMeters = 1, ...) {
  
  
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
  
  
  ## TODO: there is no accounting for two components with the same name (e.g. phases)
  # ---> use cokey for internal iteration
  # get SSURGO hydraulic data for select components
  s <- suppressMessages(
    prepare_SSURGO_hydro_data(cokeys = cokeys, max.depth = 100)
  )
  
  # extract required variables
  vars <- c('compname', 'sat', 'fc', 'pwp', 'corrected_depth')
  s <- s$agg[, vars]
  
  # specify thickness and recession coef.
  s$thickness <- s$corrected_depth
  s$a.ss <- 0.1
  
  # daily water balance and moisture state classification
  wb <- dailyWB(s, daily.data, id = 'compname', S_0 = 0.5)
  
  # ## run model
  # # iterate over soils and perform water balance
  # wb.series <- list()
  # for(i in 1:nrow(s$agg)) {
  #   
  #   # current data
  #   d.i <- s$agg[i, ]
  #   series.i <- d.i[['compname']]
  #   
  #   # run water balance
  #   wb <- simpleWB(
  #     PPT = daily.data$DM$prcp..mm.day., 
  #     PET = daily.data$ET$ET.Daily, 
  #     D = daily.data$DM$date, 
  #     thickness = d.i$corrected_depth, 
  #     sat = d.i$sat, 
  #     fc = d.i$fc,
  #     ...
  #   )
  #   
  #   # add series label
  #   wb[['series']] <- series.i
  #   
  #   wb.series[[series.i]] <- wb
  # }
  # 
  # # flatten to DF
  # wb.series <- do.call('rbind', wb.series)
  # 
  # # ## careful!!! this is not well tested
  # # # set series levels
  # # wb.series$series <- factor(wb.series$series, levels=s$agg$compname[gc$order])
  # 
  # ## moisture state classification
  # # join with aggregate data, likely a better way to do this..?
  # wb.series <- merge(wb.series, s$agg, by.x='series', by.y='compname', all.x=TRUE, sort=FALSE)
  # 
  # # classify moisture state
  # wb.series$state <- with(wb.series, estimateSoilMoistureState(VWC, U, sat, fc, pwp))
  # 
  # # months for grouping
  # wb.series$month <- months(wb.series$date, abbreviate = TRUE)
  # wb.series$month <- factor(wb.series$month, levels=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
  # 
  # # weeks for grouping
  # wb.series$week <- factor(format(wb.series$date, '%U'))
  # 
  # # days for grouping
  # wb.series$doy <- factor(format(wb.series$date, '%j'))
  # 
  # ## TODO: need a proper cokey / site ID
  # # convert series to factor for now
  # wb.series$series <- factor(wb.series$series)
  
  
  return(wb)
}





