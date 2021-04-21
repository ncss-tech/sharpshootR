


# d: DAYMET data with Date added
# elevation: elevation in meters
.estimatePET <- function(d, elevation) {
  
  # compile into zoo objects
  Tmax <- zoo::zoo(d$tmax..deg.c., d$date)
  Tmin <- zoo::zoo(d$tmin..deg.c., d$date)
  
  # daily total SRAD in MJ/sq.m
  # https://daymet.ornl.gov/overview
  Rs <- zoo::zoo(d$srad..W.m.2. * d$dayl..s. / 1e6, d$date)
  
  # compile into expected input format
  climate.data <- list(
    Date.daily = d$date, 
    Tmax = Tmax, 
    Tmin = Tmin, 
    Rs = Rs
  )
  
  # safe way to load package data
  # note: this is incompatible with LazyData: true
  constants <- NULL
  # whoa: non-standard file naming...
  load(system.file("data/constants.RData", package="Evapotranspiration")[1])
  cs <- constants
  
  # only need to modify elevation
  cs$Elev <- elevation
  
  # this works
  ET <- Evapotranspiration::ET.Makkink(
    climate.data, 
    constants = cs, 
    ts = "daily", 
    solar = "data", 
    save.csv = FALSE
  )
  
  return(ET)
}


# x: long
# y: lat
# start_yr
# end_yr
.getDayMet <- function(x, y, start_yr, end_yr) {
  
  d <- daymetr::download_daymet("daymet",
                                lat = y,
                                lon = x,
                                start = start_yr,
                                end = end_yr,
                                internal = TRUE
  )
  
  # keep only the data
  d <- d$data
  
  # date for plotting and ET estimation
  d$date <- as.Date(sprintf('%s %s', d$year, d$yday), format="%Y %j")
  return(d)
}


.prepareDailyInputs <- function(x, start, end) {
  
  # get elevation
  e <- suppressMessages(elevatr::get_elev_point(locations = x)$elevation)
  
  # coordinates for DAYMET
  coords <- coordinates(x)  
  
  # DAYMET WWW lookup
  dm <- suppressMessages(
    .getDayMet(x = coords[, 1], y = coords[, 2], start_yr = start, end_yr = end)
  )
  
  ## estimate PET from DAYMET
  ET <- suppressMessages(
    .estimatePET(dm, elevation = e)
  )
  
  return(
    list(
      DM = dm,
      ET = ET
    )
  )
  
}




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
#' @param x `SpatialPoints` or `SpatialPointsDataFrame` representing a single point
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
    
  # get daily input data as list
  daily.data <- .prepareDailyInputs(x, start = start, end = end)
  
  # use component keys at `x` if none provided
  if(is.null(cokeys)) {
    cokeys <- .getSSURGO_at_point(x, bufferRadiusMeters = bufferRadiusMeters)
  }
  
  # get SSURGO hydraulic data for select components
  s <- suppressMessages(
    prepare_SSURGO_hydro_data(cokeys = cokeys, max.depth = 100)
  )
  
  
  
  ## TODO: there is no accounting for two components with the same name (e.g. phases)
  # ---> use cokey for internal iteration
  
  
  ## run model
  # iterate over soils and perform water balance
  wb.series <- list()
  for(i in 1:nrow(s$agg)) {
    
    # current data
    d.i <- s$agg[i, ]
    series.i <- d.i[['compname']]
    
    # run water balance
    wb <- simpleWB(
      PPT = daily.data$DM$prcp..mm.day., 
      PET = daily.data$ET$ET.Daily, 
      D = daily.data$DM$date, 
      thickness = d.i$corrected_depth, 
      sat = d.i$sat, 
      fc = d.i$fc,
      ...
    )
    
    # add series label
    wb[['series']] <- series.i
    
    wb.series[[series.i]] <- wb
  }
  
  # flatten to DF
  wb.series <- do.call('rbind', wb.series)
  
  # ## careful!!! this is not well tested
  # # set series levels
  # wb.series$series <- factor(wb.series$series, levels=s$agg$compname[gc$order])
  
  ## moisture state classification
  # join with aggregate data, likely a better way to do this..?
  wb.series <- merge(wb.series, s$agg, by.x='series', by.y='compname', all.x=TRUE, sort=FALSE)
  
  # classify moisture state
  wb.series$state <- with(wb.series, estimateSoilMoistureState(VWC, U, sat, fc, pwp))
  
  # months for grouping
  wb.series$month <- months(wb.series$date, abbreviate = TRUE)
  wb.series$month <- factor(wb.series$month, levels=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
  
  # weeks for grouping
  wb.series$week <- factor(format(wb.series$date, '%U'))
  
  # days for grouping
  wb.series$doy <- factor(format(wb.series$date, '%j'))
  
  ## TODO: need a proper cokey / site ID
  # convert series to factor for now
  wb.series$series <- factor(wb.series$series)
  
  return(wb.series)
}





