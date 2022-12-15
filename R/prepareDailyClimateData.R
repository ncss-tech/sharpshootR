
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


## TODO: column names are kind of funky
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


#' @title Prepare daily climate data (DAYMET) for a single point
#' 
#' @description This function returns daily climate data required for a simple water balance (and more), using three packages:
#'   * `elevatr`: elevation data at `x`
#'   * `daymetr`: DAYMET data at `x` for years `start` through `end`
#'   * `Evapotranspiration`: Makkink formulation for estimating reference crop evapotranspiration
#'
#' @param x `sf` object representing a single point
#' @param start start year (1998)
#' @param end end year (2018)
#' @param onlyWB logical, return just those date required by `dailyWB`
#'
#' @return a `data.frame`
#' @export
#'
prepareDailyClimateData <- function(x, start, end, onlyWB = TRUE) {
  
  # check for required packages
  if(
    !requireNamespace('elevatr', quietly = TRUE) | 
    !requireNamespace('daymetr', quietly = TRUE) |
    !requireNamespace('sf', quietly=TRUE) |
    !requireNamespace('Evapotranspiration', quietly = TRUE)
  ) {
    stop('please install the `Evapotranspiration`, `elevatr`, and `daymetr` packages', call. = FALSE)
  }
   
  # sanity checks
  stopifnot(inherits(x, 'sf'))
  
  # TODO: eventally vectorize over points
  # for now, only a single point allowed
  stopifnot(length(x) == 1)
   
  # get elevation
  e <- suppressMessages(elevatr::get_elev_point(locations = x)$elevation)
  
  ## TODO: abstract to anything that can give {long,lat}
  ## TODO: iterate over coordinates
  # coordinates for DAYMET
  coords <- sf::st_coordinates(x)  
  
  # DAYMET API
  DM <- suppressMessages(
    .getDayMet(x = coords[, 1], y = coords[, 2], start_yr = start, end_yr = end)
  )
  
  # estimate PET from DAYMET
  ET <- suppressMessages(
    .estimatePET(DM, elevation = e)
  )
  
  # re-package into something really simple
  if(onlyWB) {
    
    res <- data.frame(
      date = DM$date,
      PPT = DM$prcp..mm.day.,
      PET = ET$ET.Daily
    )
    
  } else {
    # everything
    res <- list(
      DM = DM,
      ET = ET
    )
    
  }
  
  return(res)
  
}

