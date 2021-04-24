## TODO: sometimes CDEC returns no data... why?


#' @title Easy Access to the CDEC API
#' 
#' @description A (relatively) simple interface to the CDEC website.
#' 
#' @param id station ID (e.g. 'spw'), single value or vector of station IDs, see details
#' 
#' @param sensor the sensor ID, single value or vector of sensor numbers, see details
#' 
#' @param interval character, 'D' for daily, 'H' for hourly, 'M' for monthly, 'E' for event: see Details.
#' 
#' @param start starting date, in the format 'YYYY-MM-DD'
#' 
#' @param end ending date, in the format 'YYYY-MM-DD'
#' 
#' @return A \code{data.frame} object with the following fields: `datetime`, `year`, `month`, `value`.
#' 
#' @references \url{http://cdec.water.ca.gov/queryCSV.html}
#' 
#' @author D.E. Beaudette
#' 
#' @details 
#' 
#' Sensors that report data on an \code{interval} other than monthly ('M'), daily ('D'), or hourly ('H') can be queried with an `event` interval ('E'). Soil moisture and temperature sensors are an example of this type of reporting. See examples below.
#' 
#' \describe{
#' \item{1.}{Station IDs can be found here: \url{http://cdec.water.ca.gov/staInfo.html}}
#' \item{2a.}{Sensor IDs can be found using this URL: \url{http://cdec.water.ca.gov/dynamicapp/staMeta?station_id=}, followed by the station ID.}
#' \item{2b.}{Sensor details can be accessed using \code{\link{CDEC_StationInfo} with the station ID.}}
#' \item{3.}{Resevoir capacities can be found here: \url{http://cdec.water.ca.gov/misc/resinfo.html}}
#' \item{4.}{A new interactive map of CDEC stations can be found here: \url{http://cdec.water.ca.gov}}
#' }
#' 
#' @seealso \code{\link{CDECsnowQuery}} \code{\link{CDEC_StationInfo}}
#' 
#' @examples 
#' 
#' \donttest{
#'
#' if(requireNamespace("curl") &
#' curl::has_internet() &
#'   require(latticeExtra) &
#'   require(plyr) &
#'   require(e1071)) {
#'     
#'     library(RColorBrewer)
#'     
#'     # get daily reservoir storage (ac. ft) from 
#'     # Pinecrest, New Melones and Lyons reservoirs
#'     pinecrest <- CDECquery(id='swb', sensor=15, interval='D', 
#'     start='2012-09-01', end='2015-01-01')
#'     
#'     new.melones <- CDECquery(id='nml', sensor=15, interval='D', 
#'     start='2012-09-01', end='2015-01-01')
#'     
#'     lyons <- CDECquery(id='lys', sensor=15, interval='D', 
#'     start='2012-09-01', end='2015-01-01')
#'     
#'     # compute storage capacity
#'     pinecrest$capacity <- pinecrest$value / 18312 * 100
#'     new.melones$capacity <- new.melones$value / 2400000 * 100
#'     lyons$capacity <- lyons$value / 6228 * 100
#'     
#'     # combine
#'     g <- make.groups(new.melones, lyons, pinecrest)
#'     
#'     # resonable date scale
#'     r <- range(g$datetime)
#'     s.r <- seq(from=r[1], to=r[2], by='1 month')
#'     
#'     # better colors
#'     tps <- list(superpose.line=list(lwd=2, col=brewer.pal(n=3, name='Set1')))
#'     
#'     # plot
#'     xyplot(capacity ~ datetime, groups=which, data=g, type='l', 
#'            xlab='', ylab='Capacity (%)', ylim=c(-5, 105),
#'            scales=list(x=list(at=s.r, labels=format(s.r, "%b\n%Y"))), 
#'            auto.key=list(columns=3, lines=TRUE, points=FALSE),
#'            par.settings=tps,
#'            panel=function(...) {
#'              panel.abline(h=seq(0, 100, by=10), col='grey')
#'              panel.abline(v=s.r, col='grey')
#'              panel.xyplot(...)
#'            })
#'     
#'     
#'     ##
#'     # New Melones monthly data, retrieve as far back in time as possible 
#'     new.melones.monthly <- CDECquery(id='nml', sensor=15, interval='M', 
#'                                      start='1900-01-01', end='2015-01-01')
#'     
#'     # convert to pct. capacity
#'     new.melones.monthly$capacity <- new.melones.monthly$value / 2400000 * 100
#'     
#'     
#'     # make a nice color ramp function
#'     cols <- colorRampPalette(brewer.pal(9, 'Spectral'), 
#'                              space='Lab', interpolate='spline')
#'     
#'     # plot, each pixel is colored by the total precip by year/month
#'     levelplot(capacity ~ year * month, data=new.melones.monthly, col.regions=cols, xlab='', 
#'               ylab='', scales=list(x=list(tick.number=20)), main='New Melones Capacity (%)')
#'     
#'     
#'     ##
#'     # get daily precip totals from Stan Powerhouse
#'     x <- CDECquery(id='spw', sensor=45, interval='D', start='1900-01-01', end='2019-01-01')
#'     
#'     # compute total precip by year/month
#'     a <- ddply(x, c('year', 'month'), summarize, s=sum(value, na.rm=TRUE))
#'     
#'     # convert monthly precipitation values into Z-scores by month
#'     a.scaled <- ddply(a, 'month', summarize, year=year, scaled.ppt=scale(s))
#'     
#'     # make a nice color ramp function, scaled by the skewness of the underlying distribution
#'     cols <- colorRampPalette(
#'               brewer.pal(9, 'Spectral'),
#'               space='Lab', interpolate='spline', bias=skewness(a.scaled$scaled.ppt, na.rm=TRUE))
#'     
#'     # plot, each pixel is colored by the total precip by year/month
#'     levelplot(scaled.ppt ~ year * month, data=a.scaled, col.regions=cols, xlab='', 
#'               ylab='', scales=list(x=list(tick.number=10)), 
#'               main='Monthly Total Precipitation (as z-score) SPW')
#'     
#'     
#'     ##
#'     # get pre-aggregated monthly data from Sonora RS
#'     x <- CDECquery(id='sor', sensor=2, interval='M', start='1900-01-01', end='2019-01-01')
#'     
#'     # make a nice color ramp function, scaled by the skewness of the underlying distribution
#'     cols <- colorRampPalette(brewer.pal(9, 'Spectral'), space='Lab', 
#'                              interpolate='spline', bias=skewness(x$value, na.rm=TRUE))
#'     
#'     # plot
#'     levelplot(value ~ year * month, data=x, col.regions=cols, xlab='', 
#'               ylab='', scales=list(x=list(tick.number=20)), 
#'               main='Monthly Total Precipitation (inches) SOR')
#'     
#'     
#'     ### query an 'event' type sensor
#'     # Bryte test site (BYT)
#'     # single request: air temperature and soil temperature at depth 1 (25cm)
#'     # measurement interval is 20 minutes
#'     x <- CDECquery('BYT', c(4, 194), 'E', '2016-01-01', '2017-01-01')
#'     
#'     # data are in long format, check number of records for each sensor
#'     table(x$sensor_type)
#'     
#'     # plot grouped data
#'     xyplot(value ~ datetime, groups=sensor_type, data=x, type=c('g', 'l'),
#'            auto.key=list(columns=2, points=FALSE, lines=TRUE))
#'     
#'   }
#' 
#' 
#' }
#' 
#' 

CDECquery <- function(id, sensor, interval = 'D', start, end) {
  # important: change the default behavior of data.frame
  opt.original <- options(stringsAsFactors = FALSE)
  
  # sanity-check:
  if(missing(id) | missing(sensor) | missing(start) | missing(end))
    stop('missing arguments', call.=FALSE)
  
  # check for required packages
  if(!requireNamespace('jsonlite', quietly=TRUE))
    stop('please install the `jsonlite` packages', call.=FALSE)
  
  ## 2018-09-18: new CDEC API, 
  ## more data returned, so makes sense to include additional columns
  ## multiple stations and sensors can be specified in one request
  ## e.g.: &SensorNums=194,197
  ## JSON API returns simpler data structure
  # construct the URL for the DWR website  
  u <- paste0(
    'http://cdec.water.ca.gov/dynamicapp/req/JSONDataServlet?',
    '&Stations=', paste(id, collapse = ','), 
    '&SensorNums=', paste(sensor, collapse = ','), 
    '&dur_code=', interval, 
    '&Start=', start,
    '&End=', end)
  
  # encode as needed
  u <- URLencode(u)
  
  ## important!!! the column ordering is not guarunteed to be consistent, API issue?
  # download and convert JSON to data.frame
  # missing data are encoded via ommission of a data element
  # fromJSON() will automatically convert to NA
  d <- try(jsonlite::fromJSON(u, simplifyDataFrame = TRUE))
  
  # catch errors
  if(class(d) == 'try-error') {
    stop(sprintf('invalid URL: %s', u), call.=FALSE)
  }
  
  # no data available, play nicely with block of requestes
  if(is.list(d) & length(d) == 0) {
    message('query returned no data')
    return(NULL)
  }
  
  ## TODO: ask DWR why this happens or think of a better solution
  # re-name columns if possible
  # they aren't always in the same order, why?
  # mapping between new names and likely default names
  new.names <- c('station_id', 'dur_code', 'sensor_num', 'sensor_type', 'date', 'obsDate', 'value', 'flag', 'units')
  original.names <- c("stationId", "durCode", "SENSOR_NUM", "sensorType", "date", "obsDate", "value", "dataFlag", "units")
  
  # whatever comes back from CDEC
  nm <- names(d)
  
  # iterate over current names and re-map
  for(i in seq_along(original.names)) {
    idx <- grep(original.names[i], nm, ignore.case = TRUE)
    names(d)[idx] <- new.names[i]
  }
  
  # re-order so that all calls to CDECquery() return the same structure
  d <- d[, new.names]
    
  # convert date/time to R-friendly format
  d$datetime <- as.POSIXct(d$date, format="%Y-%m-%d %H:%M")
  
  # remove original date for tidy output
  d$date <- NULL
  d$obsDate <- NULL
  
  # extract the year and month for reporting ease later
  d$year <- as.numeric(format(d$datetime, "%Y"))
  d$month <- factor(format(d$datetime, '%B'), levels=c('January','February','March','April','May','June','July','August','September','October','November','December'))
  
  # NODATA are sometimes encoded as -9999
  idx <- which(d$value == -9999)
  if(length(idx) > 0) {
    d <- d[-idx, ]
  }
  
  ## TODO: more testing
  # 2019-02-11: add water year and date, using CA "water year" ending Spet 30 (soilDB::waterDayYear)
  # eval water year and water day, length should be the same
  w <- waterDayYear(d$datetime)
  
  # row-order is preserved
  d$water_year <- w$wy
  d$water_day <- w$wd
  
  return(d)
}
