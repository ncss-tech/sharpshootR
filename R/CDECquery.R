
CDECquery <- function(id, sensor, interval='D', start, end) {
  # important: change the default behavior of data.frame
  opt.original <- options(stringsAsFactors = FALSE)
  
  # sanity-check:
  if(missing(id) | missing(sensor) | missing(start) | missing(end))
    stop('missing arguments', call.=FALSE)
  
  # check for required packages
  if(!requireNamespace('jsonlite', quietly=TRUE))
    stop('please install the `jsonlite` packages', call.=FALSE)
  
  ## 2018-09-18: new CDEC API, 
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
  
  # download and convert JSON to data.frame
  # missing data are encoded via ommission of a data element
  # fromJSON() will automatically convery to NA
  d <- try(jsonlite::fromJSON(u, simplifyDataFrame = TRUE))
  
  ## TODO: is this wise?
  # re-name columns
  names(d) <- c('station_id', 'dur_code', 'sensor_num', 'sensor_type', 'date', 'obsDate', 'value', 'flag', 'units')
  
  # catch errors
  if(class(d) == 'try-error') {
    stop(sprintf('invalid URL: %s', u), call.=FALSE)
  }
  
  # no data available, play nicely with block of requestes
  if(nrow(d) < 1) {
    message('query returned no data', call.=FALSE)
    return(NULL)
  }
    
  # convert date/time to R-friendly format
  d$datetime <- as.POSIXct(d$date, format="%Y-%m-%d %H:%M")
  
  # remove original date for tidy output
  d$date <- NULL
  d$obsDate <- NULL
  
  # extract the year and month for reporting ease later
  d$year <- as.numeric(format(d$datetime, "%Y"))
  d$month <- factor(format(d$datetime, '%B'), levels=c('January','February','March','April','May','June','July','August','September','October','November','December'))
  
  # 2018-09-18: more data returned by the API, so makes sense to include additional columns
  return(d)
}
