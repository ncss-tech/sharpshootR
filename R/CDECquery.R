
CDECquery <- function(id, sensor, interval='D', start, end) {
  # important: change the default behavior of data.frame
  opt.original <- options(stringsAsFactors = FALSE)
  
  # sanity-check:
  if(missing(id) | missing(sensor) | missing(start) | missing(end))
    stop('missing arguments', call.=FALSE)
  
  ## 2018-09-18: new CDEC API, 
  ## multiple stations and sensors can be specified in one request
  ## e.g.: &SensorNums=194,197
  # construct the URL for the DWR website  
  u <- paste0(
    'http://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?',
    '&Stations=', paste(id, collapse = ','), 
    '&SensorNums=', paste(sensor, collapse = ','), 
    '&dur_code=', interval, 
    '&Start=', start,
    '&End=', end)
  
  # encode as needed
  u <- URLencode(u)
  
  # init temp file and download
  tf <- tempfile()
  suppressWarnings(download.file(url=u, destfile=tf, quiet=TRUE))
  
  # try to parse CSV
  # 2018-09-18: CDEC API returns bogus CSV file
  # https://github.com/ncss-tech/sharpshootR/issues/13
  # read without header and skip first row
  # NA encoded as '---'
  # manually specify column classes so that time 0000 (HHMM) is corectly encoded
  CDEC.columns <- c('station_id', 'dur_code', 'sensor_num', 'sensor_type', 'date', 'time', 'value', 'flag', 'units')
  CDEC.column.class <- c('character', 'character', 'integer', 'character', 'character', 'character', 'numeric', 'character', 'character')
  d <- try(read.csv(file=tf, header=FALSE, skip=1, na.strings='---', colClasses = CDEC.column.class))
  
  # add column names
  names(d) <- CDEC.columns
  
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
  d$datetime <- as.POSIXct(paste(d$date, d$time), format="%Y%m%d %H%M")
  
  # extract the year and month for reporting ease later
  d$year <- as.numeric(format(d$datetime, "%Y"))
  d$month <- factor(format(d$datetime, '%B'), levels=c('January','February','March','April','May','June','July','August','September','October','November','December'))
  
  # 2018-09-18: more data returned by the API, so makes sense to include additional columns
  return(d)
}
