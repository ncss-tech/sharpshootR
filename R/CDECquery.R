
CDECquery <- function(id, sensor, interval='D', start, end) {
  # important: change the default behavior of data.frame
  opt.original <- options(stringsAsFactors = FALSE)
  
  # sanity-check:
  if(missing(id) | missing(sensor) | missing(start) | missing(end))
    stop('missing arguments', call.=FALSE)
  
  ## 2018-09-18: new CDEC API, multiple sensors can be specified in one request 
  ## e.g.: &SensorNums=194,197
  ## seems slower than before
  ##
  ## TODO: currently broken, why?
  # construct the URL for the DWR website  
  u <- paste0(
    'http://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=', id, 
    '&SensorNums=', sensor, 
    '&dur_code=', interval, 
    '&Start=', start,
    '&End=', end)
  
  # encode as needed
  u <- URLencode(u)
  
  # init temp file and download
  tf <- tempfile()
  suppressWarnings(download.file(url=u, destfile=tf, quiet=TRUE))
  
  # try to parse CSV
  d <- read.csv(file=tf, header=TRUE, na.strings='', stringsAsFactors=FALSE)
  d <- try(read.csv(file=tf, header=TRUE, na.strings='', stringsAsFactors=FALSE),  silent=TRUE)
  
  # catch errors
  if(class(d) == 'try-error') {
    ref.url <- paste0('invalid URL; see ','http://cdec.water.ca.gov/cgi-progs/queryCSV?station_id=', id)
    stop(ref.url, call.=FALSE)
  }
  
  # no data available
  if(nrow(d) == 0)
    stop('query returned no data', call.=FALSE)
  
  # convert date/time to R-friendly format
  d$datetime <- as.POSIXct(paste(d[[1]], d[[2]]), format="%Y%m%d %H%M")
  
  # strip-out first two columns
  d[[1]] <- NULL # date part
  d[[1]] <- NULL # time part
  
  # note: d[[1]] will now contain the requested data, but likely with a nasty label
  # relable here:
  names(d)[1] <- 'value'
  
  # extract the year and month for reporting ease later
  d$year <- as.numeric(format(d$datetime, "%Y"))
  d$month <- factor(format(d$datetime, '%B'), levels=c('January','February','March','April','May','June','July','August','September','October','November','December'))
  
  # return the result in a more useful order
  return(d[, c('datetime', 'year', 'month', 'value')])
}
