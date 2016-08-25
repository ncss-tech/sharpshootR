
# get sensor details for a CDEC station
# s: CDEC station ID
CDEC_StationInfo <- function(s) {
  # check for required packages
  if(!requireNamespace('rvest', quietly = TRUE))
    stop('please install the `rgeos` package', call.=FALSE)
  
  u <- paste0("http://cdec.water.ca.gov/cgi-progs/queryCSV?station_id=", s)
  h <- read_html(u)
  hn <- html_nodes(h, "table")
  if(length(as.list(hn)) > 0) {
    ht <- html_table(hn[[1]])
    names(ht) <- c('sensor', 'sensor_details', 'interval', 'period_of_record')
    return(ht)
  } else {
    message('no station found')
    return(NULL)
  }
  
}
