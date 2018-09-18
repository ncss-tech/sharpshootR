
# get sensor details for a CDEC station
# s: CDEC station ID
CDEC_StationInfo <- function(s) {
  # check for required packages
  if(!requireNamespace('rvest', quietly = TRUE) | !requireNamespace('xml2', quietly = TRUE))
    stop('please install the `rvest` package', call.=FALSE)
  
  u <- sprintf("http://cdec.water.ca.gov/dynamicapp/staMeta?station_id=%s", s)
  h <- xml2::read_html(u)
  hn <- rvest::html_nodes(h, "table")
  
  # make sure there are some results to process, there should be 3 tables
  if(length(as.list(hn)) > 0) {
    # site, poorly formatted
    site.meta <- rvest::html_table(hn[[1]])
    
    # sensors, can be converted into data.frame
    sensor.meta <- rvest::html_table(hn[[2]])
    names(sensor.meta) <- c('sensor_details', 'sensor', 'interval', 'sensor_name', 'collection_method', 'period_of_record')
    
    # notes, may be missing
    # site.notes <- rvest::html_table(hn[[3]])
    
    ## TODO: return a list with nicely-formatted site, sensor, notes
    return(sensor.meta)
    
  } else {
    
    message('no station found')
    return(NULL)
  }
  
}
