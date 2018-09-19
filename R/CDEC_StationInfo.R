
# get sensor details for a CDEC station
# s: CDEC station ID
CDEC_StationInfo <- function(s) {
  # check for required packages
  if(!requireNamespace('rvest', quietly = TRUE) | !requireNamespace('xml2', quietly = TRUE))
    stop('please install the `rvest` package', call.=FALSE)
  
  # scrape HTML tables from the results
  u <- sprintf("http://cdec.water.ca.gov/dynamicapp/staMeta?station_id=%s", s)
  h <- xml2::read_html(u)
  hn <- rvest::html_nodes(h, "table")
  
  # make sure there are some results to process, there should be 2 or 3 tables
  if(length(as.list(hn)) > 0) {
    
    ## site metadata, poorly formatted in split, 2-column layout
    site.meta <- rvest::html_table(hn[[1]])
    
    # column 1
    c1 <- as.data.frame(t(site.meta[, 2]))
    names(c1) <- site.meta[, 1]
    # column 2
    c2 <- as.data.frame(t(site.meta[, 4]))
    names(c2) <- site.meta[, 3]
    
    # combine
    site.meta <- cbind(c1,c2)
    
    # strip non-numeric chars from coordinates | elevation and convert to numeric
    site.meta$Longitude <- as.numeric(gsub("[^0-9\\.]", "", site.meta$Longitude))
    site.meta$Latitude <- as.numeric(gsub("[^0-9\\.]", "", site.meta$Latitude))
    site.meta$Elevation <- as.numeric(gsub("[^0-9\\.]", "", site.meta$Elevation))
    
    ## sensors, can be converted into data.frame
    sensor.meta <- rvest::html_table(hn[[2]])
    names(sensor.meta) <- c('sensor_details', 'sensor', 'interval', 'sensor_name', 'collection_method', 'period_of_record')
    
    # comments, may be missing
    if(length(hn) > 2) {
      site.comments <- rvest::html_table(hn[[3]])
      names(site.comments) <- c('date', 'comment')
    } else {
      site.comments <- NULL
    }
    
    # return a list with nicely-formatted site, sensor, comment 
    return(list('site.meta'=site.meta, 'sensor.meta'=sensor.meta, 'comments'=site.comments))
    
  } else {
    
    message('no station found')
    return(NULL)
  }
  
}
