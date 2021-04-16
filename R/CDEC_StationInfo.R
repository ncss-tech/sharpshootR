

#' @title CDEC Sensor Details (by Station)
#' 
#' @description Query CDEC Website for Sensor Details
#'
#' @param s character, a single CDEC station ID (e.g. 'HHM')
#' 
#' @details This function requires the `rvest` package.
#'
#' @return A `list` object containing site metadata, sensor metadata, and possibly comments about the site.
#' 
#' @author D.E. Beaudette
#' 
#' @seealso `[CDECquery]`
#' 
#' @export
#'
#' @examples
#' 
#' \donttest{
#' if(requireNamespace("curl") &
#'    curl::has_internet() 
#' ) {
#' 
#'   # CDEC API needs a long timeout
#'   options(timeout = 60000)
#'   
#'   CDEC_StationInfo('HHM')  
#'   
#' }
#' 
#' 
#' }
#' 
CDEC_StationInfo <- function(s) {
  # check for required packages
  if(!requireNamespace('rvest', quietly = TRUE) | !requireNamespace('xml2', quietly = TRUE))
    stop('please install the `rvest` package', call.=FALSE)
  
  # scrape HTML tables from the results
  u <- sprintf("http://cdec.water.ca.gov/dynamicapp/staMeta?station_id=%s", s)
  h <- xml2::read_html(u)
  
  # get H2 elements: station name stored here
  hn.h2 <- rvest::html_nodes(h, "h2")
  
  # get tables: data are stored here
  hn <- rvest::html_nodes(h, "table")
  
  # make sure there are some results to process, there should be 2 or 3 tables
  if(length(as.list(hn)) > 0) {
    
    # attempt to extract station name
    if(length(as.list(hn.h2)) > 0) {
      stn.name <- rvest::html_text(hn.h2[[1]])
    } else {
      stn.name <- "UNKNOWN"
    }
    
    ## site metadata, poorly formatted in split, 2-column layout
    site.meta <- rvest::html_table(hn[[1]])
    
    # column 1
    c1 <- as.data.frame(t(site.meta[, 2]))
    names(c1) <- as.character(unlist(site.meta[, 1]))
    # column 2
    c2 <- as.data.frame(t(site.meta[, 4]))
    names(c2) <- as.character(unlist(site.meta[, 3]))
    
    # combine
    site.meta <- cbind(c1, c2)
    
    # add site name
    site.meta$Name <- stn.name
    
    # strip non-numeric chars from coordinates | elevation and convert to numeric
    site.meta$Longitude <- as.numeric(gsub("[^0-9\\.]", "", site.meta$Longitude))
    site.meta$Latitude <- as.numeric(gsub("[^0-9\\.]", "", site.meta$Latitude))
    site.meta$Elevation <- as.numeric(gsub("[^0-9\\.]", "", site.meta$Elevation))
    
    # reset rownames
    row.names(site.meta) <- NULL
    
    ## sensors, can be converted into data.frame
    sensor.meta <- as.data.frame(rvest::html_table(hn[[2]]))
    names(sensor.meta) <- c('sensor_details', 'sensor', 'interval', 'sensor_name', 'collection_method', 'period_of_record')
    
    # comments, may be missing
    if(length(hn) > 2) {
      # this will fail if the table is empty
      site.comments <- try(
        suppressWarnings(
          as.data.frame(
            rvest::html_table(hn[[3]])
          )
        ), silent = TRUE
      )
      
      # no comments
      if(inherits(site.comments, 'try-error')) {
        site.comments <- NULL
        # otherwise, there are comments
      } else {
        names(site.comments) <- c('date', 'comment')
      }
      
    } else {
      # comment table missing
      site.comments <- NULL
    }
    
    # return a list with nicely-formatted site, sensor, comment 
    return(
      list(
        'site.meta' = site.meta, 
        'sensor.meta' = sensor.meta, 
        'comments' = site.comments
        )
    )
    
  } else {
    
    message('no station found')
    return(NULL)
  }
  
}
