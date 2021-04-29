
#' @title Get snow survey data (California only) from the CDEC website.
#' 
#' @description Get snow survey data (California only) from the CDEC website.
#' 
#' 
#' @param course integer, course number (e.g. 129)
#' @param start_yr integer, the starting year (e.g. 2010)
#' @param end_yr integer, the ending year (e.g. 2013)
#' 
#' @details This function downloads data from the CDEC website, therefore an internet connection is required. The `SWE` column contains adjusted SWE if available (`Adjusted` column), otherwise the reported SWE is used (`Water` column). See the [tutorial](http://ncss-tech.github.io/AQP/sharpshootR/CA-snow-survey.html) for examples.
#' 
#' @note Snow course locations, ID numbers, and other information can be found here: \url{http://cdec.water.ca.gov/misc/SnowCourses.html}
#' 
#' @return a `data.frame` object, see examples
#' 
#' @references \url{http://cdec.water.ca.gov/cgi-progs/snowQuery}
#' 
#' @author D.E. Beaudette
#' 
#' @export
#'
CDECsnowQuery <- function(course, start_yr, end_yr) {
  # construct the URL for the DWR website	
  u <- paste(
    'http://cdec.water.ca.gov/cgi-progs/snowQuery?course_num=', course, 
    '&month=(All)&start_date=', start_yr, 
    '&end_date=', end_yr, 
    '&csv_mode=Y&data_wish=Retrieve+Data', 
    sep='')
  
  # read the result of sending the URL as a CSV file:
  # noting that it has a header,
  # skipping the first line
  # not converting characters to factors
  # interpreting '        --' as NA
  d <- read.csv(file=url(u), header=TRUE, skip=1, as.is=TRUE, na.strings='        --')
  
  if(nrow(d) == 0 | ncol(d) != 5)
    stop('query returned no data', call.=FALSE)
  
  # compute the density, as percent
  d$density <- (d$Water / d$Depth) * 100.0
  
  # add SWE collumn using Adjusted if present
  d$SWE <- ifelse(is.na(d$Adjusted), d$Water, d$Adjusted)
  
  # convert date to R-friendly format
  d$Meas.Date <- as.Date(d$Meas.Date, format="%d-%B-%Y")
  
  # convert representative date 
  # note that this month isn't the same as the month when the data were collected
  # note that we need to add an arbitrary 'day' to the string in order for it to be parsed correctly
  d$Date <- as.Date(paste('01/', d$Date, sep=''), format="%d/%m/%Y")
  
  # extract the year and month for reporting ease later
  d$year <- as.numeric(format(d$Date, "%Y"))
  d$month <- factor(format(d$Date, '%B'), levels=c('January','February','March','April','May','June','July','August','September','October','November','December'))
  
  # return the result
  return(d[, c('Meas.Date','Date','year','month','Depth','Water','Adjusted','SWE','density')])
}
