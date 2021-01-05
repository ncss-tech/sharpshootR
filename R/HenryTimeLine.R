
## TODO: split chunks of non-NA, simple to do but not easily rendered with segplot...

#' @title Sensor Data Timeline from Henry Mount Soil and Water DB
#' 
#' @description This function generates a simple chart of start/end dates for a set of sensor data returned by \code{soilDB::fetchHenry}.
#' 
#' @param sensor_data `soiltemp`, `soilVWC`, or related data returned by \code{soilDB::fetchHenry()}
#' @param ... additional arguments to \code{latticeExtra::segplot}
#'
#' @note This function does not symbolize sections of missing data between the first and last record.
#'
#' @return a \code{lattice} graphics object
#' 
#' @author D.E. Beaudette
#' @export
HenryTimeLine <- function(sensor_data, ...) {
  
  # must have latticeExtra installed
  if(!requireNamespace('latticeExtra'))
    stop('please install the `latticeExtra` package', call.=FALSE)
  
  # hack for R CMD check
  date_time <- NULL
  
  # filter NA
  x.no.na <- na.omit(sensor_data)
  
  # compute date ranges by sensor
  s <- split(x.no.na, x.no.na$sensor_name)
  
  # compute start / end dates
  x.range <- lapply(s, function(i) {
    data.frame(
      sensor_name = i$sensor_name[1],
      start = as.Date(min(i$date_time)),
      end = as.Date(max(i$date_time)),
      stringsAsFactors = FALSE
    )
  })
  
  # flatten to DF and convert sensor name to factor for convenient plotting
  x.range <- do.call('rbind', x.range)
  x.range$sensor_name <- factor(x.range$sensor_name)
  
  # composite plot
  p <- latticeExtra::segplot(
    sensor_name ~ start + end, data = x.range,
    groups = 1,
    scales = list(alternating = 3, x = list(cex = 0.85, tick.number = 10), y = list(relation = 'free', cex = 0.65, rot = 0)),
    band.height = 0.75,
    xlab = '', ylab = '',
    panel = function(...) {
      panel.abline(h = 1:length(levels(x.range$sensor_name)), col = 'grey', lty = 3)
      latticeExtra::panel.segplot(...)
    }, ...
  )
  
  return(p)
}

