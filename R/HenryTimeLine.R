
## TODO: currently relies on daily granularity and pad.missing.days = TRUE

#' @title Sensor Data Timeline from Henry Mount Soil and Water DB
#' 
#' @description This function generates a simple chart of start/end dates for non-NA sensor data returned by `soilDB::fetchHenry()`. Data are organized according to sensor name + sensor depth.
#' 
#' @param sensor_data `soiltemp`, `soilVWC`, or related data returned by `soilDB::fetchHenry()`
#' @param ... additional arguments to `latticeExtra::segplot`
#'
#'
#' @return a `lattice` graphics object
#' 
#' @author D.E. Beaudette
#' @export
HenryTimeLine <- function(sensor_data, ...) {
  
  # must have latticeExtra installed
  if(!requireNamespace('latticeExtra'))
    stop('please install the `latticeExtra` package', call.=FALSE)
  
  # hack for R CMD check
  date_time <- NULL
  
  # safely fail when there are no data
  if(!inherits(sensor_data, 'data.frame')) {
    stop('insufficient data', call. = FALSE)
  }
  
  
  # add convenience name + depth
  sensor_data[['.name']] <- sprintf("%s %scm", sensor_data$sensor_name, sensor_data$sensor_depth)
  
  # split by sensor ID
  s <- split(sensor_data, sensor_data$sid)
  
  ## NOTE: this requires pad.missing.days = TRUE, only daily data until fetchHenry() is updated
  
  # chunk and compute start / end dates
  x.range <- lapply(s, function(i) {
    
    # find NA: blocks of NA marked with TRUE
    .na <- is.na(i$sensor_value)
    na.rle <- rle(.na)
    
    # init chunk label
    i[['.chunk']] <- NA
    
    # counters for chunk ID
    .chunkID <- 1
    
    # place holder for previous sequence end
    .end <- 0
  
    for(j in seq_along(na.rle$values)) {
      # current RLE sequence
      .length <- na.rle$lengths[j]
      .value <- na.rle$values[j]
      
      # row start
      .start <- ifelse(j == 1, 1, .end + 1)
      
      # row end
      .end <- .end + .length
      
      # compute current row index
      .rows <- seq(from = .start, to = .end, by = 1)
      
      if(!.value) {
        .value <- sprintf("%04d", .chunkID)
        # increment chunk ID counter
        .chunkID <- .chunkID + 1
      } else {
        .value <- 'missing'
      }
      
      # assign chunk label
      i[['.chunk']][.rows] <- .value
      
    }
    
    
    # remove missing
    i <- i[i$.chunk != 'missing', ]
    
    # process chunks
    # summarize start/stop dates of non-NA values
    .chunks <- split(i, i[['.chunk']])
    
    res <- lapply(.chunks, function(k) {
      data.frame(
        .name = k$.name[1],
        .chunk = k$.chunk[1],
        sid = k$sid[1],
        sensor_name = k$sensor_name[1],
        start = as.Date(min(k$date_time, na.rm = TRUE)),
        end = as.Date(max(k$date_time, na.rm = TRUE))
      )
    })
    
    # flatten
    res <- do.call('rbind', res)
    row.names(res) <- NULL
    
    return(res)
  })
  
  # flatten to DF and convert sensor name to factor for convenient plotting
  x.range <- do.call('rbind', x.range)
  row.names(x.range) <- NULL
  
  # init factors for plotting
  x.range$.name <- factor(x.range$.name)
  x.range$.chunk <- factor(x.range$.chunk)
  
  # composite plot
  p <- latticeExtra::segplot(
    .name ~ start + end , data = x.range,
    groups = .chunk,
    scales = list(alternating = 3, x = list(cex = 0.85, tick.number = 10), y = list(relation = 'free', cex = 0.65, rot = 0)),
    band.height = 0.75,
    xlab = '', ylab = '',
    panel = function(...) {
      panel.abline(h = 1:length(levels(x.range$.name)), col = 'grey', lty = 3)
      latticeExtra::panel.segplot(...)
    }, 
    ...
  )
  
  return(p)
}

