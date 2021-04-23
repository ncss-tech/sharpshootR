



#' Simple Daily Water Balance
#'
#' @param x `data.frame`, required columns include:
#'   * `sat`
#'   * `fc`
#'   * `pwp`
#'   * `thickness`
#'   * `a.ss`
#'   * "id"
#'   
#' @param daily.data `data.frame`, required columns include:
#'    * `date`
#'    * `PPT`
#'    * `PET`
#' 
#' @param id character, name of column in `x` that is used to identify records
#' 
#' @param S_0 fraction of water storage filled at time = 0 (range: 0-1)
#'
#' @return a `data.frame` containing:
#' 
#' @export
#'
#' @examples
dailyWB <- function(x, daily.data, id, S_0 = 0.5) {
  
  # sanity checks
  stopifnot(inherits(x, 'data.frame'))
  stopifnot(inherits(daily.data, 'data.frame'))
  
  # required variables
  req.vars.x <- c(id, 'sat', 'fc', 'pwp', 'thickness', 'a.ss')
  req.vars.dd <- c('date', 'PPT', 'PET')
  
  if(!all(req.vars.x %in% names(x))) {
    stop(sprintf('`x` must contain columns: %s', paste(req.vars.x, collapse = ', ')))
  }
  
  if(!all(req.vars.dd %in% names(daily.data))) {
    stop(sprintf('`daily.data` must contain columns: %s', paste(req.vars.dd, collapse = ', ')))
  }
  
  # records are defined by rows in `x`
  z <- lapply(1:nrow(x), function(i) {
    
    # current record
    x.i <- x[i, ]
    
    wb <- simpleWB(
      PPT = daily.data$PPT, 
      PET = daily.data$PET, 
      D = daily.data$date, 
      thickness = x.i$thickness, 
      sat = x.i$sat, 
      fc = x.i$fc, 
      a.ss = x.i$a.ss,
      S_0 = S_0
    )
    
    # add contextual data
    wb[[id]] <- factor(x.i[[id]])
    wb$sat <- x.i$sat
    wb$fc <- x.i$fc
    wb$pwp <- x.i$pwp
    
    # possibly merge daily.data for context?
    
    return(wb)
  })
  
  z <- do.call('rbind', z)
  
  # moisture state, based on interpretation of water retention curve
  z$state <- with(z, estimateSoilMoistureState(VWC, U, sat, fc, pwp))
  
  # months for grouping
  z$month <- months(z$date, abbreviate = TRUE)
  z$month <- factor(
    z$month, 
    levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  )
  
  # weeks for grouping
  z$week <- factor(format(z$date, '%U'))
  
  # days for grouping
  z$doy <- factor(format(z$date, '%j'))
  
  return(z)
  
}
