



#' @title Compute moisture state proportions
#'
#' @param x `data.frame` created by `dailyWB_SSURGO()`
#' @param id character, column name identifying sites, soils, or soil series
#' @param step time step, one of 'month', 'week', or 'doy'
#'
#' @return `data.frame`
#' @export
#'
moistureStateProportions <- function(x, id = 'series', step = c('month', 'week', 'doy')) {
  
  # limit argument choices
  step <- match.arg(step)
  
  rs <- split(x, f = x[[id]])
  
  rs.prop <- lapply(rs, function(i) {
    # tabulate at given step
    tab <- table(i[[step]], i$state)
    
    # convert to proportions
    ss <- sweep(tab, MARGIN = 1, STATS = rowSums(tab), FUN = '/')
    ss <- as.data.frame(ss)
    names(ss) <- c('interval', 'state', 'proportion')
    
    # keep track of current ID
    ss[[id]] <- unique(i[[id]])
    
    return(ss)
  })
  
  rs.prop <- do.call('rbind', rs.prop)
  
  ## BUG: ordered moisture state factor down-graded to regular factor
  ##      levels are correct
  rs.prop$state <- ordered(rs.prop$state)
  
  # reset rownames
  row.names(rs.prop) <- NULL
  
  return(rs.prop)
  
}





