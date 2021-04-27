

#' @title Statistics on Soil Moisture State
#'
#' @param x `data.frame`, created by [`moistureStateProportions()`]
#' @param id name of ID column
#'
#' @return `data.frame` containing the most-likely moisture state and Shannon entropy.
#' @export
#' 
moistureStateStats <- function(x, id = 'compname') {
  
  # iterate over ID / interval
  xx <- split(x, f = list(x[[id]], x[['interval']]))
  
  xx <- lapply(xx, function(i) {
    
    ## TODO: no accounting for ties
    # most-likely state
    ml.state <- i$state[which.max(i$proportion)]
    # Shannon entropy via aqp
    H <- shannonEntropy(i$proportion)
    
    # package-up
    res <- data.frame(
      id = i[[id]][1],
      interval = i[['interval']][1],
      state = ml.state,
      H = H
    )
    
    return(res)
  })
  
  xx <- do.call('rbind', xx)
  names(xx)[1] <- id
  row.names(xx) <- NULL
  
  return(xx)
}


#' @title Compute moisture state proportions
#'
#' @param x `data.frame` created by [`dailyWB()`] or [`dailyWB_SSURGO()`]
#' @param id character, column name identifying sites, components, or soil series
#' @param step time step, one of 'month', 'week', or 'doy'
#'
#' @return `data.frame`
#' @export
#'
moistureStateProportions <- function(x, id = 'compname', step = c('month', 'week', 'doy')) {
  
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





