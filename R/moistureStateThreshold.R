

#' @title Apply a threshold to soil moisture states
#'
#' @param x a `data.frame` created by [`dailyWB()`] or [`dailyWB_SSURGO()`]
#' @param id character, column name identifying sites, soils, or soil series
#' @param threshold moisture state threshold, see [`estimateSoilMoistureState`]
#' @param operator one of "<", ">", "==", "<=", or ">="
#'
#' @author D.E. Beaudette
#'
#' @return `data.frame`
#' @export
#'
moistureStateThreshold <- function(x, id = 'compname', threshold = 'moist', operator = c("<", ">", "==", "<=", ">=")) {
  
  # limit to these choices
  operator <- match.arg(operator)
  
  # convert operator from text -> function
  operator <- get(operator)
  
  # iteration over ID, typically soil series or component name
  rs <- split(x, f = x[[id]])
  rs.doy <- lapply(rs, function(i) {
    
    # iterate over DOY (records = years)
    i.doy <- split(i, i$doy)
    state.lt.conditon.prob <- sapply(i.doy, function(j) {
      
      # evaluate expression
      expr <- operator(j[['state']], threshold)
      
      # there may be case where there are no TRUE cases
      # converting to a factor ensures table dimensions are consistent
      tab <- table(
        factor(expr, levels=c('FALSE', 'TRUE'))
      )
      
      # convert to proportions
      tab <- prop.table(tab)
      
      # return TRUE proportions
      return(tab[['TRUE']])
    })
    
    # conveniently re-package
    res <- data.frame(
      series = i[[id]][1], 
      doy = names(state.lt.conditon.prob), 
      Pr = state.lt.conditon.prob, 
      stringsAsFactors = FALSE
    )
    
    # use original ID name
    names(res)[1] <- id
    
    return(res)
  })
  
  rs.doy <- do.call('rbind', rs.doy)
  
  # reset rownames
  row.names(rs.doy) <- NULL
  
  return(rs.doy)
}
