

#' @title Title
#' @description 
#' 
#' @param x 
#' @param which 
#'
#' @return
#' @export
#'
reconcileOSDGeomorp <- function(x, which = c('hillpos', 'geomcomp', 'flats', 'mtnpos', 'terrace', 'shape_across', 'shape_down')) {
  
  # sanity checks
  which <- match.arg(which)
  
  if(inherits(x, 'list')) {
    if(!inherits(x$SPC, 'SoilProfileCollection')) {
      stop('`x` should be the result from fetchOSD(..., extended = TRUE)', call. = FALSE)
    }
  } else {
    stop('`x` should be the result from fetchOSD(..., extended = TRUE)', call. = FALSE)
  }
  
  if(is.logical(x[[which]])) {
    message('not enough data')
    return(NULL)
  }
  
  # minimum subset of profile IDs
  nm <- intersect(profile_id(x$SPC), x[[which]]$series)
  
  # keep only those series that exist in both
  sub <- subset(x$SPC, profile_id(x$SPC) %in% nm)
  
  # inverse problem: extra records in geomorph proportion
  geom.sub <- subset(x[[which]], subset = series %in% profile_id(sub))
  
  return(list(
    SPC = sub,
    geom = geom.sub
  ))
}
