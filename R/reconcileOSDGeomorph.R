

#' @title Reconcile IDs between a SPC and associated geomorphic proportion table
#' @description This function can assist with linked visualizations that include soil morphology data stored in a `SoilProfileCollection` and geomorphic proportions stored in a `data.frame`, as returned by `soilDB::fetchOSD()`.
#' 
#' @param x resulting list from `soilDB::fetchOSD(..., extended = TRUE)`
#' @param selection character, name of geomorphic proportion table
#'
#' @return a `list` with subset `SoilProfileCollection` and `data.frame` of geomorphic proportions, `selection` is preserved as an attribute.
#' @author D.E. Beaudette
#' 
#' @export
#'
reconcileOSDGeomorph <- function(x, selection = c('hillpos', 'geomcomp', 'flats', 'mtnpos', 'terrace', 'shape_across', 'shape_down')) {
  
  # satisfy R CMD check
  series <- NULL
  
  # sanity checks
  selection <- match.arg(selection)
  
  if(inherits(x, 'list')) {
    if(!inherits(x$SPC, 'SoilProfileCollection')) {
      stop('`x` should be the result from soilDB::fetchOSD(..., extended = TRUE)', call. = FALSE)
    }
  } else {
    stop('`x` should be the result from soilDB::fetchOSD(..., extended = TRUE)', call. = FALSE)
  }
  
  if(is.logical(x[[selection]])) {
    message('not enough data')
    return(NULL)
  }
  
  # minimum subset of profile IDs
  nm <- intersect(profile_id(x$SPC), x[[selection]]$series)
  
  # keep only those series that exist in both
  sub <- subset(x$SPC, profile_id(x$SPC) %in% nm)
  
  # inverse problem: extra records in geomorph proportion
  geom.sub <- subset(x[[selection]], subset = series %in% profile_id(sub))
  
  # keep track of geomorphic proportion table selection
  attr(geom.sub, 'selection') <- selection
  
  return(list(
    SPC = sub,
    geom = geom.sub
  ))
}
