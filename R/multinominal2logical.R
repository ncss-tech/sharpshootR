
## experimental / probably not very efficient 
## function for converting a column of multinominal values into logical matrix
## x: SPC
## v: site-level variable name, must be a factor




#' @title Convert Multinominal to Logical Matrix
#' 
#' @description Convert a single multinominal, site-level attribute from a `SoilProfileCollection` into a matrix of corresponding logical values. The result contains IDs from the `SoilProfileCollection` and can easily be joined to the original site-level data.
#'
#' @param x a `SoilProfileCollection` object
#' @param v the name of a site-level attribute that is a factor, or can be coerced to a factor, with more than 2 levels
#'
#' @return A `data.frame` with IDs in the first column, and as many columns of logical vectors as there were levels in `v`. See examples.
#' 
#' @author D.E. Beaudette
#' 
#' @seealso [diagnosticPropertyPlot()]
#' 
#' @keywords manip
#' 
#' @export
#'
#' @examples
#' 
#' \donttest{
#' 
#' if(require(soilDB) &
#'    require(aqp) &
#'    require(latticeExtra)) {
#'   
#'   
#'   # sample data, an SPC
#'   data(loafercreek, package='soilDB')
#'   
#'   # convert to logical matrix
#'   hp <- multinominal2logical(loafercreek, 'hillslopeprof')
#'   
#'   # join-in to site data
#'   site(loafercreek) <- hp
#'   
#'   # variable names
#'   v <- c('lithic.contact', 'paralithic.contact', 
#'          'argillic.horizon', 'toeslope', 'footslope', 
#'          'backslope', 'shoulder', 'summit')
#'   
#'   # visualize with some other diagnostic features
#'   x <- diagnosticPropertyPlot(loafercreek, v, k = 5, 
#'                               grid.label = 'bedrckkind', dend.label = 'upedonid')  
#' }

#' 
#' }
#' 
multinominal2logical <- function(x, v) {
  
  if( ! inherits(x, 'SoilProfileCollection'))
    stop('`x` must be a SoilProfileCollection', call. = FALSE)
  
  # get internal ID name
  id <- idname(x)
  
  # site data only
  s <- site(x)
  
  # if not a factor, attempt to convert to one
  if( ! inherits(s[[v]], 'factor')) {
    s[[v]] <- factor(s[[v]])
    
    if(length(levels(s[[v]])) < 2 ) {
      stop('`v` should have more than 2 unique values;', call. = FALSE)
    }
  }
    
  
  # construct formula for dcast
  fm <- paste0(idname(x), ' ~ ', v)
  
  # cast to wide format, filling non-NA entries with ID
  w <- dcast(s, fm, value.var = id, drop = FALSE) 
  
  # not done yet, neet to convert into logical
  # first column is the ID
  w.logical <- sapply(w[, -1], function(i) ! is.na(i))
  
  # merge ID back in
  w.final <- data.frame(w[, 1], w.logical[, levels(s[[v]])], stringsAsFactors = FALSE)
  names(w.final)[1] <- id
  
  return(w.final)
}

