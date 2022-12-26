
#' @title Generate a unique ID for line segments
#' @description Generate a unique ID for a line segment, based on the non-cryptographic murmur32 hash.
#' 
#' @param x an `sf` object, with 1 line segment per feature
#' @param precision digits are rounded to this many places to the right (negative) or left (positive) of the decimal place
#' @param algo hash function algorithm, passed to [digest::digest()]
#'
#' @return A vector of unique IDs created from the hash of line segment start and end vertex coordinates. Unique IDs are returned in the order of records of `x` and can therefore be saved into a new column of the associated attribute table. `NA` is returned for empty geometries.
#' 
#' @details The input `sf` object must NOT contain multi-part features. The precision specified should be tailored to the coordinate system in use and the snapping tolerance used to create join decision line segments. A `precision` of 4 is reasonable for geographic coordinates (snapping tolerance of 0.0001 degrees or ~ 10 meters). A `precision` of -1 (snapping tolerance of 10 meters) is reasonable for projected coordinate systems with units in meters.
#' 
#' @author D.E. Beaudette
#' 
#' @note An error is issued if any non-unique IDs are generated. This could be caused by using coordinates that do not contain enough precision for unique hashing.
#' 
#' @export
#'
#' @examples
#' 
#' if(requireNamespace("sf")) {
#' 
#' # 10 random line segments
#' # shared end vertices
#' .x <- runif(n = 11, min = 0, max = 100)
#' .y <- runif(n = 11, min = 0, max = 100)
#' m <- matrix(c(.x, .y), ncol = 2, byrow = TRUE)
#' 
#' # init LINESTRING geometries
#' a <- lapply(1:(nrow(m) - 1), function(i) {
#'   .idx <- c(i, i+1)
#'   geom <- sf::st_sfc(sf::st_linestring(m[.idx, ]))
#'   a <- sf::st_sf(geom)
#'   
#' })
#' 
#' # flatten list -> 10 feature sf object
#' a <- do.call('rbind', a)
#' 
#' # line hashes
#' a$id <- generateLineHash(a, precision = 0)
#' 
#' # graphical check
#' plot(a, lwd = 2, key.width = lcm(4), axes = TRUE, las = 1)
#' 
#' 
#' # simulate empty geometry
#' a$geom[2] <- sf::st_sfc(sf::st_linestring())
#' 
#' # NA returned for empty geometry
#' generateLineHash(a, precision = 0)
#' 
#' }
#' 
#' 
generateLineHash <- function(x, precision = -1, algo = 'murmur32') {
  
  # requires sf
  if(!requireNamespace('sf'))
    stop('please install the `venn` and `gower` packages', call.=FALSE)
  
  # check that this is not a multi-geometry
  if(any(grepl('multi', sf::st_geometry_type(x), ignore.case = TRUE))) {
    stop('this function can only be applied to single-part features', call. = FALSE)
  }
  
  # cannot be used for points / polygons
  if(any(grepl('point|polygon', sf::st_geometry_type(x), ignore.case = TRUE))) {
    stop('this function can only be applied to single-part line features', call. = FALSE)
  }
  
  ## TODO: what about n vertices > 2?
  
  # iterate over geometry
  .res <- sapply(1:nrow(x), function(i) {
    
    # extract line coordinates
    .xy <- sf::st_coordinates(x[i, ])
    
    # number of vertices
    n <- nrow(.xy)
    
    # return NA for empty or degenerate geometry
    if(n < 2) {
      return(NA)
    }
    
    # hash is based on first / last vertex coordinates
    .start <- .xy[1, 1:2]
    .end <- .xy[n, 1:2]
    
    # round to specified precision
    .coords <- round(c(.start, .end), digits = precision)
    hash <- digest(.coords, algo = algo)
    
    return(hash)
  })
  
  # check for collisions in hash
  if(any(table(.res) > 1)) {
    stop('collision in hash function, consider increasing precision', call. = FALSE)
  }
    
  return(.res)
}
