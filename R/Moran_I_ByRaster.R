##
## needs more testing!!!
## needs documentation
##



## TODO: should this account for rho(lag) ?
# 
# (Fortin & Dale 2005, p. 223, Equation 5.15 using global Moran's I as 'rho'
# 
#' Estimate Effective Sample Size
#' 
#' Estimation of effective sample size (ESS). See Fortin & Dale 2005, p. 223, Equation 5.15 using global Moran's I as 'rho'. 
#'
#' @param n sample size
#' @param rho Global Moran's I
#' @references Fortin, M.J. and Dale, M.R.T. (2005) Spatial Analysis: A Guide for Ecologists. Cambridge University Press, Cambridge, 1-30.
#' @return numeric; estimated Effective Sample Size
#' @author D.E. Beaudette
#' @export
ESS_by_Moran_I <- function(n, rho) {
  
  # TODO: what about negative spatial autocorrelation?
  # hack: clamping rho {0,1}
  rho <- ifelse(rho < 0, 0, rho)
  n_eff <- n * ((1 - rho) / (1 + rho))
  
  return(n_eff)
}

#' Compute Moran's I for a raster sampled from a mapunit extent
#' 
#' Compute Moran's I using a subset of sample collected within the extent of a mapunit. This is likely an under-estimate of SA because we are including pixels both inside/outside MU delineations
#'
#' @param r single `SpatRaster`
#' @param mu.extent `SpatVector` representation of mapunit polygons bounding box (via `terra::ext()`)
#' @param n number of regular samples (what is a reasonable value?)
#' @param k number of neighbors used for weights matrix
#' @param do.correlogram compute correlogram?
#' @param cor.order order of correlogram
#' @param crop.raster optionally disable cropping of the raster layer
#'
#' @return If `do.correlogram` is `TRUE` a list with estimated Moran's I (`$I`) and the correlogram (`$correlogram`), otherwise the estimated Moran's I value. 
#' @details This function uses the `spdep::moran.test()` function
#' @author D.E. Beaudette
#' @export
Moran_I_ByRaster <- function(r, mu.extent=NULL, n=NULL, k=NULL, do.correlogram=FALSE, cor.order=5, crop.raster=TRUE) {
  
  if (!requireNamespace("terra"))
    stop('please install the `terra` package')
  
  if (!requireNamespace("spdep"))
    stop('please install the `spdep` package')
  
  if (crop.raster && !is.null(mu.extent)) {
    ## NOTE: this will include raster "information" between map unit polygons
    # crop to extent of map units
    mu.extent <- terra::project(terra::as.polygons(terra::ext(mu.extent), 
                                                   crs = terra::crs(mu.extent)), 
                                terra::crs(r))
    r <- terra::crop(r, mu.extent)
  }
  
  # setup some sensible defaults
  # sample size for Moran's I:
  # use the number of pixels or 10k which ever is smaller
  if (is.null(n)) {
    n <- ifelse(terra::ncell(r) < 10000, terra::ncell(r), 10000)
  }
  
  # number of neighbors
  # 5 should be enough
  if (is.null(k)) {
    k <- 5
  }
  
  # sample raster
  s <- suppressWarnings(terra::spatSample(r, size = n, na.rm = TRUE, as.points = TRUE))
  
  # neighborhood weights
  s.n <- spdep::knearneigh(as(s, 'Spatial'), k = k)
  s.nb <- spdep::knn2nb(s.n)
  s.listw <- spdep::nb2listw(s.nb)
  
  # I as a function of "lag"
  if (do.correlogram) {
    s.sp <- spdep::sp.correlogram(s.nb, s[[1]][[1]], order = cor.order, method = 'I')
  } else {
    s.sp <- NULL
  }
  
  # Moran's I, no need for permutation tests
  res <- spdep::moran.test(s[[1]][[1]], s.listw, rank = TRUE, randomisation = FALSE)
  
  if (do.correlogram)
    return(list(I = res$estimate[1], correlogram = s.sp))
  
  res$estimate[1]
  
}


## this approach doesn't work all that well because map unit delineations cover only a couple PRISM raster cells
## a reasonable estimate should come from all polygons

# 
# Moran_I_ByPolygon <- function(r, mu, n=NULL, k=NULL) {
#   
#   
#   .M <- function(s, k) {
#     
#     # neighborhood weights
#     s.n <- spdep::knearneigh(s, k=k)
#     s.nb <- spdep::knn2nb(s.n)
#     s.listw <- spdep::nb2listw(s.nb)
#     
#     moran.test(s[[1]], s.listw, rank=TRUE, randomisation = FALSE)
#   }
#   
#   for(i in 1:nrow(mu) {
#     # crop to extent of map units
#     mu.i <- spTransform(mu[i, ], CRS(proj4string(r)))
#     r.i <- crop(r, mu.i)
#     
#     # mask to MU polygons
#     r.i <- mask(r.i, spTransform(mu.i, CRS(proj4string(r))))
#     
#     
#     # setup some sensible defaults
#     # sample size for Moran's I:
#     # use the number of pixels or 10k which ever is smaller
#     if(is.null(n)) {
#       n <- ifelse(length(r) < 10000, length(r), 10000)
#     }
#     
#     # number of neighbors
#     # 5 should be enough
#     if(is.null(k)) {
#       k <- 5
#     }
#     
#     # sample raster and remove NA
#     s <- sampleRegular(r.i, n, sp=TRUE)
#     s <- s[which(!is.na(s[[1]])), ]
#     
#   }
#   
#   
#     
#   
#   
#   # Moran's I, no need for permutation tests
#   res <-
#   
#   return(res$estimate[1])
# }
# 
# 
