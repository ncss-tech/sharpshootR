##
## needs more testing!!!
##





## TODO: should this account for rho(lag) ?
# estimation of effective sample size (ESS)
# (Fortin & Dale 2005, p. 223, Equation 5.15
# using global Moran's I as 'rho'
ESS_by_Moran_I <- function(n, rho) {
  
  # TODO: what about negative spatial autocorrelation?
  # hack: clamping rho {0,1}
  rho <- ifelse(rho < 0, 0, rho)
  n_eff <- n * ((1-rho) / (1+rho))
  
  return(n_eff)
}





## compute Moran's I using a subset of sample collectioned within the extent of MU
## this is likely an under-estimate of SA because we are including pixels both inside/outside MU delineations
##
# r: single raster layer
# mu.extent: SpatialPolygons representation of MU polygons BBOX (raster::extent)
# n: number of regular samples (what is a reasonable value?)
# k: number of neighbors used for weights matrix
# do.correlogram: compute correlogram?
# cor.order: order of correlogram
Moran_I_ByRaster <- function(r, mu.extent, n=NULL, k=NULL, do.correlogram=FALSE, cor.order=5) {
  

  ## NOTE: this will include raster "information" between map unit polygons
  # crop to extent of map units
  mu.extent <- spTransform(mu.extent, CRS(proj4string(r)))
  r <- raster::crop(r, mu.extent)
  
  # setup some sensible defaults
  # sample size for Moran's I:
  # use the number of pixels or 10k which ever is smaller
  if(is.null(n)) {
    n <- ifelse(length(r) < 10000, length(r), 10000)
  }
  
  # number of neighbors
  # 5 should be enough
  if(is.null(k)) {
    k <- 5
  }
  
  # sample raster and remove NA
  s <- raster::sampleRegular(r, n, sp=TRUE)
  s <- s[which(!is.na(s[[1]])), ]
  
  # neighborhood weights
  s.n <- spdep::knearneigh(s, k=k)
  s.nb <- spdep::knn2nb(s.n)
  s.listw <- spdep::nb2listw(s.nb)
  
  # I as a function of "lag"
  if(do.correlogram) {
    s.sp <- spdep::sp.correlogram(s.nb, s[[1]], order=cor.order, method='I')
  } else {
    s.sp <- NULL
  }
  
  # Moran's I, no need for permutation tests
  res <- spdep::moran.test(s[[1]], s.listw, rank=TRUE, randomisation = FALSE)
  
  if(do.correlogram)
    return(list(I=res$estimate[1], correlogram=s.sp))
  else
    return(res$estimate[1])
  
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
