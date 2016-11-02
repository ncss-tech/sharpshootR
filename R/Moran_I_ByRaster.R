##
## needs more testing!!!
##

## TODO: implement function with ape::Moran.I

# ## TODO: this isn't very fast for large N
# ## TODO: should this perform tests at increasing lags?
# .Moran <- function(pts, val, k=3) {
#   # compute spatial weights matrix from k-nearest neighbors
#   ## some time wasted here...
#   pts.n <- spdep::knearneigh(pts, k=k)
#   pts.nb <- spdep::knn2nb(pts.n)
#   pts.listw <- spdep::nb2listw(pts.nb)
#   # get Moran's I from result (don't need test stats or p-value)
#   I <- as.vector(spdep::moran.test(val, pts.listw, rank=TRUE, randomisation = FALSE)$estimate[1])
#   return(I)
# }



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
# r: single raster layer
# mu.extent: exent of all MU polygons
# n: number of regular samples
# k: number of neighbors used for weights matrix
# do.correlogram: compute correlogram?
# cor.order: order of correlogram
Moran_I_ByRaster <- function(r, mu.extent, n=10000, k=5, do.correlogram=FALSE, cor.order=5) {
  
  # crop to extent of map units
  mu.extent <- spTransform(mu.extent, CRS(proj4string(r)))
  r <- crop(r, mu.extent)
  
  # sample raster and remove NA
  s <- sampleRegular(r, 10000, sp=TRUE)
  s <- s[which(!is.na(s[[1]])), ]
  
  # neighborhood weights
  s.n <- spdep::knearneigh(s, k=5)
  s.nb <- spdep::knn2nb(s.n)
  s.listw <- spdep::nb2listw(s.nb)
  
  # I as a function of "lag"
  if(do.correlogram) {
    s.sp <- spdep::sp.correlogram(s.nb, s[[1]], order=cor.order, method='I')
  } else {
    s.sp <- NULL
  }
  
  # Moran's I, no need for permutation tests
  res <- moran.test(s[[1]], s.listw, rank=TRUE, randomisation = FALSE)
  
  if(do.correlogram)
    return(list(I=res$estimate[1], correlogram=s.sp))
  else
    return(res$estimate[1])
  
}
