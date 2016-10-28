
##
## needs more testing!!!
##

## TODO: implement function with ape::Moran.I

## TODO: this isn't very fast for large N
## TODO: should this perform tests at increasing lags?
.Moran <- function(s, val, k=3) {
  # compute spatial weights matrix from k-nearest neighbors
  ## some time wasted here...
  s.n <- spdep::knearneigh(s, k=k)
  s.nb <- spdep::knn2nb(s.n)
  s.listw <- spdep::nb2listw(s.nb)
  # get Moran's I from result (don't need test stats or p-value)
  I <- as.vector(spdep::moran.test(val, s.listw, rank=TRUE, randomisation = FALSE)$estimate[1])
  return(I)
}


## TODO: should this account for rho(lag) ?
# simple correction
# (Fortin & Dale 2005, p. 223, Equation 5.15
# using global Moran's I as 'rho'
.effective_n <- function(n, rho) {
  
  # TODO: what about negative spatial autocorrelation?
  # hack: clamping rho {0,1}
  rho <- ifelse(rho < 0, 0, rho)
  n_eff <- n * ((1-rho) / (1+rho))
  
  return(n_eff)
}


##
## needs more testing!!!
##


## TODO:
##   1. abstract into smaller functions
##   2. optimise for parallel execution
##   3. keep track of time spent on various sub-tasks

## load raster stack into memory if possible
## perform constant density sampling across subsets of map units
## extract raster values from stack
## combine into list / data.frame
# mu: map unit polygons, in projected CRS
# mu.set: character vector of map units to work on
# mu.col: column used to subset map units
# raster.list: see formatting in mu summary reports
# p: requested percentiles
# progress: print progress bar?

sampleRasterStackByMU <- function(mu, mu.set, mu.col, raster.list, pts.per.acre, p=c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), progress=TRUE, estimateEffectiveSampleSize=TRUE) {
  
  # sanity check: package requirements
  if(!requireNamespace('rgdal') | !requireNamespace('rgeos') | !requireNamespace('raster') | !requireNamespace('spdep'))
    stop('please install the packages: rgdal, rgeos, raster, spdep', call. = FALSE)
    
  # enforce projected CRS
  if(!is.projected(mu))
    stop('map unit polygons must be in a projected CRS', call.=FALSE)
  
  # init containers for intermediate results
  l.mu <- list() # samples
  l.unsampled <- list() # un-sampled polygon IDs
  a.mu <- list() # area stats
  l.spatial.stats <- list() # Moran's I and effective DF
  
  # load pointers to raster data
  raster.list <- lapply(raster.list, function(i) {
    i <- try(raster::raster(i))
    if(class(i) == 'try-error')
      stop(paste0('Cannot find raster file: ', i), call. = FALSE)
    else
      return(i)
  })
  
  
  ##
  ## iterate over rasters and read into memory if possible
  ##
  message('Loading raster data...')
  
  nm <- names(raster.list)
  # progress bar
  if(progress)
    pb <- txtProgressBar(min=0, max=length(nm), style=3)
  
  for(i in 1:length(nm)) {
    # attempt reading into memory
    r <- try(raster::readAll(raster.list[[i]]), silent = TRUE)
    # if successful, move into list
    if(class(r) == 'RasterLayer' )
      raster.list[[i]] <- r
    
    if(progress)
      setTxtProgressBar(pb, i)
  }
  
  if(progress)
    close(pb)
  
  
  ##
  ## check that raster extent completely covers mu extent
  ##
  
  # get MU extent, in original CRS
  e.mu <- as(raster::extent(mu), 'SpatialPolygons')
  proj4string(e.mu) <- proj4string(mu)
  
  raster.containment.test <- vector(mode='logical', length=length(nm))
  for(i in 1:length(nm)) {
    # get current raster extent in original CRS
    e.r <-as(raster::extent(raster.list[[i]]), 'SpatialPolygons')
    proj4string(e.r) <- proj4string(raster.list[[i]])
    
    # transform MU extent to CRS of current raster
    e.mu.r <- spTransform(e.mu, CRS(proj4string(e.r)))
    
    # check for complete containment of MU by current raster
    raster.containment.test[i] <- rgeos::gContainsProperly(e.r, e.mu.r)
  }
  
  ## TODO: finish this
  if(any(! raster.containment.test))
    warning('Raster extent does not completly cover MU extent')
  
  
  ##
  ## iterate over map units, sample, extract raster values
  ##
  message('Sampling polygons, and extracting raster values...')
  
  # progress bar
  if(progress)
    pb <- txtProgressBar(min=0, max=length(mu.set), style=3)
  
  for(mu.i in mu.set) {
    # get current polygons
    mu.i.sp <- mu[which(mu[[mu.col]] == mu.i), ]
    
    ## messages are issued when it isn't possibe to place the requested num. samples in a polygon
    # sample each polygon at a constant density
    suppressMessages(s <- constantDensitySampling(mu.i.sp, n.pts.per.ac=pts.per.acre, min.samples=1, polygon.id='pID', iterations=20))
    
    # keep track of un-sampled polygons
    l.unsampled[[mu.i]] <- setdiff(mu.i.sp$pID, unique(s$pID))
    
    ## ADDED 2016-08-27 to account for very small polygons
    ## see related fix in constantDensitySampling
    # if there are no samples, skip stats
    if(is.null(s)) {
      # save and continue
      a.mu[[mu.i]] <- NULL
      l.mu[[mu.i]] <- NULL
      
      if(progress)
        setTxtProgressBar(pb, match(mu.i, mu.set))
      # break out of current iteration, move to next map unit
      next
      
    } else {
      # make a unique sample ID, need this for conversion: long -> wide
      s$sid <- 1:nrow(s)
      
      # iterate over raster data
      l <- list() # used to store raster samples
      l.s <- list() # used to store spatial stats
      
      for(i in seq_along(raster.list)) {
        i.name <- names(raster.list)[i]
        # extract raster data, sample ID, polygon ID to DF
        l[[i.name]] <- data.frame(value=raster::extract(raster.list[[i]], s), pID=s$pID, sid=s$sid)
        
        ## TODO: this may be far too slow
        if(estimateEffectiveSampleSize) {
          message(paste0('   Estimating effective sample size: ', i.name))
          
          # compute within each polygon: slightly faster
          ss <- list()
          # split spatial samples / extracted raster data by polygon
          s.polys <- split(s, s$pID)
          s.polys <- split(l[[i.name]]$value, l[[i.name]]$pID)
          
          for(this.poly in names(s.polys)) {
            # attempt to compute Moran's I
            rho <- try(.Moran(s.polys[[this.poly]], v.polys[[this.poly]]), silent = FALSE)
            
            # if successful
            if(class(rho) != 'try-error') {
              # compute effective sample size and save
              n <- nrow(s.polys[[this.poly]])
              n_eff <- .effective_n(n, rho)
              ss[[this.poly]] <- data.frame(Moran_I=round(rho, 3), n_eff=round(n_eff), n=n)
            }
            else {
              # otherwise use NA
              ss[[this.poly]] <- data.frame(Moran_I=NA, n_eff=NA)
            }
          }
          
        } else { # otherwise return NA
          ## TODO: finish this
          # ss[[this.poly]] <- data.frame(Moran_I=NA, n_eff=NA)
        }
        # save stats computed by polygon to list indexed by raster name
        ss <- ldply(ss)
        names(ss)[1] <- 'pID'
        l.s[[i.name]] <- ss
      }
      
      # convert to DF and fix default naming of raster column
      d <- ldply(l)
      d.s <- ldply(l.s)
      names(d)[1] <- 'variable'
      names(d.s)[1] <- 'variable'
      
      # extract polygon areas as acres
      a <- sapply(slot(mu.i.sp, 'polygons'), slot, 'area') * 2.47e-4
      
      # compute additional summaries
      .quantiles <- quantile(a, probs=p)
      .total.area <- sum(a)
      .samples <- nrow(s)
      .mean.sample.density <- round(.samples / .total.area, 2)
      .polygons <- length(a)
      .unsampled.polygons <- length(l.unsampled[[mu.i]])
      
      # compile into single row
      a.stats <- c(round(c(.quantiles, .total.area, .samples, .polygons, .unsampled.polygons)), .mean.sample.density)
      
      # fix name
      names(a.stats) <- c('Min', 'Q5', 'Q25', 'Median', 'Q75', 'Q95', 'Max', 'Total Area', 'Samples', 'Polygons', 'Polygons Not Sampled', 'Mean Sample Dens.')
      
      # save stats to lists indexed by map unit ID
      a.mu[[mu.i]] <- a.stats
      l.mu[[mu.i]] <- d
      l.spatial.stats[[mu.i]] <- d.s
      
      if(progress)
        setTxtProgressBar(pb, match(mu.i, mu.set))
    }
    
  } # done iterating over map units
  
  if(progress)
    close(pb)
  
  # assemble into DF
  d.mu <- ldply(l.mu)
  unsampled.idx <- unlist(l.unsampled)
  mu.area <- ldply(a.mu)
  d.spatial.stats <- ldply(l.spatial.stats)
  
  # get raster summary
  rs <- sapply(raster.list, raster::filename)
  rs <- gsub('\\\\', '/', rs)
  rs <- data.frame(Variable=names(rs), File=rs, inMemory=as.character(sapply(raster.list, raster::inMemory)), ContainsMU=raster.containment.test)
  
  # combine into single object and result
  return(list('raster.samples'=d.mu, 'area.stats'=mu.area, 'unsampled.ids'=unsampled.idx, 'raster.summary'=rs, 'spatial.stats'=d.spatial.stats))
}


