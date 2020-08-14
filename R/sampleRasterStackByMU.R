
## 2016-11-02: total re-write of ESS via Moran's I: based on pre-sampling of cropped rasters, much faster... useful?
## 2016-10-28: added support for estimating effective sample size via Moran's I -- currently very slow for large n: 
## 2016-08-27: abstracted sampling / extraction into single function

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
# estimateEffectiveSampleSize: estimate effective sampling size via Moran's I

sampleRasterStackByMU <- function(mu, mu.set, mu.col, raster.list, pts.per.acre, p=c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), progress=TRUE, estimateEffectiveSampleSize=TRUE) {
  
  # sanity check: package requirements
  if(!requireNamespace('rgdal') | !requireNamespace('rgeos') | !requireNamespace('raster') | !requireNamespace('spdep'))
    stop('please install the packages: rgdal, rgeos, raster, spdep', call. = FALSE)
  
  # enforce projected CRS
  if(!is.projected(mu))
    stop('map unit polygons must be in a projected CRS', call.=FALSE)
  
  # check for invalid geometry
  validity.res <- data.frame(id=mu[[mu.col]], Plolygon.Validity=rgeos::gIsValid(mu, byid=TRUE, reason=TRUE), stringsAsFactors = FALSE)
  
  # init containers for intermediate results
  l.mu <- list() # samples
  l.unsampled <- list() # un-sampled polygon IDs
  a.mu <- list() # area stats
  
  # load pointers to raster data
  raster.list <- rapply(raster.list, how='replace', f=function(i) {
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
  
  # these are the names assigned to each raster layer
  nm <- unlist(lapply(raster.list, names))
  
  ## TODO: not sure how to get a progress bar for this...
  # https://cran.r-project.org/web/packages/pbapply/index.html
  
  # recursively iterate over sets of raster data, attempting to load into memory
  raster.list <- rapply(raster.list, how='replace', f=function(r) {
    # attempt reading into memory
    r.mem <- try(raster::readAll(r), silent = TRUE)
    # if successful, return pointer to inMemory version
    if(class(r.mem) == 'RasterLayer' ) {
      return(r.mem)
    } else {
      # if not possible, retain the original file-based pointer
      return(r)
    }
  })
  
  
  ##
  ## check that raster extent completely covers mu extent
  ##
  
  # get MU extent, in original CRS
  e.mu <- as(raster::extent(mu), 'SpatialPolygons')
  proj4string(e.mu) <- proj4string(mu)
  
  message('Checking raster/MU extents...')
  
  # recursively iterate over sets of raster data, checking extents
  raster.containment.test <- rapply(raster.list, f=function(r) {
    # get current raster extent in original CRS
    e.r <-as(raster::extent(r), 'SpatialPolygons')
    proj4string(e.r) <- proj4string(r)
    
    # transform MU extent to CRS of current raster
    e.mu.r <- spTransform(e.mu, CRS(proj4string(e.r)))
    
    # check for complete containment of MU by current raster
    return(rgeos::gContainsProperly(e.r, e.mu.r))
  })
  
  
  ## TODO: finish this
  if(any(! raster.containment.test))
    warning('Raster extent does not completly cover MU extent')
  
  
  ## Moran's I by raster
  # result is a data.frame based on sampling of the raster sources, after cropping to mu extent
  if(estimateEffectiveSampleSize) {
    message('Estimating effective sample size...')
    # note: this only makes sense for "continuous" type raster data sources
    MI <- ldply(raster.list$continuous, Moran_I_ByRaster, mu.extent = e.mu, .progress = ifelse(progress, 'text', NULL))
    names(MI) <- c('Variable', 'Moran.I')
  } else {
    # when Moran's I is 0, n_effective = n
    MI <- data.frame(Variable=nm, Moran.I=0)
  }
  
  
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
      
      ## TODO: test parallel processing here
      # http://www.guru-gis.net/extract-raster-values-in-parallel/
      
      ## TODO: figure out Rast I/O errors sometimes generated here:
      # https://github.com/ncss-tech/soilReports/issues/48
      
      # extract raster data, sample ID, polygon ID to DF
      l.mu[[mu.i]] <- rapply(raster.list, how = 'replace', f=function(r) {
        res <- data.frame(value=raster::extract(r, s), pID=s$pID, sid=s$sid)
        return(res)
      })
      
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
      
      # save stats to list indexed by map unit ID
      a.mu[[mu.i]] <- a.stats
      
      if(progress)
        setTxtProgressBar(pb, match(mu.i, mu.set))
    }
    
  } # done iterating over map units
  
  if(progress)
    close(pb)
  
  # assemble MU area stats
  mu.area <- ldply(a.mu)
  
  # iterate over map unit collections of samples
  # structure looks like this:
  #  $ 7011:List of 3
  # ..$ continuous :List of 9
  # ..$ categorical:List of 3
  # ..$ circular   :List of 1
  # $ 5012:List of 3
  # ..$ continuous :List of 9
  # ..$ categorical:List of 3
  # ..$ circular   :List of 1
  # $ 7085:List of 3
  # ..$ continuous :List of 9
  # ..$ categorical:List of 3
  # ..$ circular   :List of 1
  
  d.mu <- lapply(l.mu, function(i) {
    
    # extract each variable type
    # fix names
    # remove NA
    .processVars <- function(j) {
      # extract pieces
      d <- ldply(j)
      # check to make sure there are some rows to process
      if(nrow(d) > 0) {
        # fix names
        names(d)[1] <- 'variable'
        # remove NA
        d <- na.omit(d)
      }
      
      return(d)
    }
    
    # iterate over varible types, retaining list structure
    res <- lapply(i, .processVars)
    
    # assemble into DF and fix names
    res <- ldply(res)
    names(res)[1] <- 'variable.type'
    
    return(res)
  })
  
  # assemble into data.frame
  d.mu <- ldply(d.mu)
  
  ## unspool polygon sample IDs when no samples were collected
  unsampled.idx <- unlist(l.unsampled)
  
  # get raster summary
  rs <- rapply(raster.list, f=raster::filename, how = 'unlist')
  rs <- gsub('\\\\', '/', rs)
  
  rs.df <- data.frame(Variable = nm, File = rs, 
                      Resolution = as.character(signif(as.numeric(rapply(raster.list, res, how = 'unlist')), 2)), 
                      inMemory = as.character(rapply(raster.list, f = raster::inMemory, how = 'unlist')), 
                      ContainsMU = raster.containment.test)
  
  # join-in Moran's I
  rs.df <- join(rs.df, MI, by='Variable', type='left')
  
  # replace missing Moran's I with 0
  # this should only affect categorical / circular variables
  rs.df$Moran.I[is.na(rs.df$Moran.I)] <- 0
  
  
  # combine into single object and result
  return(list('raster.samples'=d.mu, 'area.stats'=mu.area, 'unsampled.ids'=unsampled.idx, 'raster.summary'=rs.df, 'mu.validity.check'=validity.res, 'Moran_I'=MI))
}



## Moran's I by polygon: very slow

# 
# ## TODO: this may be far too slow
# if(estimateEffectiveSampleSize) {
#   message(paste0('   Estimating effective sample size: ', i.name))
#   
#   # compute within each polygon: slightly faster
#   ss <- list()
#   # split spatial samples / extracted raster data by polygon into lists, indexed by pID
#   s.polys <- split(s, as.character(s$pID))
#   v.polys <- split(l[[i.name]]$value, as.character(l[[i.name]]$pID))
#   
#   # iterate over polygons
#   for(this.poly in names(s.polys)) {
#     
#     ## TODO: errors here (?) when used from package, but not when sourced locally
#     # starting sample size
#     n <- nrow(s.polys[[this.poly]])
#     
#     ## TODO: errors here (?) when used from package, but not when sourced locally
#     # attempt to compute Moran's I
#     rho <- try(.Moran(s.polys[[this.poly]], v.polys[[this.poly]]), silent = TRUE)
#     
#     # if successful
#     if(class(rho) != 'try-error') {
#       # compute effective sample size and save
#       n_eff <- .effective_n(n, rho)
#       ss[[this.poly]] <- data.frame(Moran_I=round(rho, 3), n_eff=round(n_eff), n=n)
#     }
#     else {
#       # otherwise use NA
#       ss[[this.poly]] <- data.frame(Moran_I=NA, n_eff=NA, n=n)
#     }
#   }
#   
# } else { # otherwise return NA
#   ## TODO: finish this
#   # ss[[this.poly]] <- data.frame(Moran_I=NA, n_eff=NA, n=n)
# }
# 
# 
# 
# # save stats computed by polygon to list indexed by raster name
# ss <- ldply(ss)
# names(ss)[1] <- 'pID'
# l.s[[i.name]] <- ss
# 



