
## load raster stack into memory if possible
## perform constant density sampling across subsets of map units
## extract raster values from stack
## combine into list / data.frame
# mu: map unit polygons, in projected CRS
# mu.set: character vector of map units to work on
# mu.col: column used to subset map units
# raster.list: 

sampleRasterStackByMU <- function(mu, mu.set, mu.col, raster.list, pts.per.acre, progress=TRUE) {
  
  # enforce projected CRS
  if(!is.projected(mu))
    stop('map unit polygons must be in a projected CRS', call.=FALSE)
  
  # init containers for intermediate results
  l.mu <- list() # samples
  l.unsampled <- list() # un-sampled polygon IDs
  a.mu <- list() # area stats
  
  # load pointers to raster data
  raster.list <- lapply(raster.list, function(i) {
    i <- try(raster(i))
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
    r <- try(readAll(raster.list[[i]]), silent = TRUE)
    # if successful, move into list
    if(class(r) == 'RasterLayer' )
      raster.list[[i]] <- r
    
    if(progress)
      setTxtProgressBar(pb, i)
  }
  
  if(progress)
    close(pb)
  
  
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
      l <- list()
      for(i in seq_along(raster.list)) {
        i.name <- names(raster.list)[i]
        l[[i.name]] <- data.frame(value=extract(raster.list[[i]], s), pID=s$pID, sid=s$sid)
      }
      
      # convert to DF and fix default naming of raster column
      d <- ldply(l)
      names(d)[1] <- 'variable'
      
      # extract polygon areas as acres
      a <- sapply(slot(mu.i.sp, 'polygons'), slot, 'area') * 2.47e-4
      
      # compute additional summaries
      .quantiles <- quantile(a, probs=c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1))
      .total.area <- sum(a)
      .samples <- nrow(s)
      .mean.sample.density <- round(.samples / .total.area, 2)
      .polygons <- length(a)
      .unsampled.polygons <- length(l.unsampled[[mu.i]])
      
      # compile into single row
      a.stats <- c(round(c(.quantiles, .total.area, .samples, .polygons, .unsampled.polygons)), .mean.sample.density)
      
      # fix name
      names(a.stats) <- c('Min', 'Q5', 'Q25', 'Median', 'Q75', 'Q95', 'Max', 'Total Area', 'Samples', 'Polygons', 'Polygons Not Sampled', 'Mean Sample Dens.')
      
      # save and continue
      a.mu[[mu.i]] <- a.stats
      l.mu[[mu.i]] <- d
      
      if(progress)
        setTxtProgressBar(pb, match(mu.i, mu.set))
    }
    
  }
  
  if(progress)
    close(pb)
  
  # assemble into DF
  d.mu <- ldply(l.mu)
  unsampled.idx <- unlist(l.unsampled)
  mu.area <- ldply(a.mu)
  
  # get raster summary
  rs <- sapply(raster.list, filename)
  rs <- gsub('\\\\', '/', rs)
  rs <- data.frame(Variable=names(rs), File=rs, inMemory=as.character(sapply(raster.list, inMemory)))
  
  # combine into single object and result
  return(list('raster.samples'=d.mu, 'area.stats'=mu.area, 'unsampled.ids'=unsampled.idx, 'raster.summary'=rs))
}


