

constantDensitySampling <- function(x, polygon.id='pID', parallel=FALSE, cores=NULL, n.pts.per.ac=1, min.samples=5, sampling.type='regular', iterations=10) {
  
  # sanity check: this must be a projected CRS
  if(!is.projected(x)) {
    stop('input polygons must be in a projected coordinate system with units of meters', call. = FALSE)
  }
  
  ## 2016-10-13: return NULL when there are no features
  # must have > 0 features
  if(length(x) == 0)
    return(NULL)
  
  # retain proj4 information
  p4s <- proj4string(x)
  
  ## NOT ready for prime time, usually slower than sequential
  ## https://github.com/ncss-tech/sharpshootR/issues/10
  ## requires slightly more RAM
  ## significant overhead
  ## not worth doing when nrow(x) < number of cores
  if(parallel) {
    
    # establish possible number of CPU cores if not specified
    if(is.null(cores)) {
      # the smaller: available CPU cores | number of polygons
      cores <- min(c(parallel::detectCores(), nrow(x)))
    }
    
    # init nodes
    ## TODO: optimal setting for useXDR ?
    ## TODO: makeCluster() [platform agnostic] or makePSOCKcluster() [Windoze]
    cl <- parallel::makePSOCKcluster(cores, useXDR=TRUE)
    
    ## probably not required?
    # # setup clusters
    # parallel::clusterEvalQ(cl, {
    #   library(sp)
    #   library(sharpshootR)
    #   return(NULL)
    # })
    
    # parallel sampling
    # sample and return a list, one element / valid polygon
    res <- parallel::parLapply(cl=cl, X=slot(x, 'polygons'), 
                                 fun=sample.by.poly, 
                                 n.pts.per.ac=n.pts.per.ac, 
                                 min.samples=min.samples, 
                                 sampling.type=sampling.type, 
                                 iterations=iterations,
                                 p4s=p4s)
    
    # stop nodes
    parallel::stopCluster(cl)
  }

    
  # sequential processing
  # sample and return a list, one element / valid polygon
  res <- lapply(slot(x, 'polygons'), 
                FUN=sample.by.poly, 
                n.pts.per.ac=n.pts.per.ac, 
                min.samples=min.samples, 
                sampling.type=sampling.type, 
                iterations=iterations,
                p4s=p4s)
  
  
  # check for NULL in this list:
  # this happens when there aren't enough sample points based on min.samples
  # * cases where it was too difficult to place a point
  # * could be caused by invalid geometry / topological error 
  #   --> spsample() says: "cannot derive coordinates from non-numeric matrix"
  
  null.items <- which(sapply(res, is.null))
  if(length(null.items) > 0) {
    message('some polygons were too small for the requested number of samples')
    res <- res[-null.items]
  }
  
  # if there are no polygons large enough to sample, return NULL
  if(length(res) < 1)
    return(NULL)
  
  
  # when rbind-ing the result there is an error related to duplicate rownames
  # reset with running counter
  total <- 0
  for(i in seq_along(res)) {
    
    # number of points
    n <- length(res[[i]])
    
    # new row names seq
    i.seq <- seq(from=(total + 1), to=(total + n))
    
    # reset rownames
    dimnames(res[[i]]@coords)[[1]] <- as.character(i.seq)
    
    # increment counter
    total <- total + n
  }
  
  # convert into a single SP object
  if(length(res) > 0)
    res <- do.call('rbind', res)
  
  # upgrade to SPDF with polygon ID
  pID.df <- sp::over(res, x)[, polygon.id, drop=FALSE]
  res <- SpatialPointsDataFrame(res, data=pID.df)

  return(res)
}


# sample by polygon, must be from a projected CRS
# p: Polygon object
# n: number of points per acre (results will be close)
# min.samples: minimum requested samples / polygon
# iterations: number of sampling "tries"
# p4s: proj4string assigned to SpatialPoints object
sample.by.poly <- function(p, n.pts.per.ac=1, min.samples=5, sampling.type='regular', iterations=10, p4s=NULL) {
  # convert _projected_ units to acres
  ac.i <- p@area * 2.47e-4
  
  # determine number of points based on requested density
  n.samples <- round(ac.i * n.pts.per.ac)
  
  # polygon must be at least large enough to support requested number of samples
  if(n.samples >= min.samples) {
    # trap errors caused by bad geometry
    s.i <- try(spsample(p, n=n.samples, type=sampling.type, iter=iterations), silent=TRUE)
    
    # errors: return NULL
    # invalid geometry could be the cause
    if(class(s.i) == 'try-error') {
      # print error for debugging
      message(paste0('sample.by.poly: ', attr(s.i, 'condition')), call. = FALSE)
      # can't do much else, move on to the next feature
      return(NULL)
    }

    
    # assign original proj4 string
    if(!is.null(p4s) & !is.null(s.i))
      proj4string(s.i) <- p4s
    
    return(s.i)
  }
  
  # not enough samples, return NULL
  else
    return(NULL)
}
