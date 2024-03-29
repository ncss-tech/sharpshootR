

constantDensitySampling <- function(x, polygon.id='pID', parallel=FALSE, cores=NULL, n.pts.per.ac=1, min.samples=5, sampling.type='regular') {
  
  if (!requireNamespace("terra")) {
    stop('please install the `terra` package', call. = FALSE)
  }
  
  # attempt conversion to terra object
  if (!inherits(x, 'SpatVector')) {
    x <- terra::vect(x)
  }
  
  # sanity check: this must be a projected CRS
  if (terra::is.lonlat(x)) {
    stop('input polygons must be in a projected coordinate system with units of meters', call. = FALSE)
  }
  
  ## 2016-10-13: return NULL when there are no features
  # must have > 0 features
  if (length(x) == 0)
    return(NULL)
  
  # retain proj4 information
  p4s <- terra::crs(x)
  
  # ## see https://github.com/ncss-tech/sharpshootR/issues/10
  # ## NOT ready for prime time, usually slower than sequential
  # ## https://github.com/ncss-tech/sharpshootR/issues/10
  # ## requires slightly more RAM
  # ## significant overhead
  # ## not worth doing when nrow(x) < number of cores
  if (parallel) {
    .Deprecated(msg = "`parallel` argument is deprecated")

    # establish possible number of CPU cores if not specified
    if (is.null(cores)) {
      # the smaller: available CPU cores | number of polygons
      cores <- min(c(parallel::detectCores(), nrow(x)))
    }

    # init nodes
    ## TODO: optimal setting for useXDR ?
    ## TODO: makeCluster() [platform agnostic] or makePSOCKcluster() [Windoze]
    cl <- parallel::makeCluster(cores, useXDR = TRUE)

    # parallel sampling
    # sample and return a list, one element / valid polygon
    res <- parallel::parLapply(cl = cl, X = split(x, 1:length(x)),
                               fun = sample.by.poly,
                               n.pts.per.ac = n.pts.per.ac,
                               min.samples = min.samples,
                               sampling.type = sampling.type,
                               p4s = p4s)

    # stop nodes
    parallel::stopCluster(cl)
  }

    
  # sequential processing
  # sample and return a list, one element / valid polygon
  res <- lapply(
    1:nrow(x),
    FUN = function(i) {
      smp <- sample.by.poly(
        x[i,],
        n.pts.per.ac = n.pts.per.ac,
        min.samples = min.samples,
        sampling.type = sampling.type,
        p4s = p4s
      )
      smp$pID <- i
      smp
      }
    )
  
  # check for NULL in this list:
  # this happens when there aren't enough sample points based on min.samples
  # * cases where it was too difficult to place a point
  # * could be caused by invalid geometry / topological error 
  
  null.items <- which(sapply(res, is.null))
  if (length(null.items) > 0) {
    message('some polygons were too small for the requested number of samples')
    res <- res[-null.items]
  }
  
  # if there are no polygons large enough to sample, return NULL
  if (length(res) < 1) {
    return(NULL)
  } else if (length(res) > 1) { 
    res <- do.call('rbind', res)
  } else {
    res <- res[[1]]
  }
  
  # add polygon ID by intersection
  res[[polygon.id]] <- x[[polygon.id]][[1]][apply(terra::relate(res, x, "intersects"), MARGIN = 1, which)]

  return(res)
}


# sample by polygon, must be from a projected CRS
# p: Polygon object
# n: number of points per acre (results will be close)
# min.samples: minimum requested samples / polygon
# p4s: proj4string assigned to SpatialPoints object
sample.by.poly <- function(p,
                           n.pts.per.ac = 1,
                           min.samples = 5,
                           sampling.type = 'regular',
                           p4s = NULL
) {
  
  if (!requireNamespace("terra")) {
    stop('please install the `terra` package', call. = FALSE)
  }
  
  # attempt conversion to terra object
  if (!inherits(p, 'SpatVector')) {
    p <- terra::vect(p)
  }
  
  # convert _projected_ units to acres
  ac.i <- terra::expanse(p) * 2.47e-4
  
  # determine number of points based on requested density
  n.samples <- round(ac.i * n.pts.per.ac)
  
  # if polygon is too small for given density, request the minimum number of samples
  if (n.samples < min.samples) {
    n.samples <- min.samples
  }
  
  if (n.samples > 0) {
    # trap errors caused by bad geometry
    s.i <- try(terra::spatSample(p, size = n.samples, method = sampling.type), silent = TRUE)
    
    # errors: return NULL
    # invalid geometry could be the cause
    if (inherits(s.i, 'try-error')) {
      # print error for debugging
      message(paste0('sample.by.poly: ', attr(s.i, 'condition')), call. = FALSE)
      # can't do much else, move on to the next feature
      return(NULL)
    }

    # assign original proj4 string
    if (!is.null(p4s) & !is.null(s.i))
      terra::crs(s.i) <- p4s
    
    return(s.i)
  } else return(p[0,])
}
