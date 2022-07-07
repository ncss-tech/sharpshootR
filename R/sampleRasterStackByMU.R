
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

#' Sample a Raster Stack
#' 
#' Sample a raster stack by map unit polygons, at a constant density.
#' 
#' @param mu a `SpatialPolygonsDataFrame` object in a projected coordinate reference system (CRS)
#' @param mu.set character vector of map unit labels to be sampled
#' @param mu.col column name in attribute table containing map unit labels
#' @param raster.list a `list` containing raster names and paths, see details below
#' @param pts.per.acre target sampling density in `points per acre`
#' @param p percentiles for polygon area stats, e.g. `c(0.05, 0.25, 0.5, 0.75, 0.95)`
#' @param iterations Number of iterations, passed to `constantDensitySampling()`
#' @param progress logical, print a progress bar while sampling?
#' @param estimateEffectiveSampleSize estimate an effective sample size via Moran's I?
#' @param polygon.id Column name containing unique polygon IDs; default: `"pID"`; calculated if missing
#' @details This function is used by various NRCS reports that summarize or compare concepts defined by collections of polygons using raster data sampled from within each polygon, at a constant sampling density. Even though the function name includes "RasterStack", this function doesn't actually operate on the "stack" object as defined in the raster package. The collection of raster data defined in `raster.list` do not have to share a common coordinate reference system, grid spacing, or extent. Point samples generated from `mu` are automatically converted to the CRS of each raster before extracting values. The extent of each raster in `raster.list` must completely contain the extent of `mu`.
#' @return A `list` containing:
#' \describe{
#'   \item{`raster.samples`}{a `data.frame` containing samples from all rasters in the stack}
#'   \item{`area.stats`}{a `data.frame` containing area statistics for all map units in the collection}
#'   \item{`unsampled.ids`}{an index to rows in the original SPDF associated with polygons not sampled}
#'   \item{`raster.summary`}{a `data.frame` containing information on sampled rasters}
#'   \item{`Moran_I`}{a `data.frame` containing estimates Moran's I (index of spatial autocorrelation)}
#' 	} 
#' @author D.E. Beaudette
#' @seealso \code{\link{constantDensitySampling}}, \code{\link{sample.by.poly}}
#' @keywords manip
#' @export
sampleRasterStackByMU <- function(mu,
                                  mu.set,
                                  mu.col,
                                  raster.list,
                                  pts.per.acre,
                                  p = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
                                  iterations = 20,
                                  progress = TRUE,
                                  estimateEffectiveSampleSize = TRUE,
                                  polygon.id = "pID") {
    
  
  # sanity check: package requirements
  if (!requireNamespace('terra'))
    stop('please install the terra package', call. = FALSE)
  
  v.mu <- {if (inherits(mu, 'SpatVector')) mu else terra::vect(mu)}
  
  # enforce projected CRS
  if (terra::is.lonlat(v.mu))
    stop('map unit polygons must be in a projected CRS', call. = FALSE)
  
  # check polygon ID column, create if missing
  stopifnot(length(polygon.id) == 1)
  stopifnot(is.character(polygon.id))
  
  if (!polygon.id %in% colnames(v.mu)) {
    v.mu[[polygon.id]] <- 1:nrow(v.mu)
    message("created unique polygon ID `", polygon.id, "`")
  }
  
  # check for invalid geometry
  validity.res <- data.frame(
      id = v.mu[[mu.col]],
      Polygon.Validity = terra::is.valid(v.mu),
      stringsAsFactors = FALSE
    )
  
  # init containers for intermediate results
  l.mu <- list() # samples
  l.unsampled <- list() # un-sampled polygon IDs
  a.mu <- list() # area stats
  
  # load pointers to raster data
  raster.list <- rapply(raster.list, how = 'replace', f = function(i) {
    i <- try(terra::rast(i))
    if (inherits(i, 'try-error')) {
      stop(paste0('Cannot find raster file: ', i), call. = FALSE)
    } else return(i)
  })
  
  ##
  ## iterate over rasters and read into memory if possible
  ##
  message('Loading raster data...')
  
  # these are the names assigned to each raster layer
  nm <- unlist(lapply(raster.list, names))
  
  ## TODO: not sure how to get a progress bar for this...
  # https://cran.r-project.org/web/packages/pbapply/index.html
  
  ##s
  ## check that raster extent completely covers mu extent
  ##
  
  # get MU extent, in original CRS
  e.mu <- terra::as.polygons(terra::ext(v.mu), crs = terra::crs(v.mu))
  
  message('Checking raster/MU extents...')
  
  # recursively iterate over sets of raster data, checking extents
  raster.containment.test <- rapply(raster.list, f = function(r) {
    # get current raster extent in original CRS
    e.r <- terra::as.polygons(terra::ext(r), crs = terra::crs(r))

    # transform MU extent to CRS of current raster
    e.mu.r <- terra::project(e.mu, terra::crs(e.r))

    # check for complete containment of MU by current raster
    return(terra::relate(e.r, e.mu.r, "contains")[1])
  })
  
  if (any(!raster.containment.test))
    warning('Raster extent does not completly cover MU extent')
  
  ## Moran's I by raster
  # result is a data.frame based on sampling of the raster sources, after cropping to mu extent
  if (estimateEffectiveSampleSize) {
    message('Estimating effective sample size...')
    # note: this only makes sense for "continuous" type raster data sources
    MI <- ldply(raster.list$continuous, Moran_I_ByRaster, mu.extent = e.mu, .progress = ifelse(progress, 'text', NULL))
    names(MI) <- c('Variable', 'Moran.I')
  } else {
    # when Moran's I is 0, n_effective = n
    MI <- data.frame(Variable = nm, Moran.I = 0)
  }
  
  ##
  ## iterate over map units, sample, extract raster values
  ##
  message('Sampling polygons, and extracting raster values...')
  
  # progress bar
  if (progress)
    pb <- txtProgressBar(min = 0, max = length(mu.set), style = 3)
  
  for (mu.i in mu.set) {
    # get current polygons
    mu.i.sp <- mu[which(mu[[mu.col]] == mu.i), ]
    
    ## messages are issued when it isn't possible to place the requested num. samples in a polygon
    # sample each polygon at a constant density
    suppressMessages({
      s <- constantDensitySampling(
          mu.i.sp,
          n.pts.per.ac = pts.per.acre,
          min.samples = 1,
          polygon.id = polygon.id,
          iterations = iterations
        )
    })
    
    # keep track of un-sampled polygons
    l.unsampled[[mu.i]] <- setdiff(mu.i.sp$pID, unique(s$pID))
    
    ## ADDED 2016-08-27 to account for very small polygons
    ## see related fix in constantDensitySampling
    # if there are no samples, skip stats
    if (is.null(s)) {
      # save and continue
      a.mu[[mu.i]] <- NULL
      l.mu[[mu.i]] <- NULL
      
      if (progress)
        setTxtProgressBar(pb, match(mu.i, mu.set))
      # break out of current iteration, move to next map unit
      next
      
    } else {
      # make a unique sample ID, need this for conversion: long -> wide
      s$sid <- 1:nrow(s)
      
      # extract raster data, sample ID, polygon ID to DF
      l.mu[[mu.i]] <- rapply(raster.list, how = 'replace', f = function(r) {
        cbind(value = terra::extract(r, terra::project(s, r))[[2]], data.frame(pID = s$pID, sid = s$sid))
      })
      
      # extract polygon areas as acres
      a <- terra::expanse(mu.i.sp) * 2.47e-4
      
      # compute additional summaries
      .quantiles <- quantile(a, probs = p)
      .total.area <- sum(a)
      .samples <- nrow(s)
      .mean.sample.density <- round(.samples / .total.area, 2)
      .polygons <- length(a)
      .unsampled.polygons <- length(l.unsampled[[mu.i]])
      
      # compile into single row
      a.stats <- c(round(c(.quantiles, .total.area, .samples, .polygons, .unsampled.polygons)), .mean.sample.density)
      
      # column names
      c.names <- paste0("Q", round(p*100))
      c.names[c.names == "Q0"] <- "Min"
      c.names[c.names == "Q50"] <- "Median"
      c.names[c.names == "Q100"] <- "Max"
      
      # fix name
      names(a.stats) <- c(c.names, 'Total Area', 'Samples', 'Polygons', 'Polygons Not Sampled', 'Mean Sample Dens.')
      
      # save stats to list indexed by map unit ID
      a.mu[[mu.i]] <- a.stats
      
      if (progress)
        setTxtProgressBar(pb, match(mu.i, mu.set))
    }
    
  } # done iterating over map units
  
  if (progress)
    close(pb)
  
  # assemble MU area stats
  mu.area <- ldply(a.mu)
  colnames(mu.area)[1] <- mu.col
  
  # iterate over map unit collections of samples
  d.mu <- lapply(l.mu, function(i) {
    # extract each variable type
    # fix names
    # remove NA
    .processVars <- function(j) {
      # extract pieces
      d <- ldply(j)
      # check to make sure there are some rows to process
      if (nrow(d) > 0) {
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
  colnames(d.mu)[1] <- mu.col
  
  ## unspool polygon sample IDs when no samples were collected
  unsampled.idx <- unlist(l.unsampled)
  
  # get raster summary
  rs <- rapply(raster.list, f = terra::sources, how = 'unlist')
  rs <- gsub('\\\\', '/', rs)
  
  rs.df <- data.frame(Variable = nm, File = rs, 
                      Resolution = as.character(rapply(raster.list, function(x) signif(terra::res(x)[1], 2), how = 'unlist')), 
                      inMemory = as.character(rapply(raster.list, f = terra::inMemory, how = 'unlist')), 
                      ContainsMU = raster.containment.test)
  
  # join-in Moran's I
  rs.df <- merge(rs.df, MI, by = 'Variable', all.x = TRUE, sort = FALSE)
  
  # replace missing Moran's I with 0
  # this should only affect categorical / circular variables
  rs.df$Moran.I[is.na(rs.df$Moran.I)] <- 0
  
  
  # combine into single object and result
  return(list(
      'raster.samples' = d.mu,
      'area.stats' = mu.area,
      'unsampled.ids' = unsampled.idx,
      'raster.summary' = rs.df,
      'mu.validity.check' = validity.res,
      'Moran_I' = MI
    )
  )
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



