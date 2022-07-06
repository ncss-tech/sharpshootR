sampleRasterStackByMU2 <- function(mu,
                                  mu.set,
                                  mu.col,
                                  raster.list,
                                  pts.per.acre,
                                  p = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
                                  iterations = 20,
                                  progress = TRUE,
                                  estimateEffectiveSampleSize = TRUE,
                                  polygon.id = "pID", 
                                  ...) {
  
  # sanity check: package requirements
  if (!requireNamespace('terra'))
    stop('please install the terra package', call. = FALSE)
  
  if (inherits(mu, 'SpatVector')) {
    v.mu <- mu
  } else{
    v.mu <- terra::vect(mu)
  }
  
  e.mu <- terra::as.polygons(terra::ext(v.mu), crs = terra::crs(v.mu))
  
  # enforce projected CRS
  if (terra::is.lonlat(v.mu))
    stop('map unit polygons must be in a projected CRS', call. = FALSE)
  
  # check polygon ID column, create if missing
  stopifnot(length(polygon.id) == 1)
  stopifnot(is.character(polygon.id))
  
  if (!polygon.id %in% colnames(v.mu)) {
    v.mu$pID <- 1:nrow(v.mu)
    message("created unique polygon ID `pID`")
  }
  
  # check for invalid geometry
  validity.res <- data.frame(
    id = v.mu[[mu.col]],
    Polygon.Validity = terra::is.valid(v.mu),
    stringsAsFactors = FALSE
  )
  
  # load pointers to raster data
  raster.list <- rapply(raster.list, how = 'replace', f = function(i) {
    i <- try(terra::rast(i))
    if (inherits(i, 'try-error')) {
      stop(paste0('Cannot find raster file: ', i), call. = FALSE)
    } else return(i)
  })
  
  nm <- do.call('c', lapply(raster.list, names))
  rs <- rapply(raster.list, f = terra::sources, how = 'unlist')
  rs <- gsub('\\\\', '/', rs)
  
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
    MI <- ldply(raster.list$continuous, Moran_I_ByRaster, mu.extent = v.mu)
    names(MI) <- c('Variable', 'Moran.I')
  } else {
    # when Moran's I is 0, n_effective = n
    MI <- data.frame(Variable = nm, Moran.I = 0)
  }
  
  res <- rapply(raster.list, how = "list", function(r) {
    res2 <- exactextractr::exact_extract(
      r,
      sf::st_as_sf(terra::project(v.mu, r)),
      force_df = TRUE,
      include_xy = TRUE,
      include_cols = c(mu.col, polygon.id),
      progress = FALSE
    )
    res2
  })
  
  raster.samples <- do.call('rbind', lapply(c("continuous", "circular", "categorical"), function(v) {
    do.call('rbind', lapply(names(res[[v]]), function(x) {
      y <- do.call('rbind', res[[v]][[x]])
      y$variable <- x
      y$variable.type <- v
      y$sid <- 1:nrow(y)
      y
    }))
  }))
  
  rs.df <- data.frame(Variable = nm, File = rs, 
                      Resolution = as.character(rapply(raster.list, function(x) signif(terra::res(x)[1], 2), how = 'unlist')), 
                      inMemory = as.character(rapply(raster.list, f = terra::inMemory, how = 'unlist')), 
                      ContainsMU = raster.containment.test)
  
  # join-in Moran's I
  rs.df <- merge(rs.df, MI, by = 'Variable', all.x = TRUE, sort = FALSE)
  
  # replace missing Moran's I with 0
  # this should only affect categorical / circular variables
  rs.df$Moran.I[is.na(rs.df$Moran.I)] <- 0
  
  # area stats
  a.mu <- list() 

  for (mu.i in mu.set) {
    a <- terra::expanse(v.mu[v.mu[[mu.col]] == mu.i, ]) * 2.47e-4
    
    # compute additional summaries
    .quantiles <- quantile(a, probs = p)
    .total.area <- sum(a)
    .samples <- round(nrow(raster.samples[raster.samples[[mu.col]] == mu.i, ]) / length(unique(raster.samples$variable)))
    .mean.sample.density <- round(.samples / .total.area, 2)
    .polygons <- length(a)
    # .unsampled.polygons <- length(l.unsampled[[mu.i]])
    
    #   # compile into single row
    a.stats <-
      c(round(c(
        .quantiles, .total.area, .samples, .polygons
        # , .unsampled.polygons
      )), .mean.sample.density)
    
    # column names
    c.names <- paste0("Q", round(p * 100))
    c.names[c.names == "Q0"] <- "Min"
    c.names[c.names == "Q50"] <- "Median"
    c.names[c.names == "Q100"] <- "Max"
    names(a.stats) <- c(c.names, 'Total Area', 'Samples', 'Polygons'
                        # , 'Polygons Not Sampled'
                        , 'Mean Sample Dens.')

    # save stats to list indexed by map unit ID
    a.mu[[mu.i]] <- a.stats
  }
  
  mu.area <- as.data.frame(do.call('rbind', a.mu))
  mu.area <- cbind(setNames(data.frame(rownames(mu.area)), mu.col), mu.area)
  rownames(mu.area) <- NULL
  
  # combine into single object and result
  return(
    list(
      'raster.samples' = raster.samples,
      'area.stats' = mu.area,
      'unsampled.ids' = NULL,#unsampled.idx,
      'raster.summary' = rs.df,
      'mu.validity.check' = validity.res,
      'Moran_I' = MI
    )
  )
}

