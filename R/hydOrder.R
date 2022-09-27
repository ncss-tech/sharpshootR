# x: geomorphic proportion matrix
# g: geomorphic type
# j.amount: amount of jittering applied to problematic proportions 0.05 is about right


#' @title Apply Hydrologic Ordering of a Geomorphic Proportion Matrix
#'
#' @param x x `data.frame`, geomorphic proportion matrix, as created by `soilDB::fetchOSD(..., extended=TRUE)`
#' @param g character, name of geomorphic summary table, one of: `c('geomcomp', 'hillpos', 'flats', 'terrace', 'mtnpos', 'shape')`
#' @param clust logical, perform clustering of geomorphic proportion matrix
#' @param j.amount amount of noise applied to rows having a duplicate proportion vector, passed to `jitter()`
#'
#' @author D.E. Beaudette
#'
#' @return when `clust = FALSE` a vector of series names, in hydrologic ordering, otherwise a `list` with the following elements:
#'    * `clust`: rotated `hclust` object
#'    * `hyd.order`: vector of series names, in hydrologic ordering
#'    * `clust.hyd.order`: vector of series names, after clustering + rotation, approximate hydrologic ordering
#'    * `match.rate`: fraction of series matching target hydrologic ordering, after clustering + rotation
#'    * `obj`: objective function value (sum of squared rank differences), used by [`iterateHydOrder()`]
#'    
#' @export
#'
hydOrder <- function(x, g, clust = TRUE, j.amount = 0) {
  
  # sanity checks
  stopifnot(g %in% c('geomcomp', 'hillpos', 'flats', 'terrace', 'mtnpos', 'shape'))
  stopifnot(inherits(x, 'data.frame'))
  
  # scoring used to create hydrologic ordering
  g.stats <- switch(
    g,
    'geomcomp' = c(4, 2, 1, 1, -2, -4),
    'hillpos'  = c(-4, -2, 0.1, 2, 4),
    'flats'    = c(-2, 0, 0, 2),
    'terrace'  = c(-1, 1),
    'mtnpos'   = c(4, 2, 1, 1, -2, -4),
    'shape'    = c(-4, 1, 4, 5, 6)
  )
  
  # series name is always in the first column
  # there may be extra columns `n` and `shannon_entropy`, these will be ignored
  
  
  # local vars
  n.prop <- length(g.stats)
  n.series <- nrow(x)
  
  # just geomorphic proportions
  x.prop <- x[, 2:(n.prop + 1)]
  row.names(x.prop) <- x$series
  
  # compute hydrologic ordering scores: geomorphic proportions * score vector
  # ranking this vector will always produce a reasonable ordering
  # exact rotation of a dendrogram is not always possible
  .rs <- rowSums(
    sweep(x.prop, 2, STATS = g.stats, FUN = '*')
  )
  
  # create ordering index for labels by ordering hydrologic ordering scores
  hyd.order <- order(.rs, decreasing = TRUE)
  
  # optionally perform clustering and rotation of dendrogram
  if(clust) {
    # "ties" in the proportion matrix will result in problematic dendrograms which cannot be 
    # rotated as close to hyd.order is would otherwise be possible
    
    # find approximate ties at proportion threshold of 0.001
    .hash <- apply(round(x.prop, 3), 1, digest)
    .tab <- table(.hash)
    .nm <- names(which(.tab > 1))
    
    # flag those series (rows) 
    # that have ties in the proportion matrix
    problem.idx <- which(.hash %in% .nm)
    
    # if there are too few unique values
    # add some noise to flagged rows
    if(length(problem.idx) > 0) {
      
      # add noise to affected rows
      for(i in problem.idx) {
        x.prop[i, ] <- jitter(unlist(x.prop[i, ]), amount = j.amount)
      }
    }
    
    
    # cluster proportions
    # results will be close to "hydrologic" order
    # force interpretation as interval-scale
    x.d <- as.hclust(
      diana(
        daisy(x.prop, type = list(numeric = 1:n.prop))
      )
    )
    
    # attempt rotate clustering according to hydrologic ordering
    # perfect ordering not possible when there are many ties
    x.d.hydro <- dendextend::rotate(x.d, order = x$series[hyd.order])
    
    ## TODO: consider using ape pkg since we are already importing from it
    # x.d.hydro <- as.hclust(ape::rotateConstr(as.phylo(x.d), constraint = x$series[hyd.order]))
    
    # number of exact matches
    match.rate <- length(
      which(
        x$series[hyd.order] == x$series[x.d.hydro$order]
      )
    ) / n.series
    
    # objective function: sum of squared rank distances
    # this is used by iterateHydOrder() when maxIter is exceeded
    obj <- sum((hyd.order - x.d.hydro$order)^2)
    
    
    # clustering + stats on rotation
    .res <- list(
      clust = x.d.hydro,
      hyd.order = x$series[hyd.order],
      clust.hyd.order = x$series[x.d.hydro$order],
      match.rate = match.rate,
      obj = obj
    )
  } else {
    
    # no clustering, just vector of series names in hydrologic ordering
    .res <- x$series[hyd.order]
  }
  
  
  return(.res)
}
