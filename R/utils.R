# iteratively apply hydrologic ordering, 
# until exact ordering of dendrogram is achieve (.hydScore == 1)
# or max iterations
# x: geomorphic proportion matrix
# maxIter: max number of perturbations of geomorphic probability matrix
.iterateHydOrder <- function(x, .maxIter = 100, ...) {
  
  .hydScore <- 0.5
  .iter <- 1
  
  # save intermediate steps here, in case we hit .maxIter
  .log <- list()
  
  # target minimum score
  while(.hydScore < 1) {
    .res <- .hydOrderRotate(x, ...)
    
    # update local vars
    .hydScore <- .res$score
    .clust <- .res$clust
    
    # save iterations, in case we have to select the best out of max iterations
    .log[[.iter]] <- .res
    
    # failsafe: too many iterations
    if(.iter > .maxIter) {
      
      # select the configuration with best score
      .highScore <- which.max(sapply(.log, '[[', 'score'))
      .hydScore <- .log[[.highScore]]$score
      .clust <- .log[[.highScore]]$clust
      
      break
    }
    
    # increment counter
    .iter <- .iter + 1
  }
  
  # most important result is the rotated hclust object
  # also include final score, number of iterations
  .res <- list(
    clust = .clust, 
    score = .hydScore,
    niter = .iter
  ) 
  
  return(.res)
}


# x: geomorphic proportion matrix
# g: geomorphic type
# j.amount: amount of jittering applied to problematic proportions
.hydOrderRotate <- function(x, g, j.amount = 0.01) {
  
  # local vars
  x.prop <- x[, -1]
  n.series <- nrow(x)
  
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
  
  # re-order labels based on sorting of proportions: "hydrologic" ordering
  hyd.order <- order(
    rowSums(
      sweep(x.prop, 2, STATS = g.stats, FUN = '*')
    ), 
    decreasing = TRUE
  )
  
  # evaluate number of unique values by column
  unique.prop.n <- apply(x.prop, 2, function(i) length(unique(i)))
  
  # flag those columns that have fewer unique values than half number of series
  problem.idx <- which(unique.prop.n < n.series / 2)
  
  # if there are too few unique values
  # add some noise to flagged columns
  if(length(problem.idx) > 0) {
    
    # add noise to affected columns
    for(i in problem.idx) {
      x.prop[, i] <- jitter(unlist(x.prop[, i]), amount = j.amount)
    }
  }
  
  
  # cluster proportions: results are not in "hydrologic" order, but close
  # force interpretation as interval-scale
  x.d <- as.hclust(
    diana(
      daisy(x.prop, type = list(numeric = 1:ncol(x.prop)))
    )
  )
  
  # attempt rotate clustering according to hydrologic ordering
  # perfect ordering not possible when there are many ties
  x.d.hydro <- dendextend::rotate(x.d, order = x$series[hyd.order])
  
  ## TODO: consider using ape pkg
  # x.d.hydro <- as.hclust(ape::rotateConstr(as.phylo(x.d), constraint = x$series[hyd.order]))
  
  # ordering objective function
  score <- length(
    which(
      x$series[hyd.order] == x$series[x.d.hydro$order]
    )
  ) / n.series
  
  .res <- list(
    clust = x.d.hydro,
    score = score
  )
  
  return(.res)
}

