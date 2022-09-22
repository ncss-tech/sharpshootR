
.iterateHydOrder <- function(x, .maxIter = 101, ...) {
  # iteratively apply hydrologic ordering, 
  # until exact ordering of dendrogram is achieve (.hydScore == 1)
  # or max iterations
  .hydScore <- 0.5
  .iter <- 1
  
  # save intermediate steps here, in case we hit .maxIter
  .log <- list()
  
  while(.hydScore < 0.95) {
    .res <- .hydOrderRotate(x, ...)
    
    # update local vars
    .hydScore <- .res$score
    .clust <- .res$clust
    
    # save iterations, in case we have to select the best out of max iterations
    .log[[.iter]] <- .res
    
    # increment counter
    .iter <- .iter + 1
    
    # failsafe: too many iterations
    if(.iter > .maxIter) {
      
      # select the configuration with best score
      .highScore <- which.max(sapply(.log, '[[', 'score'))
      .hydScore <- .log[[.highScore]]$score
      .clust <- .log[[.highScore]]$clust
      
      break
    }
  }
  
  return(list(clust = .clust, score = .hydScore))
}


.hydOrderRotate <- function(x, g, j.amount = 0.01) {
  
  # local vars
  x.prop <- x[, -1]
  n.series <- nrow(x)
  
  # scoring used to create hydrologic ordering
  g.stats <- switch(
    g,
    'geomcomp' = c(4, 2, 1, 1, -2, -4),
    'hillpos'  = c(-4, -2, 0.1, 2, 4)
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
  
  # add some noise to flagged columns
  # if there are too few unique values
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
  
  # rotate clustering according to hydrologic ordering
  x.d.hydro <- dendextend::rotate(x.d, order = x$series[hyd.order])
  
  ## TODO: consider using ape pkg
  # x.d.hydro <- as.hclust(ape::rotateConstr(as.phylo(x.d), constraint = x$series[hyd.order]))
  
  # ordering objective function
  score <- length(which(x$series[hyd.order] == x$series[x.d.hydro$order])) / n.series
  
  res <- list(
    clust = x.d.hydro,
    score = score
  )
  
  return(res)
}

