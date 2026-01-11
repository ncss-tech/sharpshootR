
## TODO: add short-circuit for cases where a perfect match isn't possible 


#' @title Iteratively Attempt Hydrologic Ordering of Geomorphic Proportion Matrix
#' 
#' @details This function is used by the suite of geomorphic proportion visualization functions (`viz*`) to attempt rotation of a dendrogram according to "hydrologic ordering" rules. A perfect rotation is not always possible, and reported as a match rate in the returned `score` value
#'
#' @param x `data.frame` geomorphic proportion matrix, as created by `soilDB::fetchOSD(..., extended=TRUE)`
#' @param g name of geomorphic summary table, one of: `c('geomcomp', 'hillpos', 'flats', 'terrace', 'mtnpos', 'shape', 'geomorphons')`
#' @param target numeric, target match rate
#' @param maxIter integer, maximum number of perturbations of geomorphic probability matrix
#' @param j.amount numeric, amount of noise applied to rows with too few unique values, passed to `jitter()`
#' @param verbose logical, additional output printed via message
#' @param trace logical, additional list of results for each iteration
#' 
#' @author D.E. Beaudette
#'
#' @return
#' A `list` with the following elements:
#'    * `clust`: rotated `hclust` object
#'    * `hyd.order`: vector of series names, in hydrologic ordering
#'    * `clust.hyd.order`: vector of series names, after clustering + rotation, approximate hydrologic ordering
#'    * `match.rate`: fraction of series matching target hydrologic ordering, after clustering + rotation
#'    * `obj`: objective function value (sum of squared rank differences), used by [`iterateHydOrder()`]
#'    * `niter`: number of iterations
#'    * `trace`: list of results by iteration, only when `trace = TRUE`
#'    
#' @export
#' 
#' @examplesIf requireNamespace("dendextend", quietly = TRUE) 
#' 
#' # example data, similar to results from soilDB::fetchOSD(..., extended = TRUE)
#' data("OSDexamples")
#' 
#' # single iteration of hydrologic ordering
#' h1 <- hydOrder(OSDexamples$hillpos, g = 'hillpos', clust = TRUE)
#' 
#' # perform several iterations, keep the best one
#' h2 <- iterateHydOrder(OSDexamples$hillpos, 'hillpos', verbose = TRUE)
#' 
#' # compare: only slightly better match rate achieved
#' h1$match.rate
#' h2$match.rate
#' 
#' # return trace log for eval of objective function
#' # increase max iterations
#' h2 <- iterateHydOrder(OSDexamples$hillpos, 'hillpos', maxIter = 100, verbose = TRUE, trace = TRUE)
#' 
#' # inspect objective function evolution
#' tr <- h2$trace
#' obj <- sapply(tr, '[[', 'obj')
#' 
#' plot(obj, type = 'b')
#' hist(obj)
#' 
#' # in this case the clustering of hillpos proportions has only two possible configurations

#' 
iterateHydOrder <- function(x, g, target = 0.9, maxIter = 20, j.amount = 0.05, verbose = FALSE, trace = FALSE) {
  
  # sanity checks
  stopifnot(g %in% c('geomcomp', 'hillpos', 'flats', 'terrace', 'mtnpos', 'shape', 'geomorphons'))
  stopifnot(inherits(x, 'data.frame'))
  
  # initial conditions
  .iter <- 1
  .ho <- hydOrder(x, g = g, j.amount = 0)
  .obj <- .ho$obj
  .hyd.order <- .ho$hyd.order
  .clust.hyd.order <- .ho$clust.hyd.order
  .match.rate <- .ho$match.rate
  .clust <- .ho$clust
  
  # save intermediate steps here, in case we hit maxIter
  .log <- list()
  
  # iterate while less than target score
  # skip if we are already there
  while(.match.rate < target) {
    
    # try again with jittering
    .ho <- hydOrder(x, g = g, j.amount = j.amount)
    
    # update local vars
    .obj <- .ho$obj
    .hyd.order <- .ho$hyd.order
    .clust.hyd.order <- .ho$clust.hyd.order
    .match.rate <- .ho$match.rate
    .clust <- .ho$clust
    
    # save iterations, in case we have to select the best out of max iterations
    .log[[.iter]] <- .ho
    
    # failsafe: too many iterations
    if(.iter >= maxIter) {
      
      # select the configuration with lowest objective function value
      # this is the sum of squared differences in ranks
      .best <- which.min(sapply(.log, '[[', 'obj'))
      
      # best match rate and resulting clustering
      .match.rate <- .log[[.best]]$match.rate
      .clust <- .log[[.best]]$clust
      
      # best ordering vectors
      .obj <- .log[[.best]]$obj
      .hyd.order <- .log[[.best]]$hyd.order
      .clust.hyd.order <- .log[[.best]]$clust.hyd.order
      
      # done
      break
    }
    
    # or, keep going
    .iter <- .iter + 1
  }
  
  # most important result is the rotated hclust object
  # also include final match rate score, number of iterations
  .res <- list(
    clust = .clust, 
    hyd.order = .hyd.order,
    clust.hyd.order = .clust.hyd.order,
    match.rate = .match.rate,
    obj = .obj,
    niter = .iter
  ) 
  
  # debugging
  if(verbose) {
    message(sprintf('%s%% match rate after %s iterations', round(.match.rate * 100, 1), .iter))
  }
  
  if(trace) {
    .res$trace <- .log
  }
  
  return(.res)
}



