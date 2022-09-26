
## TODO: add short-circuit for cases where a perfect match isn't possible 


#' @title Iteratively Attempt Hydrologic Ordering of Geomorphic Proportion Matrix
#' 
#' @details This function is used by the suite of geomorphic proportion visualization functions (`viz*`) to attempt rotation of a dendrogram according to "hydrologic ordering" rules. A perfect rotation is not always possible, and reported as a match rate in the returned `score` value
#'
#' @param x `data.frame` geomorphic proportion matrix, as created by `soilDB::fetchOSD(..., extended=TRUE)`
#' @param g name of geomorphic summary table, one of: `c('geomcomp', 'hillpos', 'flats', 'terrace', 'mtnpos', 'shape')`
#' @param target numeric, target match rate
#' @param maxIter integer, maximum number of perturbations of geomorphic probability matrix
#' @param j.amount numeric, amount of noise applied to rows with too few unique values, passed to `jitter()`
#' @param verbose logical, additional output printed via message
#' 
#' @author D.E. Beaudette
#'
#' @return
#' A `list` with the following elements:
#'    * `clust`: rotated `hclust` object
#'    * `score`: scoring of hydrologic ordering of dendrogram (match rate)
#'    * `niter`: number of iterations
#'    
#' @export
iterateHydOrder <- function(x, g, target = 0.9, maxIter = 20, j.amount = 0.05, verbose = FALSE) {
  
  # sanity checks
  stopifnot(g %in% c('geomcomp', 'hillpos', 'flats', 'terrace', 'mtnpos', 'shape'))
  stopifnot(inherits(x, 'data.frame'))
  
  # initial conditions
  .iter <- 1
  .ho <- hydOrder(x, g = g, j.amount = 0)
  .score <- .ho$match.rate
  .clust <- .ho$clust
  
  # save intermediate steps here, in case we hit maxIter
  .log <- list()
  
  # iterate while less than target score
  # skip if we are already there
  while(.score < target) {
    # try again with jittering
    .ho <- hydOrder(x, g = g, j.amount = j.amount)
    
    # update local vars
    .score <- .ho$match.rate
    .clust <- .ho$clust
    
    # save iterations, in case we have to select the best out of max iterations
    .log[[.iter]] <- .ho
    
    # failsafe: too many iterations
    if(.iter > maxIter) {
      
      # select the configuration with lowest objective function value
      # this is the sum of squared differences in ranks
      .best <- which.min(sapply(.log, '[[', 'obj'))
      
      # best match rate and resulting clustering
      .score <- .log[[.best]]$match.rate
      .clust <- .log[[.best]]$clust
      
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
    score = .score,
    niter = .iter
  ) 
  
  # debugging
  if(verbose) {
    message(sprintf('%s%% match rate after %s iterations', round(.score * 100, 1), maxIter))
  }
  
  return(.res)
}



