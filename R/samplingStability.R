
#' @title Estimate Sampling Stability
#' 
#' @description Stability is defined as the width of the 5th-95th percentile range, over n.reps replications of median estimates associated with sampling events. The resulting width is scaled by the population median and returned as a fraction.
#' 
#' @param mu map unit polygons, must have polygon ID, must be in CRS with units of meters
#' @param r SpatRaster
#' @param n.set set of sampling density values to try
#' @param n.reps number of replications
#' @param p.id polygon ID column name
#'
#' @return data.frame with median stability values as percentage of population median, range: `[0,1]`
#' @author D.E. Beaudette
#' @export
samplingStability <- function(mu, r, n.set = c(0.01, 0.1, 0.5, 1, 2), n.reps = 10, p.id = 'pID') {
  
  # summary by quantiles 
  .summary <- function(i, p=c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)) {
    # remove NA
    v <- na.omit(i)
    # compute quantiles
    q <- quantile(v, probs = p)
    res <- data.frame(t(q))
    
    # assign reasonable names (quantiles)
    if (nrow(res) > 0) {
      names(res) <- c(paste0('Q', p * 100))
      # compute size
      res$n <- length(i)
      return(res)
    }
    else
      return(NULL)
  }
  
  
  # sample / raster overlay
  .sample <- function(mu, r, n, p.id) {
    s <- constantDensitySampling(mu, n.pts.per.ac = n, min.samples = 1, polygon.id = p.id)
    terra::extract(r, s)[[2]]
  }
  
  # polygon / raster overlay
  .population <- function(mu, r) {
    terra::extract(r, mu)[[2]]
  }
  
  # init a list to store tabular summaries
  
  # get population and median
  pop <- .population(mu, r)
  pop.median <- median(pop, na.rm = TRUE)
  
  # iterate over sampling densities in our example
  # this list will be used for two things:
  #    1. table of quantiles, number of samples, and pct of pixels
  #    2. medians extracted for stability index
  r.sample <- list()
  for (i in n.set) {
    # estimate select quantiles and sample size over replications
    # result is a list
    s.i <- replicate(n.reps, .summary(.sample(mu, r, i, p.id)), simplify = FALSE)
    # convert to DF and store
    r.sample[[as.character(i)]] <- ldply(s.i)
  }
  
  ## step 1: table of quantiles
  # add sampling units
  names(r.sample) <- paste0(names(r.sample), ' pts/ac')
  
  # create table of quantiles using mean of replications
  summary.table <- lapply(r.sample, colMeans)
  
  # add population
  summary.table[['population']] <- unlist(.summary(pop))
  
  # convert summary table to DF
  summary.table <- ldply(summary.table)
  
  # convert sample size to integer
  summary.table$n <- round(summary.table$n)
  
  # compute pixels of population sampled
  summary.table$`percent pixels sampled` <- (summary.table$n / summary.table$n[summary.table$.id == 'population']) * 100
  
  ## step 2: stability of median
  # compute 5th--95th pctile interval across all samples
  stability <- ldply(r.sample, function(i) {
    # get interval
    qq <- quantile(i[['Q50']], probs = c(0.05, 0.95))
    qq <- abs(diff(qq))
    # scale interval size relative to population median
    return(qq / pop.median)
  })
  
  # fix names
  names(stability) <- c('sampling.density', 'stability')
  names(summary.table)[1] <- 'sampling.density'
  
  return(list(summary.table = summary.table, stability = stability))
}




