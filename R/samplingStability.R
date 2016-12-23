## TODO: parallel implementation

# Stability is defined as the width of the 5th-95th percentile range, over n.reps replications of median estimates associated with sampling events. The resulting width is scaled by the population median and returned as a fraction.


# m: map unit polygons, must have polygon ID, must be in CRS with units of meters
# r: raster
# n.set: set of sampling denisty values to try
# n.reps: number of replications
# p.id: polygon ID column name

# result: data.frame with median stability values as percentage of population median, range:{0,1}
samplingStability <- function(mu, r, n.set=c(0.01, 0.1, 0.5, 1, 2), n.reps=10, p.id='pID') {
  
  # sample / raster overlay
  .sample <- function(mu, r, n, p.id) {
    s <- constantDensitySampling(mu, n.pts.per.ac=n, min.samples=1, polygon.id=p.id)
    e <- as.vector(extract(r, s))
    return(e)
  }
  
  # polygon / raster overlay
  .population <- function(mu, r) {
    # result is a list
    e <- extract(r, mu)
    e <- unlist(e)
    return(e)
  }
  
  # get the "population median"
  pop.median <- median(.population(mu, r), na.rm = TRUE)
  
  # iterate over sampling densities in our example
  r.sample <- list()
  for(i in n.set) {
    # estimate median value n.reps times via sampling
    r.sample[[as.character(i)]] <- replicate(n.reps, median(.sample(mu, r, i, p.id), na.rm = TRUE))
  }
  
  # add sampling units
  names(r.sample) <- paste0(names(r.sample), ' pts/ac')
  
  # compute 5th--95th pctile interval across all samples
  res <- ldply(r.sample, function(i) {
    # interval
    qq <- quantile(i, probs=c(0.05, 0.95))
    qq <- abs(diff(qq))
    # scale interval size relative to population median
    return(qq / pop.median)
  })
  
  # fix names
  names(res) <- c('sampling.density', 'stability')
  
  return(res)
}




