


#' @title Create an adjacency matrix from a data.frame of component data
#' 
#' @description Create an adjacency matrix from SSURGO component data
#'
#' @param d `data.frame`, typically of SSURGO data
#' @param mu name of the column containing the map unit ID (typically 'mukey')
#' @param co name of the column containing the component ID (typically 'compname')
#' @param wt name of the column containing the component weight percent (typically 'comppct_r')
#' @param method one of either: `community.matrix`, or `occurrence`; see details
#' @param standardization community matrix standardization method, passed to `vegan::decostand`
#' @param metric community matrix dissimilarity metric, passed to `vegan::vegdist`
#' @param rm.orphans `logical`, should map units with a single component be omitted? (typically yes)
#' @param similarity logical, return a similarity matrix? (if `FALSE`, a distance matrix is returned)
#' @param return.comm.matrix logical, return pseudo-community matrix? (if `TRUE` no adjacency matrix is created)
#'
#' @return a similarity matrix / adjacency matrix suitable for use with `igraph` functions or anything else that can accommodate a _similarity_ matrix.
#' 
#' @author D.E. Beaudette
#' 
#' @export
#' 
#' @keywords manip
#'
#' @examples
#' 
#' # load sample data set
#' data(amador)
#' 
#' # convert into adjacency matrix
#' m <- component.adj.matrix(amador)
#' 
#' # plot network diagram, with Amador soil highlighted
#' plotSoilRelationGraph(m, s = 'amador')
#' 
component.adj.matrix <-function(d, mu='mukey', co='compname', wt='comppct_r', method = c('community.matrix', 'occurrence'), standardization='max', metric='jaccard', rm.orphans=TRUE, similarity=TRUE, return.comm.matrix=FALSE) {
	
  # appease R CMD check
  .wt <- NULL
  
  # sanity check
  method <- match.arg(method)
  
  ## hack: move wt column into '.wt'
  d$.wt <- d[[wt]]
  
  # aggregate component percentages when multiple components of the same name are present
  ## note that we are using weights as moved into the new column '.wt'
  d <- ddply(d, c(mu, co), summarise, weight=sum(.wt))
  # re-order
  d <- d[order(d[[mu]], d[[co]]), ]
  
  # extract a list of component names that occur together
  l <- dlply(d, mu, function(i) unique(i[[co]]))
  
  # optionally keep map units with only a single component
  if(rm.orphans) {
    # include only those components that occur with other components
    mu.multiple.components <- names(which(sapply(l, function(i) length(i) > 1)))
    
    # subset, keeping only those map units with > 1 component
    d <- d[which(d[[mu]] %in% mu.multiple.components), ]
  }
  
  # naive method, no weighting
  if(method == 'occurrence') {
    # get unique vector of component names and sort
    u <- unique(d[[co]])
    u <- sort(u)
    
    # empty adjacency matrix
    m <- matrix(rep(0, times=length(u)^2), nrow=length(u))
    dimnames(m) <- list(u, u)
    
    # compute co-occurances
    for(i in 1:length(l)) {
      # get current list element
      series.vect <- na.omit(l[[i]])	
      # check to make sure there are more than 1 components in this map unit
      if(length(series.vect) > 1) {
        ## increment affected elements by 1
        m[series.vect, series.vect] <- m[series.vect, series.vect] + 1			
      }
    }
    
    # set diagonal and lower triangle to 0
    m[lower.tri(m)] <- 0
    diag(m) <- 0
  }
  
  
  ### Note: when using a limited number of "exemplars" (typically map units) the resulting
  ###       adjacency matrix may contain 0's using this method
  ###       https://github.com/ncss-tech/sharpshootR/issues/4
  
  # default method, based on ideas from ecological community matrix analysis
  if(method == 'community.matrix') {
    
    # sanity checks
    if(is.null(wt))
      stop('must supply a column name containing weights')
    
    # reshape to component x mukey community matrix
    fm <- as.formula(paste(co, ' ~ ', mu, sep=''))
    d.wide <- dcast(d, fm, value.var='weight', fill=0)
    
    # convert into community matrix by moving first column -> row.names
    d.mat <- as.matrix(d.wide[, -1])
    dimnames(d.mat)[[1]] <- d.wide[[co]]
    
    # optionally return psuedo community matrix
    if(return.comm.matrix) {
      return(d.mat)
    }
    
    
    ## standardization method and distance metric MATTER
    # convert community matrix into dissimilarity matrix
    # optional standardization
    if(standardization != 'none')
      d.mat <- decostand(d.mat, method=standardization)
    # distance matrix
    m <- vegdist(d.mat, method=metric)
    
    # convert to similarity matrix: S = max(D) - D [Kaufman & Rousseeuw]
    if(similarity == TRUE) {
      m <- as.matrix(max(m) - m)
      mat.type <- 'similarity'
      
      # set diagonal and lower triangle to 0
      m[lower.tri(m)] <- 0
      diag(m) <- 0
      
      # set attributes related to this method
      attr(m, 'standardization') <- standardization
      attr(m, 'metric') <- metric
    }
        
  }
  
  # set method attribute
  attr(m, 'method') <- method
  
	# done
	return(m)
}
