# compute pair-wise weightes for soil-relation graph
.pair.wise.wts <- function(d, wt) {
  x <- d[[wt]]
  x <- x / sum(x)
  w <- outer(x, x, FUN='*')
  return(w)
}

component.adj.matrix <-function(d, mu='mukey', co='compname', wt=NULL) {
	# extract a list of component names that occur together
	l <- dlply(d, mu, function(i) unique(i[[co]]))
	
	# include only those components that occur with other components
	mu.multiple.components <- names(which(sapply(l, function(i) length(i) > 1)))
	u <- unique(d[[co]][d[[mu]] %in% mu.multiple.components])
	u <- sort(u)
	
	# empty adjacency matrix
	m <- matrix(rep(0, times=length(u)^2), nrow=length(u))
	dimnames(m) <- list(u, u)
	
	# weighted version
	if(!missing(wt)) {
		# extract a list of pair-wise weights
		l.wts <- dlply(d, mu, .pair.wise.wts, wt=wt)
		
		for(i in 1:length(l)) {
			series.vect <- l[[i]] # get current list element
			wts.mat <- l.wts[[i]]
			
			# check to make sure there are more than 1 components in this map unit
			if(length(series.vect) > 1) {
				## note that it is the _upper triangle_ that we care about
				m[series.vect, series.vect] <- m[series.vect, series.vect] + wts.mat
			}
		}
	}
	
	# un-weighted version
	else {
		for(i in 1:length(l)) {
			# get current list element
			series.vect <- na.omit(l[[i]])	
			# check to make sure there are more than 1 components in this map unit
			if(length(series.vect) > 1) {
				## increment affected elements by 1
				m[series.vect, series.vect] <- m[series.vect, series.vect] + 1			
			}
		}
	}
	
	# set diagonal and lower triangle to 0
	m[lower.tri(m)] <- 0
	diag(m) <- 0
  
	# scale
	m <- m / max(m, na.rm=TRUE)
	
	# done
	return(m)
}
