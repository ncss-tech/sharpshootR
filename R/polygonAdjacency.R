
# optional arguments are passed to poly2nb
polygonAdjacency <- function(x, v='MUSYM', ...) {
  
  # sanity check: package requirements
  if (!requireNamespace('spdep'))
    stop('please install the `spdep` package', call. = FALSE)
  
  if (!requireNamespace("igraph")) {
    stop("package 'igraph' is required to calculate polygon adjacency", call. = FALSE)
  }
  
  # compute neighbor list
  nb <- spdep::poly2nb(x, ...)
  
  # init empty list to store common attribute polygon IDs
  # list index references the original polygon number
  # each list item contains indexes to neighbors with the same map unit symbol
  common.polys <- list()
    
  # init empy list to store adjacency information via attribute `v`
  edge.list <- list()
  
  # iterate over polygons
  for (i in 1:nrow(x)) {
    # get the neighbors for polygon i
    n.idx <- nb[[i]]
    # keep track of the current polygon's attribute and its neighbors'
    this.attr <- x[[v]][i]
    this.nb <- x[[v]][n.idx]
    # get an index to those neighbors that share the same symbol
    common.symbol.idx <- which(this.attr == this.nb)
    
    # store index to polygons with the same symbol
    common.polys[[i]] <- n.idx[common.symbol.idx]
    
    # store edge list information
    # if there is no matching neighbor, then pad with NA
    if (length(this.nb) == 0)
      this.nb <- NA
    edge.list[[i]] <- cbind(this.attr, this.nb)   
  }
  
  # get a unique set of polygon indices to investigate
  polys.to.investigate <- na.omit(unique(unlist(common.polys)))
  
  # reduce edge list to single matrix of edges
  edge.list <- do.call('rbind', edge.list)
  edge.list <- na.omit(edge.list)
  
  # init igraph object: note that there will be many duplicate edges
  g <- igraph::graph.edgelist(edge.list, directed = FALSE)
  
  # keep track of duplicate edges as weight
  # adding 1 to the count of multiples is critical here as some nodes may only touch 1 time
  # ---> log(1) = 0
  igraph::E(g)$weight <- 1 + log(igraph::count.multiple(g))
  
  # remove multiple edges, taking mean of the edge weights
  g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = 'mean')
  
  # save as weighted adjacancy matrix for plotting with sharpshootR functions
  a <- igraph::get.adjacency(g, attr = 'weight')
  
  # done
  return(list(commonLines = polys.to.investigate, adjMat = a))
}
