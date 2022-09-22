joinAdjacency <- function(x, vars=c('l_musym', 'r_musym')) {
  
  if (!requireNamespace("igraph")) {
    stop("package 'igraph' is required to calculate join adjacency matrix", call. = FALSE)
  }
  
  # extract "left" and "right" map unit symbols, removing missing values
  d <- slot(x, 'data')[, vars]
  edge.list <- as.matrix(na.omit(d))
  
  # init igraph object: note that there will be many duplicate edges
  g <- igraph::graph.edgelist(edge.list, directed = FALSE)
  
  # keep track of duplicate edges as weight
  # adding 1 to the count of multiples is critical here as some nodes may only touch 1 time
  # ---> log(1) = 0
  igraph::E(g)$weight <- 1 + log(igraph::count.multiple(g))
  
  # remove multiple, keeping mean of edge weights
  g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = 'mean')
  
  # save as weighted adjacancy matrix for plotting with sharpshootR functions
  a <- igraph::get.adjacency(g, attr = 'weight')
  
  return(a)
}

