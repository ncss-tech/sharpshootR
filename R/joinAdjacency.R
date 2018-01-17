joinAdjacency <- function(x, vars=c('l_musym', 'r_musym')) {
  
  # extract "left" and "right" map unit symbols, removing missing values
  d <- slot(x, 'data')[, vars]
  edge.list <- as.matrix(na.omit(d))
  
  # init igraph object: note that there will be many duplicate edges
  g <- graph.edgelist(edge.list, directed=FALSE)
  
  # keep track of duplicate edges as weight
  # adding 1 to the count of multiples is critical here as some nodes may only touch 1 time
  # ---> log(1) = 0
  E(g)$weight <- 1 + log(count.multiple(g))
  
  # remove multiple, keeping mean of edge weights
  g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb='mean')
  
  # save as weighted adjacancy matrix for plotting with sharpshootR functions
  a <- get.adjacency(g, attr='weight')
  
  return(a)
}

