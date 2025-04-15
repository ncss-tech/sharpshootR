#' @title Join Document Adjacency
#' 
#' @description Convert a set of line segment "join decisions" into a weighted adjacency matrix describing which map unit symbols touch.
#'
#' @param x `data.frame` or similar object, each row represents a single shared edge (typically `sf` LINESTRING feature)
#' @param vars a vector of two characters naming columns containing "left", and "right" map unit symbols
#'
#' @return A weighted adjacency matrix is returned, suitable for plotting directly with `plotSoilRelationGraph()`.
#' 
#' @author D.E. Beaudette
#' 
#' @seealso [plotSoilRelationGraph()]
#' 
#' @export
#'
joinAdjacency <- function(x, vars = c('l_musym', 'r_musym')) {
  
  if (!requireNamespace("igraph")) {
    stop("package 'igraph' is required to calculate join adjacency matrix", call. = FALSE)
  }
  
  # extract "left" and "right" map unit symbols, removing missing values
  d <- x[, vars]
  edge.list <- as.matrix(na.omit(d))
  
  # init igraph object: note that there will be many duplicate edges
  g <- igraph::graph.edgelist(edge.list, directed = FALSE)
  
  # keep track of duplicate edges as weight
  # adding 1 to the count of multiples is critical here as some nodes may only touch 1 time
  # ---> log(1) = 0
  igraph::E(g)$weight <- 1 + log(igraph::count.multiple(g))
  
  # remove multiple, keeping mean of edge weights
  g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = 'mean')
  
  # save as weighted adjacency matrix for plotting with sharpshootR functions
  a <- igraph::get.adjacency(g, attr = 'weight')
  
  return(a)
}

