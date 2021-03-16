# https://github.com/ncss-tech/sharpshootR/issues/7

## NOTE: dendrogram representation of community structure is only possible with some community detection algorithms

## TODO: investigate some heuristics for layout algorithm selection:
# layout_with_fr works most of the time
# layout_with_lgl works for most large graphs, but not when there are many disconnected sub-graphs
# layout_with_graphopt works but requires tinkering with parameters

# normalize transparency logic and argument names: should all be "alpha"

.maximum.spanning.tree <- function(x){
  # convert cost representation of weights to "strength"
  E(x)$weight <- -1 * E(x)$weight
  # compute min spanning tree on "strength"
  x <- minimum.spanning.tree(x)
  # convert "strength" representation of weights back to cost
  E(x)$weight <- -1 * E(x)$weight
  return(x)
}

# dendrogram representation relies on ape plotting functions
# ... are passed onto plot.igraph or plot.phylo




#' @title Plot a component relation graph
#'
#' @description Plot a component relation graph based on an adjacency or similarity matrix.
#'
#' @param m adjacency matrix
#' @param s central component; an empty character string is interpreted as no central component
#' @param plot.style plot style ('network', or 'dendrogram'), or 'none' for no graphical output
#' @param graph.mode interpretation of adjacency matrix: 'upper' or 'directed', see details
#' @param spanning.tree plot the minimum or maximum spanning tree ('min', 'max'), or, max spanning tree plus edges with weight greater than the n-th quantile specified in `spanning.tree`. See details and examples.
#' @param del.edges optionally delete edges with weights less than the specified quantile (0-1)
#' @param vertex.scaling.method 'degree' (default) or 'distance', see details
#' @param vertex.scaling.factor scaling factor applied to vertex size
#' @param edge.scaling.factor optional scaling factor applied to edge width
#' @param vertex.alpha optional transparency setting for vertices (0-1)
#' @param edge.transparency optional transparency setting for edges (0-1)
#' @param edge.col edge color, applied to all edges
#' @param edge.highlight.col edge color applied to all edges connecting to component named in `s`
#' @param g.layout an igraph layout function, defaults to `layout_with_fr`
#' @param vertex.label.color vertex label color
#' @param delete.singletons optionally delete vertices with no edges (`degree == 0`)
#' @param ... further arguments passed to plotting function
#' 
#' @note This function is a work in progress, ideas welcome.
#' 
#' @details Vertex size is based on a normalized index of connectivity: 
#' 
#'   - "degree" size = `sqrt(degree(g)/max(degree(g))) * scaling.factor`
#'   - "distance" size =  `sqrt(distance(V->s)/max(distance(V->s)))  * scaling.factor`, where distance(V->s) is the distance from all nodes to the named series, \code{s}. 
#'   
#' 
#' Edge width can be optionally scaled by edge weight by specifying an \code{edge.scaling.factor} value. The maximum spanning tree represents a sub-graph where the sum of edge weights are maximized. The minimum spanning tree represents a sub-graph where the sum of edge weights are minimized. The maximum spanning tree is likely a more useful simplification of the full graph, in which only the strongest relationships (e.g. most common co-occurrences) are preserved.
#' 
#' The maximum spanning tree + edges with weights > n-th quantile is an experimental hybrid. The 'backbone' of the graph is created by the maximum spanning tree, and augmented by 'strong' auxiliary edges--defined by a value between 0 and 1.
#' 
#' The \code{graph.mode} argument is passed to \code{igraph::graph_from_adjacency_matrix()} and determines how vertex relationships are coded in the adjacency matrix \code{m}. Typically, the default value of 'upper' (the upper triangle of \code{m} contains adjacency information) is the desired mode. If \code{m} contains directional information, set \code{graph.mode} to 'directed'. This has the side-effect of altering the default community detection algorithm from \code{igraph::cluster_fast_greedy} to \code{igraph::cluster_walktrap}.
#'
#' @return an igraph `graph` object is invisibly returned
#' 
#' @author D.E. Beaudette
#' 
#' @keywords hplot
#' 
#' @export
#'
#' @examples
#' 
#' # load sample data set
#' data(amador)
#' 
#' # create weighted adjacency matrix (see ?component.adj.matrix for details)
#' m <- component.adj.matrix(amador)
#' 
#' # plot network diagram, with Amador soil highlighted
#' plotSoilRelationGraph(m, s='amador')
#' 
#' # dendrogram representation
#' plotSoilRelationGraph(m, s='amador', plot.style='dendrogram')
#' 
#' # compare methods
#' m.o <- component.adj.matrix(amador, method='occurrence')
#' 
#' par(mfcol=c(1,2))
#' plotSoilRelationGraph(m, s='amador', plot.style='dendrogram')
#' title('community matrix')
#' plotSoilRelationGraph(m.o, s='amador', plot.style='dendrogram')
#' title('occurence')
#' 
#' # investigate max spanning tree
#' plotSoilRelationGraph(m, spanning.tree='max')
#' 
#' # investigate max spanning tree + edges with weights > 75-th pctile
#' plotSoilRelationGraph(m, spanning.tree=0.75)
#' 
#' \donttest{
#'   
#'   if(requireNamespace("curl") &
#'      curl::has_internet() &
#'      require(soilDB)) {
#'     
#'     # get similar data from soilweb, for the Pardee series
#'     s <- 'pardee'
#'     d <- siblings(s, component.data = TRUE)
#'     
#'     # normalize component names
#'     d$sib.data$compname <- tolower(d$sib.data$compname)
#'     
#'     # keep only major components
#'     d$sib.data <- subset(d$sib.data, subset=compkind == 'Series')
#'     
#'     # build adj. matrix and plot
#'     m <- component.adj.matrix(d$sib.data)
#'     plotSoilRelationGraph(m, s=s, plot.style='dendrogram')
#'     
#'     # alter plotting style, see ?plot.phylo
#'     plotSoilRelationGraph(m, s=s, plot.style='dendrogram', type='fan')
#'     plotSoilRelationGraph(m, s=s, plot.style='dendrogram', type='unrooted', use.edge.length=FALSE) 
#'     
#'   }
#'   
#'   
#' }
#' 
plotSoilRelationGraph <- function(m, s='', plot.style = c('network', 'dendrogram'), graph.mode='upper', spanning.tree=NULL, del.edges=NULL, vertex.scaling.method='degree', vertex.scaling.factor=2, edge.scaling.factor=1, vertex.alpha=0.65, edge.transparency=1, edge.col=grey(0.5), edge.highlight.col='royalblue', g.layout=layout_with_fr, vertex.label.color='black', delete.singletons = FALSE, ...) {
	
  # dumb hack to make R CMD check happy
  weight <- NULL
  name <- NULL
  
  # argument sanity
  plot.style <- match.arg(plot.style)
  
	# generate graph
	g <- graph.adjacency(adjmatrix = m, mode=graph.mode, weighted=TRUE)
	
	## this might have to happen later on, after edge deletion
	# optionally delete singleton vertices
	if(delete.singletons) {
	  g <- delete.vertices(g, degree(g) == 0) 
	}
	
  
	### TODO ###
	## figure out some heuristics for selecting a method: layout_with_fr is almost always the best one
	
	# when there are many clusters, layout_with_lgl doesn't work properly
	# switch back to layout_with_fr when > 5
	# g.n.clusters <-  clusters(g)$no
	
	# maybe this can be used as a heuristic as well:
	# betweenness(g)
	
# 	# select layout if not provided
# 	if(missing(g.layout)) {
# 	  if(dim(m)[1] > 20 & g.n.clusters < 5) {
# 	    g.layout <- layout_with_lgl
# 	    message('layout: Large Graph Layout algorithm')
# 	  }
# 	  
# 	  else {
# 	    g.layout <- layout_with_fr
# 	    message('layout: Fruchterman-Reingold algorithm')
# 	  }
# 	  
# 	}
	### TODO ###
	
	weight <- E(g)$weight
	
  # optionally prune weak edges less than threshold quantile
  if(!is.null(del.edges))
	  g <- delete.edges(g, E(g) [ which(weight < quantile(weight, del.edges, na.rm = TRUE)) ])
	
	# optionally compute min/max spanning tree
  if(! is.null(spanning.tree)) {
    # min spanning tree: not clear how this is useful
    if(spanning.tree == 'min'){
      g <- minimum.spanning.tree(g)
    }
    
    # max spanning tree: useful when loading an entire region
    if(spanning.tree == 'max'){
      g <- .maximum.spanning.tree(g)
    }
    
    ## TODO: this needs more testing
    # max spanning tree + edges with weights > n-tile added back
    if(is.numeric(spanning.tree)){
      # select edges and store weights
      es <- E(g) [ which(weight > quantile(weight, spanning.tree, na.rm = TRUE)) ]
      es.w <- es$weight
      # trap ovious errors
      if(length(es.w) < 1)
        stop('no edges selected, use a smaller value for `spanning.tree`')
      
      # convert edge sequence to matrix of vertex ids
      e <- get.edges(g, es)
      # compute max spanning tree
      g.m <- .maximum.spanning.tree(g)
      
      ## TODO: there must be a better way
      # add edges and weight back one at a time
      for(i in 1:nrow(e)){
        g.m <- add.edges(g.m, e[i, ], weight=es.w[i])
      }
      # remove duplicate edges
      g <- simplify(g.m)
    }
  }
  
	# transfer names
	V(g)$label <- V(g)$name 
  
	## adjust size of vertex based on some measure of connectivity
	
	# use vertex distance
	# interpretation is intuitive, but will only work when:
	# 's' is a valid series name in 'm'
	# there is no possibility of disconnected vertices (e.g deletion of edges)
  if(vertex.scaling.method == 'distance' & s != '' & is.null(del.edges)) {
    # use the distance from named series
    vertexSize <- igraph::distances(g, v=V(g)[name==s], to=V(g))
    # note that the distance from 's' -> 's' is 0, so we replace with 1
    vertexSize <- c(1.5 * max(vertexSize, na.rm=TRUE), vertexSize[-1])
    V(g)$size <- sqrt(vertexSize/max(vertexSize)) * 10 * vertex.scaling.factor
  } else {
    # scale vertex by degree (number of connections)
    vertexSize <- degree(g)
    V(g)$size <- sqrt(vertexSize/max(vertexSize)) * 10 * vertex.scaling.factor
    
  }
	
	
  
  # optionally adjust edge width based on weight
  if(!missing(edge.scaling.factor))
    E(g)$width <- sqrt(E(g)$weight) * edge.scaling.factor
  
	## extract communities
	# the fast-greedy algorithm is fast, but dosn't work with directed graphs
	if(graph.mode == 'directed')
	  g.com <- cluster_walktrap(g) ## this works OK with directed graphs
	else
	  g.com <- cluster_fast_greedy(g) ## this can crash with some networks
	
	# community metrics
	g.com.length <- length(g.com)
	g.com.membership <- membership(g.com)
  
	# save membership to original graph
	# this is based on vertex order
	V(g)$cluster <- g.com.membership
	
	# colors for communities: choose color palette based on number of communities
	if(g.com.length <= 9 & g.com.length > 2) 
		cols <- brewer.pal(n=g.com.length, name='Set1') 
	if(g.com.length < 3) 
		cols <- brewer.pal(n=3, name='Set1')
	if(g.com.length > 9)
		cols <- colorRampPalette(brewer.pal(n=9, name='Set1'))(g.com.length)
	
	# set colors based on community membership
	cols.alpha <- alpha(cols, vertex.alpha)
	V(g)$color <- cols.alpha[g.com.membership]
  
  # get an index to edges associated with series specified in 's'
  el <- get.edgelist(g)
	idx <- unique(c(which(el[, 1] == s), which(el[, 2] == s)))
	
	# set default edge color
  E(g)$color <- alpha(edge.col, edge.transparency)
	# set edge colors based on optional series name to highlight
  E(g)$color[idx] <- alpha(edge.highlight.col, edge.transparency)
  
  # previous coloring of edges based on in/out community
  # E(g)$color <- alpha(c('grey','black')[crossing(g.com, g)+1], edge.transparency)
  
	# generate vector of fonts, highlighting main soil
	font.vect <- rep(1, times=length(g.com.membership))
	font.vect[which(names(g.com.membership) == s)] <- 2
	
	if(plot.style == 'network') {
		set.seed(1010101) # consistant output
		plot(g, layout=g.layout, vertex.label.color=vertex.label.color, vertex.label.font=font.vect, ...)
		
		# invisibly return the graph
		return(invisible(g))
	}
	
	if(plot.style == 'dendrogram') {
	  plot_dendrogram(g.com, mode='phylo', label.offset=0.1, font=font.vect, palette=cols, ...)
	  
	  # invisibly return the community structure
	  return(invisible(g.com))
		}
  
  
}
