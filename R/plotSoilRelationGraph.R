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
plotSoilRelationGraph <- function(m, s='', plot.style='network', graph.mode='upper', spanning.tree=NULL, del.edges=NULL, vertex.scaling.method='degree', vertex.scaling.factor=2, edge.scaling.factor=1, vertex.alpha=0.65, edge.transparency=1, edge.col=grey(0.5), edge.highlight.col='royalblue', g.layout=layout_with_fr, vertex.label.color='black', ...) {
	
  # dumb hack to make R CMD check happy
  weight <- NULL
  name <- NULL
  
	# generate graph
	g <- graph.adjacency(m, mode=graph.mode, weighted=TRUE)
  
	### TODO ###
	## figure out some hueristics for selecting a method: layout_with_fr is almost always the best one
	
	# when there are many clusters, layout_with_lgl doesn't work properly
	# switch back to layout_with_fr when > 5
	# g.n.clusters <-  clusters(g)$no
	
	# maybe this can be used as a hueristic as well:
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
		invisible(g)
	}
	
	if(plot.style == 'dendrogram') {
	  plot_dendrogram(g.com, mode='phylo', label.offset=0.1, font=font.vect, palette=cols, ...)
	  
	  # invisibly return the community structure
	  invisible(g.com)
		}
  
  
}
