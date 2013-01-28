plotSoilRelationGraph <- function(m, s, type='network') {
	
	# generate graph
	g <- graph.adjacency(m, mode='upper', weighted=TRUE)
	
	# transfer names
	V(g)$label <- V(g)$name 

	# adjust size of vertex based on degree of connectivity
	v.size <- sqrt(degree(g)) * 2

	# community metrics
	g.com <- fastgreedy.community(g)
	g.com.length <- length(g.com)
	g.com.membership <- membership(g.com)

	# colors for communities
	if(g.com.length <= 9) cols <- brewer.pal(n=g.com.length, name='Set1') else cols <- colorRampPalette(brewer.pal(n=9, name='Set1'))(g.com.length)

	cols.alpha <- alpha(cols, 0.65)
	V(g)$color <- cols.alpha[membership(g.com)]

	# generate vector of fonts, highlighting main soil
	font.vect <- rep(1, times=length(g.com.membership))
	font.vect[which(names(g.com.membership) == s)] <- 2

	# generate vector of label cex, highlighting main soil
	cex.vect <- rep(1, times=length(g.com.membership))
	cex.vect[which(names(g.com.membership) == s)] <- 1.25
	
	if(type == 'network') {
		set.seed(1010101) # consistant output
		plot(g.com, g, layout=layout.fruchterman.reingold, vertex.size=v.size, vertex.label.color='black', vertex.label.cex=cex.vect, vertex.label.font=font.vect, colbar=cols.alpha, mark.groups=NULL, edge.color=c('grey','black')[crossing(g.com, g)+1])
		}
	if(type == 'dendrogram') {
		dendPlot(g.com, label.offset=0.1, font=font.vect, col='black', cex=cex.vect, colbar=cols)
		}
	}
