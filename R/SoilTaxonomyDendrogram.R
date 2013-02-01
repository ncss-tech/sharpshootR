
# this function only works when clustering Soil Taxonomy elements
# ideally sourced from fetchOSD()
SoilTaxonomyDendrogram <- function(spc, name='hzname', max.depth=150, n.depth.ticks=6, scaling.factor=0.015, cex.names=0.75, cex.id=0.75, axis.line.offset=-4, width=0.1, y.offset=0.5, cex.taxon.labels=0.66) {
	
	# convert relevant columns into factors
	spc$soilorder <- factor(spc$soilorder)
	spc$suborder <- factor(spc$suborder)
	spc$greatgroup <- factor(spc$greatgroup)
	spc$subgroup <- factor(spc$subgroup)
	
	# cluster using divisive algorithm
	s <- site(spc)
	s.dist <- daisy(s[, c('soilorder', 'suborder', 'greatgroup', 'subgroup')], metric='gower')
	s.hclust <- as.hclust(diana(s.dist))
	
	# add series name as labels
	s.hclust$labels <- s[[idname(spc)]]
	
	# convert to phylo class
	dend <- as.phylo(s.hclust)
	
	# extract dendrogram geometry
	edge.length.range <- range(dend$edge.length)
	
	# setup plot and add dendrogram
	par(mar=c(0,0,0,0))
	plot(dend, cex=0.8, direction='up', y.lim=c(4,0), x.lim=c(0.5, length(spc)+1), show.tip.label=FALSE)
	
	# get the last plot geometry
	lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
	
	# vector of indices for plotting soil profiles below leaves of dendrogram
	new_order <- sapply(1:lastPP$Ntip, function(i) which(as.integer(lastPP$xx[1:lastPP$Ntip]) == i))
	
	# plot the profiles, in the ordering defined by the dendrogram
	# with a couple fudge factors to make them fit
	plot(spc, name=name, plot.order=new_order, max.depth=max.depth, n.depth.ticks=n.depth.ticks, scaling.factor=scaling.factor, cex.names=cex.names, cex.id=cex.id, axis.line.offset=axis.line.offset, width=width, y.offset=max(lastPP$yy) + y.offset, add=TRUE)
	
	# generate taxonomic labels and their positions under the dendrogram
	lab <- s[new_order, 'subgroup']
	unique.lab <- unique(lab)
	group.lengths <- rle(as.numeric(lab))$lengths
	lab.x.positions <- (cumsum(group.lengths) - (group.lengths / 2)) + 0.5
	lab.y.positions <- rep(c(0.6, 0.72), length.out=length(unique.lab))
	
	# add labels-- note manual tweaking of y-coordinates
	text(lab.x.positions, lab.y.positions, unique.lab, cex=cex.taxon.labels, adj=0.5, font=3)
}
