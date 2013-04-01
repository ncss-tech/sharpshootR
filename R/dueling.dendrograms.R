# visually compare two related dendrograms
# d.1: left-hand dissimilarity matrix, rescaled to [0,1]
# d.2: right-hand dissimilarity matrix, rescaled to [0,1]
# lab.1: left-hand title
# lab.2: right-hand title
dueling.dendrograms <- function(d.1, d.2, lab.1='D1', lab.2='D2') {
	
	# cluster via divisive hierarchical algorithm
	# convert to 'phylo' class
	phylo.d1 <- as.phylo(as.hclust(diana(d.1)))
	phylo.d2 <- as.phylo(as.hclust(diana(d.2)))

	# setup some geometry for connecting lines / arrows
	arrow.left <- 0.1
	arrow.right <- 0.9
	arrow.length <- 0.1

	# setup plot layout
	lo <- layout(matrix(c(1,3,2), ncol=3), widths=c(1, 1, 1)) # check with: layout.show(lo)

	# plot hz-level dissimilarities on left-hand side
	plot(phylo.d1, cex=0.75, font=1, no.margin=TRUE, direction='rightwards', label.offset=0.015)
	mtext(lab.1, side=3, line=-1.5, font=2, cex=0.75)

	# save results of phylo environment
	p.left <- get("last_plot.phylo", envir = .PlotPhyloEnv)

	# plot site+hz-level dissimilarities on right-hand side
	plot(phylo.d2, cex=0.75, font=1, direction='leftwards', no.margin=TRUE, label.offset=0.015)
	mtext(lab.2, side=3, line=-1.5, font=2, cex=0.75)

	# save results of phylo environment
	p.right <- get("last_plot.phylo", envir = .PlotPhyloEnv)

	# get ordering of pedon IDs on each side
	left.new_order <- sapply(1:p.left$Ntip, function(i) which(as.integer(p.left$yy[1:p.left$Ntip]) == i))
	right.new_order <- sapply(1:p.right$Ntip, function(i) which(as.integer(p.right$yy[1:p.right$Ntip]) == i))

	# re-order right-hand side nodes so that they match the ordering of left-side nodes
	left.right.converstion.order <- right.new_order[match(left.new_order, right.new_order)]

	# setup new plot region
	par(mar=c(0,0,0,0))
	plot(0, 0, type='n', xlim=c(0,1), ylim=range(p.left$yy), axes=FALSE, xpd=FALSE)

	# plot connecting segments
	segments(x0=arrow.left, y0=p.left$yy[left.new_order], x1=arrow.right, 
y1=p.right$yy[left.right.converstion.order], col='RoyalBlue')

	# plot helper arrows
	arrows(x0=arrow.left, y0=p.left$yy[left.new_order], x1=arrow.left-arrow.length, y1=p.left$yy[left.new_order], col='RoyalBlue', code=2,length=0.05)
	arrows(x0=arrow.right, y0=p.right$yy[right.new_order], x1=arrow.right+arrow.length, y1=p.right$yy[right.new_order], col='RoyalBlue', code=2,length=0.05)
	
}
