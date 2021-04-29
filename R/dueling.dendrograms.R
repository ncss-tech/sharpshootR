
#' @title Dueling Dendrograms
#' 
#' @description Graphically compare two related dendrograms
#'
#' @param p.1 left-hand phylo-class dendrogram
#' @param p.2 right-hand phylo-class dendrogram
#' @param lab.1 left-hand title
#' @param lab.2 right-hand title
#' @param cex.nodelabels character expansion size for node labels
#' @param arrow.length arrow head size
#' 
#' @details Connector arrows are used to link nodes from the left-hand dendrogram to the right-hand dendrogram.
#' 
#' @keywords hplots
#' 
#' @return nothing is returned, function is called to generate graphical output
#' 
#' @author D.E. Beaudette
#'
#' @export
#'
#' @examples
#' if(require(aqp) &
#' require(cluster) &
#'   require(latticeExtra) &
#'   require(ape)
#' ) {
#'   
#'   # load sample dataset from aqp package
#'   data(sp3)
#'   
#'   # promote to SoilProfileCollection
#'   depths(sp3) <- id ~ top + bottom
#'   
#'   # compute dissimilarity using different sets of variables
#'   # note that these are rescaled to the interval [0,1]
#'   d.1 <- profile_compare(sp3, vars=c('clay', 'cec'), k=0, max_d=100, rescale.result=TRUE)
#'   d.2 <- profile_compare(sp3, vars=c('clay', 'L'), k=0, max_d=100, rescale.result=TRUE)
#'   
#'   # cluster via divisive hierarchical algorithm
#'   # convert to 'phylo' class
#'   p.1 <- as.phylo(as.hclust(diana(d.1)))
#'   p.2 <- as.phylo(as.hclust(diana(d.2)))
#'   
#'   # graphically compare two dendrograms
#'   dueling.dendrograms(p.1, p.2, lab.1='clay and CEC', lab.2='clay and L')
#'   
#'   # graphically check the results of ladderize() from ape package
#'   dueling.dendrograms(p.1, ladderize(p.1), lab.1='standard', lab.2='ladderized')
#'   
#'   # sanity-check: compare something to itself
#'   dueling.dendrograms(p.1, p.1, lab.1='same', lab.2='same')
#'   
#'   # graphically compare diana() to agnes() using d.2
#'   dueling.dendrograms(as.phylo(as.hclust(diana(d.2))), 
#'                       as.phylo(as.hclust(agnes(d.2))), lab.1='diana', lab.2='agnes')
#' }
#' 
dueling.dendrograms <- function(p.1, p.2, lab.1='D1', lab.2='D2', cex.nodelabels=0.75, arrow.length=0.05) {
	
  # this function only works with ultrametric objects
  if(!is.ultrametric(p.1) || !is.ultrametric(p.2))
    stop('both phylo objects must be ultrametric')
  
	# setup some geometry for connecting lines / arrows
	arrow.left <- 0.1
	arrow.right <- 0.9
	arrow.length <- 0.1

	# setup plot layout
	lo <- layout(matrix(c(1,3,2), ncol=3), widths=c(1, 1, 1)) # check with: layout.show(lo)

	# left-hand side dendrogram
	plot(p.1, cex=cex.nodelabels, font=1, no.margin=TRUE, direction='rightwards', label.offset=0.015)
	mtext(lab.1, side=3, line=-1.5, font=2, cex=0.75)

	# save results of phylo environment
	p.left <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  
  # get left-hand IDs
  p.left.ids <- as.hclust(p.1)$labels
  
	# right-hand side dendrogram
	plot(p.2, cex=cex.nodelabels, font=1, direction='leftwards', no.margin=TRUE, label.offset=0.015)
	mtext(lab.2, side=3, line=-1.5, font=2, cex=0.75)

	# save results of phylo environment
	p.right <- get("last_plot.phylo", envir = .PlotPhyloEnv)

	# get right-hand IDs
	p.right.ids <- as.hclust(p.2)$labels
  
	# get ordering of pedon IDs on each side
	left.new_order <- sapply(1:p.left$Ntip, function(i) which(as.integer(p.left$yy[1:p.left$Ntip]) == i))
	right.new_order <- sapply(1:p.right$Ntip, function(i) which(as.integer(p.right$yy[1:p.right$Ntip]) == i))

	# re-order right-hand side nodes so that they match the ordering of left-side nodes
	left.right.converstion.order <- right.new_order[match(left.new_order, right.new_order)]

	# setup new plot region
	op <- par(no.readonly = TRUE)
	on.exit(par(op))
	
	par(mar=c(0,0,0,0))
	plot(0, 0, type='n', xlim=c(0,1), ylim=range(p.left$yy), axes=FALSE, xpd=FALSE)

	# plot connecting segments
	segments(x0=arrow.left, y0=p.left$yy[left.new_order], x1=arrow.right, 
y1=p.right$yy[left.right.converstion.order], col='RoyalBlue')

	# plot helper arrows
	arrows(x0=arrow.left, y0=p.left$yy[left.new_order], x1=arrow.left-arrow.length, y1=p.left$yy[left.new_order], col='RoyalBlue', code=2, length=arrow.length)
	arrows(x0=arrow.right, y0=p.right$yy[right.new_order], x1=arrow.right+arrow.length, y1=p.right$yy[right.new_order], col='RoyalBlue', code=2, length=arrow.length)
	
}
