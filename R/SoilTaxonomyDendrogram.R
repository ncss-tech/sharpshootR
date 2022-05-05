
## TODO: allow for user-defined clustering functions

#' @title Soil Taxonomy Dendrogram
#' 
#' @description Plot a dendrogram based on the first 4 levels of Soil Taxonomy, with soil profiles hanging below. A dissimilarity matrix is computed using Gower's distance metric for nominal-scale variables, based on order, sub order, great group, and subgroup level taxa. See the Details and Examples sections below for more information.
#'
#' @param spc a `SoilProfileCollection` object, typically returned by `soilDB::fetchOSD`
#' @param name column name containing horizon names
#' @param name.style passed to `aqp::plotSPC` (default: `"right-center"`)
#' @param rotationOrder character vector of profile IDs with desired ordering of leaves in the dendrogram from left to right, ordering is not always possible
#' @param max.depth depth at which profiles are truncated for plotting
#' @param n.depth.ticks suggested number of ticks on the depth axis
#' @param scaling.factor scaling factor used to convert depth units into plotting units
#' @param cex.names character scaling for horizon names
#' @param cex.id character scaling for profile IDs
#' @param axis.line.offset horizontal offset for depth axis
#' @param width width of profiles
#' @param y.offset vertical offset between dendrogram and profiles
#' @param shrink logical, should long horizon names be shrunk by 80% ?
#' @param font.id font style applied to profile id, default is 2 (bold)
#' @param cex.taxon.labels character scaling for taxonomic information
#' @param dend.color dendrogram line color
#' @param dend.width dendrogram line width
#' @param ... additional arguments to `aqp::plotSPC`
#' 
#' @details This function looks for specific site-level attributes named: `soilorder`, `suborder`, `greatgroup`, and `subgroup`. See `misc/soilTaxonomyDendrogram-examples.R` for some examples.
#' 
#' The `rotationOrder` argument uses (requires) the `dendextend::rotate()` function to re-order leaves within the `hclust` representation of the ST hierarchy. Perfect sorting is not always possible.
#'
#' @return An invisibly-returned list containing:
#'
#'   * `dist`: pair-wise dissimilarity matrix
#'   * `order`: final ordering of hclust leaves
#'   
#' @author D.E. Beaudette
#' 
#' @export
#' 
#' @examples 
#' 
#' # built-in data, same as results from soilDB::fetchOSD()
#' data("OSDexamples")
#' 
#' # use first 8 profiles
#' SoilTaxonomyDendrogram(
#' OSDexamples$SPC[1:8, ], width = 0.3, name.style = 'center-center'
#' )
#' 
SoilTaxonomyDendrogram <- function(spc, name='hzname', name.style='right-center', rotationOrder=NULL, max.depth=150, n.depth.ticks=6, scaling.factor=0.015, cex.names=0.75, cex.id=0.75, axis.line.offset=-4, width=0.1, y.offset=0.5, shrink=FALSE, font.id=2, cex.taxon.labels=0.66, dend.color=par('fg'), dend.width=1, ...) {
	
	# convert relevant columns into factors
	spc$soilorder <- factor(spc$soilorder)
	spc$suborder <- factor(spc$suborder)
	spc$greatgroup <- factor(spc$greatgroup)
	spc$subgroup <- factor(spc$subgroup)
	
	# extract site attributes as data.frame
	s <- site(spc)
	# copy soil ID to row.names, so that they are preserved in the distance matrix
	row.names(s) <- s[[idname(spc)]]
	
	# compute distance matrix from first 4 levels of Soil Taxonomy
	s.dist <- daisy(s[, c('soilorder', 'suborder', 'greatgroup', 'subgroup')], metric='gower')
	s.hclust <- as.hclust(diana(s.dist))
	
	# convert to phylo class
	dend <- as.phylo(s.hclust)
	
	## 2022-05-04: switching to ape rotation methods
	# requires vector of tip labels
	if(! is.null(rotationOrder)) {
	  
	  # check that none are missing
	  if(all(rotationOrder %in% profile_id(spc)) && length(rotationOrder) == length(spc)) {
	    # attempt rotation, may not give the exact ordering
	    dend <- rotateConstr(dend, constraint = rotationOrder) 
	  } else {
	    message('`rotationOrder` does not contain a complet set of profile IDs')
	  }
	  
	}

	
	# determine best-possible locations for taxa names
	max.dist <- max(s.dist)
	taxa.lab.y.vect <- c(max.dist / 1.6666666, (max.dist / 1.6666666) + 0.12)
	
	## 2021-10-12: removing this because we often need to annotate these figures AFTER created
	##             someday we can investigate how to update `op` with final coordinates
	## device options are modified locally, reset when done
	# op <- par(no.readonly = TRUE)
	# on.exit(par(op))
	
	# setup plot and add dendrogram
	par(mar=c(0,0,0,0))
	plot(dend, cex=0.8, direction='up', y.lim=c(4,0), x.lim=c(0.5, length(spc)+1), show.tip.label=FALSE, edge.color=dend.color, edge.width=dend.width)
	
	# get the last plot geometry
	lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  
	# vector of indices for plotting soil profiles below leaves of dendrogram
	# requires conversion back to hclust
	# in case `dend` was re-ordered
	new_order <- as.hclust(dend)$order
	
	# plot the profiles, in the ordering defined by the dendrogram
	# with a couple fudge factors to make them fit
	plotSPC(spc, name=name, name.style=name.style, plot.order=new_order, max.depth=max.depth, n.depth.ticks=n.depth.ticks, scaling.factor=scaling.factor, cex.names=cex.names, cex.id=cex.id, axis.line.offset=axis.line.offset, width=width, y.offset=max(lastPP$yy) + y.offset, id.style='side', shrink=shrink, font.id=font.id, add=TRUE, ...)
	
	# generate taxonomic labels and their positions under the dendrogram
	lab <- s[new_order, 'subgroup']
	unique.lab <- unique(lab)
	group.lengths <- rle(as.numeric(lab))$lengths
	lab.x.positions <- (cumsum(group.lengths) - (group.lengths / 2)) + 0.5
	lab.y.positions <- rep(taxa.lab.y.vect, length.out=length(unique.lab))
	
	# add labels-- note manual tweaking of y-coordinates
	text(lab.x.positions, lab.y.positions, unique.lab, cex=cex.taxon.labels, adj=0.5, font=3)
	
	# invisibly return some information form the original objects
	invisible(
	  list(
	    dist = s.dist,
	    order = new_order
	  )
	)
}

