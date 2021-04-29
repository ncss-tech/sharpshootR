

#' @title Soil Taxonomy Dendrogram
#' 
#' @description Plot a dendrogram based on the first 4 levels of Soil Taxonomy, with soil profiles hanging below. A dissimilarity matrix is computed using Gower's distance metric for nominal-scale variables, based on order, sub order, great group, and subgroup level taxa. See the Details and Examples sections below for more information.
#'
#' @param spc a `SoilProfileCollection` object, typically returned by `soilDB::fetchOSD`
#' @param name column name containing horizon names
#' @param name.style passed to `aqp::plotSPC` (default: "right-center")
#' @param rotationOrder numeric vector with desired ordering of leaves in the dendrogram from left to right, or character vector matching profile IDs
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
#' @details This function looks for specific site-level attributes named: `soilorder`, `suborder`, `greatgroup`, and `subgroup`.
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
#' \donttest{
#' 
#' if(requireNamespace("curl") &
#'    curl::has_internet() &
#'    require(aqp) &
#'    require(soilDB)
#' ) {
#'   
#'   
#'   
#'   # soils of interest
#'   s.list <- c('musick', 'cecil', 'drummer', 'amador', 'pentz', 'reiff', 
#'               'san joaquin','montpellier','grangeville','pollasky','ramona')
#'   
#'   # fetch and convert data into an SPC
#'   h <- fetchOSD(s.list)
#'   
#'   # plot dendrogram + profiles
#'   SoilTaxonomyDendrogram(h)
#'   
#'   # again, this time save the pair-wise dissimilarity matrix
#'   # note that there isn't a lot of discrimination between soils
#'   (d <- SoilTaxonomyDendrogram(h))
#'   
#'   
#'   # a different set
#'   soils <- c('cecil', 'altavista', 'lloyd', 'wickham', 'wilkes',
#'              'chewacla', 'congaree')
#'   
#'   # get morphology + extended summaries for sorting of dendrogram
#'   s <- fetchOSD(soils, extended = TRUE)
#'   
#'   # get summary and ignore the figure
#'   res <- vizHillslopePosition(s$hillpos)
#'   
#'   # compare default sorting to soils sorting according to catenary, e.g.
#'   # hillslope position
#'   op <- par(no.readonly = TRUE)
#'   
#'   par(mar=c(0,0,1,1), mfrow=c(2,1))
#'   
#'   SoilTaxonomyDendrogram(s$SPC, width=0.25)
#'   mtext('default sorting', side = 2, line=-1, font=3, cex=1.25)
#'   
#'   SoilTaxonomyDendrogram(s$SPC, rotationOrder = res$order, width=0.25)
#'   mtext('approx. catenary sorting', side = 2, line=-1, font=3, cex=1.25)
#'  
#'   
#' # classic chrono-sequence from the San Joaquin Valley, CA
#' 
#' s <- c('tujunga', 'hanford', 'greenfield', 'snelling', 'san joaquin')
#' osds <- fetchOSD(s)
#' 
#' idx <- match(toupper(s), profile_id(osds))
#' 
#' # encode horizon boundarydistinctness via vertical offset
#' osds$hd <- hzDistinctnessCodeToOffset(
#'   osds$distinctness, 
#'   codes=c('very abrupt', 'abrupt', 'clear', 'gradual', 'diffuse')
#' )
#' 
#' # encode horizon boundary topography via vertical offset
#' osds$hzto <- hzTopographyCodeToOffset(
#'   osds$topography, 
#'   codes = c('smooth', 'wavy', 'irregular', 'broken')
#' )
#' 
#' # also encode horizon boundary topography las line type
#' osds$hzto.lty <- hzTopographyCodeToLineType(
#'   osds$topography,
#'   codes = c('smooth', 'wavy', 'irregular', 'broken')
#' )
#' 
#' # label data source, used later 
#' site(osds)$source <- 'OSD'
#' 
#' # concise representation of hz bnd distinctness and topography
#' # similar to field notes
#' osds$bnd.code <- sprintf(
#'   "%s%s",
#'   substr(osds$distinctness, 1, 1),
#'   substr(osds$topography, 1, 1)
#' )
#' 
#' # remove NA
#' osds$bnd.code <- gsub('NANA', '', osds$bnd.code)
#' 
#' par(mar = c(0, 0, 0, 1), bg = 'black', fg = 'white')
#' 
#' plotSPC(
#' osds, 
#' plot.order = idx, 
#' width = 0.3, 
#' name.style = 'center-center', 
#' cex.names = 0.66, 
#' plot.depth.axis = FALSE, 
#' hz.depths = TRUE, 
#' shrink = TRUE, 
#' hz.distinctness.offset = 'hd', 
#' hz.topography.offset = 'hzto', 
#' hz.boundary.lty = 'hzto.lty'
#' )
#' 
#' legend(
#' 'bottomright', 
#' horiz = TRUE, 
#' legend = c('Smooth', 'Wavy', 'Irregular', 'Broken'), 
#' lty = 1:4, 
#' inset = 0.05, 
#' bty = 'n', 
#' cex = 0.85
#' )
#' 
#' # note that `rotationOrder` uses the ordering of series names (uppercase to match profile IDs)
#' # to re-order the terminal branches of the dendrogram
#' SoilTaxonomyDendrogram(
#' osds, 
#' rotationOrder = toupper(s), 
#' cex.taxon.labels = 0.85, 
#' width = 0.3, 
#' name.style = 'center-center', 
#' cex.names = 0.66, 
#' plot.depth.axis = FALSE, 
#' hz.depths = TRUE, 
#' shrink = TRUE, 
#' hz.distinctness.offset = 'hd', 
#' hz.topography.offset = 'hzto', 
#' hz.boundary.lty = 'hzto.lty'
#' )
#' 
#' legend(
#' 'bottomright', 
#' horiz = TRUE, 
#' legend = c('Smooth', 'Wavy', 'Irregular', 'Broken'), 
#' lty = 1:4, 
#' inset = 0.05, 
#' bty = 'n', 
#' cex = 0.85
#' )
#'
#' par(op)
#'
#' }
#' 
#' }
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
	
	if(!missing(rotationOrder)) {
	  # check for required packages
	  if(!requireNamespace('dendextend', quietly=TRUE))
	    stop('please install the `dendextend` packages', call.=FALSE)
	  
	  # rotate branches as closely as possible to `rotationOrder`
	  # sorting ideally results in left -> right orientation
	  s.hclust <- dendextend::rotate(s.hclust, order = rotationOrder)
	}
	
	# convert to phylo class
	dend <- as.phylo(s.hclust)
	
	# determine best-possible locations for taxa names
	max.dist <- max(s.dist)
	taxa.lab.y.vect <- c(max.dist / 1.6666666, (max.dist / 1.6666666) + 0.12)
	
	# device options are modified locally, reset when done
	op <- par(no.readonly = TRUE)
	on.exit(par(op))
	
	# setup plot and add dendrogram
	par(mar=c(0,0,0,0))
	plot(dend, cex=0.8, direction='up', y.lim=c(4,0), x.lim=c(0.5, length(spc)+1), show.tip.label=FALSE, edge.color=dend.color, edge.width=dend.width)
	
	# get the last plot geometry
	lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  
	# vector of indices for plotting soil profiles below leaves of dendrogram
	new_order <- s.hclust$order
	
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
	    dist=s.dist, 
	    order=s.hclust$order
	  )
	)
}

