
#' @title Soil Taxonomy Dendrogram
#' 
#' @description Plot a dendrogram based on the first 4 levels of Soil Taxonomy, with soil profiles hanging below. A dissimilarity matrix is computed using Gower's distance metric for nominal (`KST.order = FALSE`) or ordinal (`KST.order = TRUE`) scale variables, based on soil order, suborder, greatgroup, and subgroup taxa.
#'
#' @param spc a `SoilProfileCollection` object, typically returned by `soilDB::fetchOSD`
#' @param KST.order logical, encode / cluster taxa via ordinal factors, based on ordering within Keys to Soil Taxonomy
#' @param rotationOrder character vector of profile IDs with desired ordering of leaves in the dendrogram from left to right; exact ordering is not always possible
#' @param name column name containing horizon names
#' @param name.style passed to `aqp::plotSPC`
#' @param id.style passed to `aqp::plotSPC`
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
#' The `rotationOrder` argument uses `ape::rotateConstr()` to reorder leaves within the `hclust` representation of the ST hierarchy. Perfect sorting is not always possible.
#'
#' @return An invisibly-returned list containing:
#'
#'   * `dist`: pair-wise dissimilarity matrix
#'   * `order`: final ordering of `hclust` leaves
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
#' # examples using first 8 profiles
#' 
#' # KST-style ordering
#' SoilTaxonomyDendrogram(
#'   OSDexamples$SPC[1:8, ], width = 0.3, name.style = 'center-center',
#'   KST.order = TRUE
#' )
#' 
#' # classic ordering, based on nominal scale variables (unordered factors)
#' SoilTaxonomyDendrogram(
#'   OSDexamples$SPC[1:8, ], width = 0.3, name.style = 'center-center',
#'   KST.order = FALSE
#' )
#' 
#' 
SoilTaxonomyDendrogram <- function(spc, KST.order = TRUE, rotationOrder = NULL, name = 'hzname', name.style = 'center-center', id.style = 'side', max.depth = max(spc), n.depth.ticks = 6, scaling.factor = 0.015, cex.names = 0.75, cex.id = 0.75, axis.line.offset = -4, width = 0.1, y.offset = 0.5, shrink = FALSE, font.id = 2, cex.taxon.labels = 0.66, dend.color = par('fg'), dend.width = 1, ...) {
	
  
  # attempt KST-based ordering:
  # 1. setup ordinal factors based on order of taxa with each level of ST hierarchy
  # 2. rotate dendrogram to reflect ordering of subgroups within keys
  # note: will create NA if obsolete taxa or typos -> test for this
  if (KST.order) {
    
    
    # requires SoilTaxonomy >= 0.1.5 (2022-02-15)
    if (!requireNamespace('SoilTaxonomy', quietly = TRUE)) {
      stop('please install the `SoilTaxonomy` packages', call. = FALSE)
    }
    
    # TODO: a function to set ordered factors in a data.frame-like object should be added to SoilTaxonomy
    ST_unique_list <- NULL
    
    # note: this is incompatible with LazyData: true
    load(system.file("data/ST_unique_list.rda", package = "SoilTaxonomy")[1])
    
    # support for NASIS physical column names (NASIS possibly should be default?)
    if (is.null(spc$soilorder) & !is.null(spc$taxorder)) {
      spc$soilorder <- spc$taxorder
    }
    if (is.null(spc$suborder) & !is.null(spc$taxsuborder)) {
      spc$suborder <- spc$taxsuborder
    }    
    if (is.null(spc$greatgroup) & !is.null(spc$taxgrtgroup)) {
      spc$greatgroup <- spc$taxgrtgroup
    }    
    if (is.null(spc$subgroup) & !is.null(spc$taxsubgrp)) {
      spc$subgroup <- spc$taxsubgrp
    }
    
    # create ordered factors, dropping unused levels, ignore case
    .soilorder <- droplevels(factor(tolower(spc$soilorder), levels = ST_unique_list$order, ordered = TRUE))
    .suborder <- droplevels(factor(tolower(spc$suborder), levels = ST_unique_list$suborder, ordered = TRUE))
    .greatgroup <- droplevels(factor(tolower(spc$greatgroup), levels = ST_unique_list$greatgroup, ordered = TRUE))
    .subgroup <- droplevels(factor(tolower(spc$subgroup), levels = ST_unique_list$subgroup, ordered = TRUE))
    
    # check for obsolete taxa / typos
    # use plain factors
    if (any(is.na(.soilorder))) {
      .soilorder <- factor(spc$soilorder)
      message('obsolete soil order or typo, reverting to regular factors: sorting will not be exact')
    }
    
    if (any(is.na(.suborder))) {
      .suborder <- factor(spc$suborder)
      message('obsolete suborder or typo, reverting to regular factors: sorting will not be exact')
    }
    
    if (any(is.na(.greatgroup))) {
      .greatgroup <- factor(spc$greatgroup)
      message('obsolete greatgroup or typo, reverting to regular factors: sorting will not be exact')
    }
    
    if (any(is.na(.subgroup))) {
      .subgroup <- factor(spc$subgroup)
      message('obsolete subgroup or typo, reverting to regular factors: sorting will not be exact')
    }
    
    # replace original values with ordered factors if possible
    # falling back to plain factors
    spc$soilorder <- .soilorder
    spc$suborder <- .suborder
    spc$greatgroup <- .greatgroup
    spc$subgroup <- .subgroup
    
    # rotate as close to KST ordering as possible
    # but only if there is no user-supplied rotation
    if(is.null(rotationOrder)) {
      rotationOrder <- profile_id(spc)[order(spc$subgroup)]
    }
    
  } else {
    
    # treat as nominal factors
    spc$soilorder <- factor(spc$soilorder)
    spc$suborder <- factor(spc$suborder)
    spc$greatgroup <- factor(spc$greatgroup)
    spc$subgroup <- factor(spc$subgroup)
  }
  
	
	
	# extract site attributes as data.frame
	s <- site(spc)
	# copy soil ID to row.names, so that they are preserved in the distance matrix
	row.names(s) <- s[[idname(spc)]]
	
	# compute distance matrix from first 4 levels of Soil Taxonomy
	s.dist <- daisy(s[, c('soilorder', 'suborder', 'greatgroup', 'subgroup')], metric = 'gower')
	
	# use divisive clustering, all other methods produce less than ideal results 
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
	    message('`rotationOrder` does not contain a complete set of profile IDs')
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
	par(mar = c(0,0,0,0))
	plot(dend, cex = 0.8, direction = 'up', y.lim = c(4,0), x.lim = c(0.5, length(spc) + 1), show.tip.label = FALSE, edge.color = dend.color, edge.width = dend.width)
	
	# get the last plot geometry
	lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  
	# vector of indices for plotting soil profiles below leaves of dendrogram
	# requires conversion back to hclust
	# in case `dend` was re-ordered
	new_order <- as.hclust(dend)$order
	
	# plot the profiles, in the ordering defined by the dendrogram
	# with a couple fudge factors to make them fit
	plotSPC(spc, 
	        name = name, 
	        name.style = name.style, 
	        plot.order = new_order, 
	        max.depth = max.depth, 
	        n.depth.ticks = n.depth.ticks, 
	        scaling.factor = scaling.factor, 
	        cex.names = cex.names, 
	        cex.id = cex.id, 
	        axis.line.offset = axis.line.offset, 
	        width = width, 
	        y.offset = max(lastPP$yy) + y.offset, 
	        id.style = id.style, 
	        shrink = shrink, 
	        font.id = font.id, 
	        add = TRUE, 
	        ...
	)
	
	# generate subgroup labels and their positions under the dendrogram
	lab <- s[new_order, 'subgroup']
	unique.lab <- unique(lab)
	group.lengths <- rle(as.numeric(lab))$lengths
	lab.x.positions <- (cumsum(group.lengths) - (group.lengths / 2)) + 0.5
	lab.y.positions <- rep(taxa.lab.y.vect, length.out = length(unique.lab))
	
	# add subgroup labels
	# note manual tweaking of y-coordinates
	text(lab.x.positions, lab.y.positions, unique.lab, cex = cex.taxon.labels, adj = 0.5, font = 3)
	
	# invisibly return some information form the original objects
	invisible(
	  list(
	    dist = s.dist,
	    order = new_order
	  )
	)
}

