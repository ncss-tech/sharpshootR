## TODO: update depth axis argument to aqp 2.0 style once on CRAN
## TODO: adapt to use plotSPC options


#' @title Soil Taxonomy Dendrogram
#' 
#' @description Plot a dendrogram based on the first 4 levels of Soil Taxonomy, with soil profiles hanging below. A dissimilarity matrix is computed using Gower's distance metric for nominal (`KST.order = FALSE`) or ordinal (`KST.order = TRUE`) scale variables, based on soil order, suborder, greatgroup, and subgroup taxa.
#'
#' @param spc a `SoilProfileCollection` object, typically returned by `soilDB::fetchOSD`
#' @param KST.order logical, encode / cluster taxa via ordinal factors, based on ordering within Keys to Soil Taxonomy
#' @param rotationOrder character vector of profile IDs with desired ordering of leaves in the dendrogram from left to right; exact ordering is not always possible
#' @param level character. One or more site-level columns in `spc`. Default: `"soilorder"`, `"suborder"`, `"greatgroup"` and `"subgroup"`
#' @param cluster.method Either "divisive" (`cluster::diana()`; default) or "agglomerative" (`cluster::agnes()`)
#' @param cluster.args Optional: additional arguments for `cluster::diana()` or `cluster::agnes()` cluster methods
#' @param name column name containing horizon names
#' @param name.style passed to `aqp::plotSPC`
#' @param id.style passed to `aqp::plotSPC`
#' @param max.depth depth at which profiles are truncated for plotting
#' @param n.depth.ticks suggested number of ticks on the depth axis
#' @param scaling.factor scaling factor used to convert depth units into plotting units
#' @param cex.names character scaling for horizon names
#' @param cex.id character scaling for profile IDs
#' @param width width of profiles
#' @param y.offset vertical offset between dendrogram and profiles
#' @param shrink logical, should long horizon names be shrunk by 80% ?
#' @param font.id integer, font style applied to profile id, default is 2 (bold)
#' @param cex.taxon.labels numeric, character scaling for taxonomic information
#' @param font.taxon.labels integer, font style applied to taxa labels, default is 3 (italic)
#' @param dend.color dendrogram line color
#' @param dend.width dendrogram line width
#' @param dend.type dendrogram type, passed to `plot.phylo()`, either "phylogram" or "cladogram"
#' @param ... additional arguments to `aqp::plotSPC`
#' 
#' @details This function looks for specific site-level attributes named: `"soilorder"`, `"suborder"`, `"greatgroup"`, and `"subgroup"`, or their NASIS physical column name analogues `"taxorder"`, `"taxsuborder"`, `"taxgrtgroup"`, and `"taxsubgrp"`. See \url{https://github.com/ncss-tech/sharpshootR/blob/master/misc/soilTaxonomyDendrogram-examples.R} for some examples.
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
#' 
SoilTaxonomyDendrogram <- function(spc, 
                                   KST.order = TRUE, 
                                   rotationOrder = NULL, 
                                   level = c(
                                     soilorder = "soilorder",
                                     suborder = "suborder",
                                     greatgroup = "greatgroup",
                                     subgroup = "subgroup"
                                   ),
                                   cluster.method = c("divisive", "agglomerative"),
                                   cluster.args = list(),
                                   name = 'hzname', 
                                   name.style = 'center-center', 
                                   id.style = 'side', 
                                   n.depth.ticks = 6, 
                                   scaling.factor = 0.015, 
                                   cex.names = 0.75, 
                                   cex.id = 0.75, 
                                   width = 0.25, 
                                   y.offset = 0.5, 
                                   shrink = FALSE, 
                                   font.id = 2, 
                                   cex.taxon.labels = 0.66,
                                   font.taxon.labels = 3, 
                                   dend.color = par('fg'), 
                                   dend.width = 1,
                                   dend.type = c("phylogram", "cladogram"),
                                   max.depth = ifelse(is.infinite(max(spc)), 200, max(spc)),
                                   ...) {
           
  # choice of cluster methods: use diana() or agnes()
  cluster.method <- match.arg(tolower(cluster.method), c("divisive", "agglomerative"))
  
  # dendrogram type, passed to plot.phylo
  dend.type <- match.arg(tolower(dend.type), c("phylogram", "cladogram"), several.ok = FALSE)
  
  # attempt KST-based ordering:
  # 1. setup ordinal factors based on order of taxa with each level of ST hierarchy
  # 2. rotate dendrogram to reflect ordering of subgroups within keys
  if (KST.order) {
    
    # requires SoilTaxonomy >= 0.1.5 (2022-02-15)
    if (!requireNamespace('SoilTaxonomy', quietly = TRUE)) {
      stop('please install the `SoilTaxonomy` package', call. = FALSE)
    }
    
    ST_unique_list <- NULL
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
    
    # check for NA (obsolete taxa / typos); fall back to nominal factors
    if (any(is.na(.soilorder))) {
      .soilorder <- factor(spc$soilorder, ordered = FALSE)
      message('obsolete soil order or typo, reverting to nominal factors')
    }
    
    if (any(is.na(.suborder))) {
      .suborder <- factor(spc$suborder, ordered = FALSE)
      message('obsolete suborder or typo, reverting to nominal factors')
    }
    
    if (any(is.na(.greatgroup))) {
      .greatgroup <- factor(spc$greatgroup, ordered = FALSE)
      message('obsolete greatgroup or typo, reverting to nominal factors')
    }
    
    if (any(is.na(.subgroup))) {
      .subgroup <- factor(spc$subgroup, ordered = FALSE)
      message('obsolete subgroup or typo, reverting to nominal factors')
    }
    
    # replace original values with ordered factors if possible
    spc$soilorder <- .soilorder
    spc$suborder <- .suborder
    spc$greatgroup <- .greatgroup
    spc$subgroup <- .subgroup
    
    # rotate as close to KST ordering as possible
    if (is.null(rotationOrder)) {
      rotationOrder <- profile_id(spc)[order(spc[[level[length(level)]]])]
    }
    
  } else {
    # treat all `level` columns as nominal factors
    # this is required to ensure columns with n=1 or n=2 levels are not "binary" in clustering algorithms
    for (l in level) {
      spc[[l]] <- factor(spc[[l]])
    }
  }
  
	# extract site attributes as data.frame
	s <- site(spc)
	
	# copy soil ID to row.names, so that they are preserved in the distance matrix
	row.names(s) <- s[[idname(spc)]]
	
	# compute distance matrix from specified levels
	s.dist <- daisy(s[, level, drop = FALSE], metric = 'gower')
	
	if (cluster.method == "divisive") {
	  # cluster::diana is the default/"divisive" method
	  s.clust <- do.call(cluster::diana, c(list(x = s.dist), cluster.args))
	} else if (cluster.method == "agglomerative") {
	  # cluster::agnes is the agglomerative method
	  s.clust <- do.call(cluster::agnes, c(list(x = s.dist), cluster.args))
	}
	
	# convert to phylo class
	dend <- as.phylo(as.hclust(s.clust))
	
	## 2022-05-04: switching to ape rotation methods
	# requires vector of tip labels
	if (!is.null(rotationOrder)) {
	  # check that none are missing
	  if (all(rotationOrder %in% profile_id(spc)) && length(rotationOrder) == length(spc)) {
	    # attempt rotation, may not give the exact ordering
	    dend <- rotateConstr(dend, constraint = rotationOrder) 
	  } else {
	    message('`rotationOrder` does not contain a complete set of profile IDs')
	  }
	}
	
	## TODO: these estimates work for divisive, need adjustments for agglomerative
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
  	plot(dend, cex = 0.8, direction = 'up', y.lim = c(4,0), x.lim = c(0.5, length(spc) + 1), show.tip.label = FALSE, edge.color = dend.color, edge.width = dend.width, type = dend.type)
	
	# get the last plot geometry
	lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  
	# vector of indices for plotting soil profiles below leaves of dendrogram
	# requires conversion back to hclust
	# in case `dend` was re-ordered
	new_order <- as.hclust(dend)$order
	
	## TODO: allow most plotSPC arguments to be set by options
	# get arguments to plotSPC set via options
	# override this function's default values with these values
	# .opArgs <- getOption(".aqp.plotSPC.args", default = list())
	
	# plot the profiles, in the ordering defined by the dendrogram
	# with a couple fudge factors to make them fit
	plotSPC(spc,
	        add = TRUE,
	        y.offset = max(lastPP$yy) + y.offset, 
	        plot.order = new_order, 
	        name = name, 
	        name.style = name.style, 
	        n.depth.ticks = n.depth.ticks, 
	        scaling.factor = scaling.factor, 
	        cex.names = cex.names, 
	        cex.id = cex.id, 
	        width = width, 
	        id.style = id.style, 
	        shrink = shrink, 
	        font.id = font.id,
	        max.depth = max.depth,
	        ...
	)
	
	# generate subgroup labels and their positions under the dendrogram
	lab <- s[new_order, level[length(level)]]
	unique.lab <- unique(lab)
	group.lengths <- rle(as.numeric(lab))$lengths
	lab.x.positions <- (cumsum(group.lengths) - (group.lengths / 2)) + 0.5
	lab.y.positions <- rep(taxa.lab.y.vect, length.out = length(unique.lab))
	
	# add subgroup labels
	# note manual tweaking of y-coordinates
	text(lab.x.positions, lab.y.positions, unique.lab, cex = cex.taxon.labels, adj = 0.5, font = font.taxon.labels)
	
	# invisibly return some information form the original objects
	invisible(
	  list(
	    dist = s.dist,
	    order = new_order
	  )
	)
}

