## TODO 
# * generalize to other gemorphic summaries (everything hard-coded for hillslope position)
# * encode Shannon entropy: values are computed row-wise, data plotted as columns


#' @title Present a `SoilProfileCollection` aligned to a geomorphic summary as cross-section.
#'
#' @param x resulting list from `soilDB::fetchOSD(..., extended = TRUE)`
#' @param type character, 'line' for line plot or 'bar' for barplot of geomorphic proportions
#' @param g character, select a geomorphic summary. Currently 'hillpos' (2D hillslope position) is the only supported choice.
#' @param clust logical, use clustering order of geomorphic proportions (`TRUE`) or exact hydrologic ordering (`FALSE`), see [`hydOrder()`]
#' @param col character vector of colors
#' @param \dots additional arguments to [`iterateHydOrder()`]
#' 
#' @details Additional arguments to [aqp::plotSPC()] can be provided using `options(.aqp.plotSPC.args = list(...))`. For example, adjustments to maximum depth and profile width can be set via: `options(.aqp.plotSPC.args = list(max.depth = 150, width = 0.35)`. Default arguments can be reset with `options(.aqp.plotSPC.args = NULL`).
#' 
#' When `clust = TRUE`, especially for `SoilProfileCollections` with a wide range in depth, it may be necessary to adjust the `scaling.factor` argument to [aqp::plotSPC()] via: `options(.aqp.plotSPC.args = list(scaling.factor = 0.01))`. Larger values will increase the height of profile sketches.
#' 
#' @author D.E. Beaudette
#' @export
#'
plotGeomorphCrossSection <- function(x, type = c('line', 'bar'), g = 'hillpos', clust = TRUE, col = c("#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#E41A1C"), ...) {
  
  # satisfy R CMD check
  series <- NULL
  
  # get arguments to plotSPC set via options
  # override this function's default values with these values
  .opArgs <- getOption(".aqp.plotSPC.args", default = list())
  
  # sanity checks
  type <- match.arg(type)
  
  # eventually others will be supported
  # stopifnot(g %in% c('geomcomp', 'hillpos', 'flats', 'terrace', 'mtnpos', 'shape'))
  
  # for now, just hillpos
  stopifnot(g %in% c('hillpos'))
  
  # the latest soilDB::fetchOSD() will automatically encode horizon distinctness offset
  # backwards compatibility
  if(is.null(x$SPC$hzd)) {
    
    # convert horizon boundary distinctness -> vertical distance
    x$SPC$hzd <- hzDistinctnessCodeToOffset(
      x$SPC$distinctness, 
      codes = c('very abrupt', 'abrubt', 'clear', 'gradual', 'diffuse')
    )
  }
  
  # reconcile possible missing IDs
  o <- reconcileOSDGeomorph(x, 'hillpos')
  
  if(clust) {
    # perform clustering, keep only the hclust object
    # we later extract ordering vector via res$order
    res <- iterateHydOrder(o$geom, g = g, ...)$clust
  } else {
    # no clustering, just a plotting index
    res <- match(hydOrder(o$geom, g = 'hillpos', clust = FALSE), profile_id(o$SPC))
  }
  
  # re-order rows of geomorphic proportion matrix
  if(clust) {
    # re-order geomorphic proportions according to clustering
    hp <- o$geom[res$order, ]
    nm <- names(hp[, 2:6]) 
  } else {
    # re-order geomorphic proportions according to raw hydrologic ordering
    hp <- o$geom[res, ]
    nm <- names(hp[, 2:6])
  }
  
  
  ## the resulting figure is kind of confusing, and bar graphs broken
  ## saving for future ideas, may involve smaller point sizes or something like that for near-zero values
  
  # # convert near-zero proportions to NA
  # for(i in 2:6) {
  #   idx <- which(hp[, i] < 0.0001)
  #   if(length(idx) > 0) {
  #     hp[idx, i] <- NA
  #   }
  # }
    
    
  
  if(type == 'line') {
    
    # setup figure panes
    layout(
      matrix(c(1,2)), 
      widths = c(1,1), 
      heights = c(2,1)
    )
    
    # top panel
    if(clust) {
      # dendrogram + profiles
      plotProfileDendrogram(
        o$SPC, 
        clust = res, 
        dend.y.scale = 3, 
        scaling.factor = ifelse(!is.null(.opArgs$scaling.factor), .opArgs$scaling.factor, 0.01), 
        y.offset = ifelse(!is.null(.opArgs$y.offset), .opArgs$y.offset, 0.2),
        width = ifelse(!is.null(.opArgs$width), .opArgs$width, 0.32), 
        name.style = ifelse(!is.null(.opArgs$name.style), .opArgs$name.style, 'center-center'), 
        cex.names = ifelse(!is.null(.opArgs$cex.names), .opArgs$cex.names, 0.7), 
        shrink = ifelse(!is.null(.opArgs$shrink), .opArgs$shrink, TRUE), 
        cex.id = ifelse(!is.null(.opArgs$cex.id), .opArgs$cex.id, 0.55),
        hz.distinctness.offset = 'hzd'
      )
      
      # appropriate xlim for plotting region setup by plotProfileDendrogram()
      x.lim <- c(0.5, length(o$SPC) + 1)
      
    } else {
      # profiles only
      plotSPC(
        o$SPC, 
        plot.order = res,
        width = ifelse(!is.null(.opArgs$width), .opArgs$width, 0.32), 
        name.style = ifelse(!is.null(.opArgs$name.style), .opArgs$name.style, 'center-center'), 
        cex.names = ifelse(!is.null(.opArgs$cex.names), .opArgs$cex.names, 0.7), 
        shrink = ifelse(!is.null(.opArgs$shrink), .opArgs$shrink, TRUE), 
        cex.id = ifelse(!is.null(.opArgs$cex.id), .opArgs$cex.id, 0.55),
        hz.distinctness.offset = 'hzd'
      )
      
      # get plotting details from aqp environment
      lsp <- get('last_spc_plot', envir = aqp.env)
      x.lim <- lsp$xlim
      
      
      ## TODO: temporary fix until latest aqp is on CRAN
      # close approximation
      if(is.null(x.lim)) {
        x.lim <- c(0.5, length(o$SPC) + 1)
      }
      
    }
    
    # bottom panel
    # arrange geomorphic proportion line plot
    matplot(
      y = hp[, 2:6], 
      type = 'b', 
      lty = 1, 
      pch = 16, 
      axes = FALSE, 
      col = col, 
      xlab = '', 
      ylab = '', 
      xlim = x.lim
    )
    
    # proportion axis
    axis(side = 4, line = -1, las = 1, cex.axis = 0.7, col.axis = par('fg'))
    
    # legend
    legend('topleft', legend = rev(nm), col = rev(col), pch = 16, bty = 'n', cex = 0.8, pt.cex = 2, horiz = TRUE, inset = c(0.01, 0.01))
    mtext('Proportion', side = 2, line = -2, font = 2)  
  }
  
  
  if(type == 'bar') {
    
    # setup figure panes
    layout(
      matrix(c(1,2)), 
      widths = c(1,1), 
      heights = c(2,1)
    )
    
    # top panel
    if(clust) {
      # dendrogram + profiles
      plotProfileDendrogram(
        o$SPC, 
        clust = res, 
        dend.y.scale = 3, 
        scaling.factor = ifelse(!is.null(.opArgs$scaling.factor), .opArgs$scaling.factor, 0.01), 
        y.offset = ifelse(!is.null(.opArgs$y.offset), .opArgs$y.offset, 0.2),
        width = ifelse(!is.null(.opArgs$width), .opArgs$width, 0.32), 
        name.style = ifelse(!is.null(.opArgs$name.style), .opArgs$name.style, 'center-center'), 
        cex.names = ifelse(!is.null(.opArgs$cex.names), .opArgs$cex.names, 0.7), 
        shrink = ifelse(!is.null(.opArgs$shrink), .opArgs$shrink, TRUE), 
        cex.id = ifelse(!is.null(.opArgs$cex.id), .opArgs$cex.id, 0.55),
        hz.distinctness.offset = 'hzd'
      )
      
      # appropriate xlim for plotting region setup by plotProfileDendrogram()
      x.lim <- c(0.5, length(o$SPC) + 1)
      
    } else {
      # profiles only
      plotSPC(
        o$SPC, 
        plot.order = res,
        width = ifelse(!is.null(.opArgs$width), .opArgs$width, 0.32), 
        name.style = ifelse(!is.null(.opArgs$name.style), .opArgs$name.style, 'center-center'), 
        cex.names = ifelse(!is.null(.opArgs$cex.names), .opArgs$cex.names, 0.7), 
        shrink = ifelse(!is.null(.opArgs$shrink), .opArgs$shrink, TRUE), 
        cex.id = ifelse(!is.null(.opArgs$cex.id), .opArgs$cex.id, 0.55),
        hz.distinctness.offset = 'hzd'
      )
      
      # get plotting details from aqp environment
      lsp <- get('last_spc_plot', envir = aqp.env)
      x.lim <- lsp$xlim
      
      ## TODO: temporary fix until latest aqp is on CRAN
      # close approximation
      if(is.null(x.lim)) {
        x.lim <- c(0.5, length(o$SPC) + 1)
      }
    }
    
    
    
    # setup barplot
    sp <- c(1.5, rep(1, times = length(o$SPC) - 1))
    barplot(
      height = t(as.matrix(hp[, 2:6])), 
      beside = FALSE, 
      width = 0.5, 
      space = sp, 
      col = col, 
      axes = FALSE, 
      xlab = '', 
      ylab = '', 
      xlim = x.lim, 
      ylim = c(0, 1.2)
    )
    
    # annotate
    idx <- match(hp$series, profile_id(o$SPC))
    text(
      x = (1:nrow(hp)) + 0.4, 
      y = 0.5, 
      labels = o$SPC$subgroup[idx], 
      cex = 0.75, 
      srt = 90, 
      font = 2
    )
    
    # legend
    legend(x = 0.75, y = 1.2, legend = rev(nm), col = rev(col), pch = 15, bty = 'n', cex = 0.8, pt.cex = 1.25, horiz = TRUE)
    mtext('Proportion', side = 2, line = -2, font = 2)
    
  }
  
  # cleanup
  on.exit({
    # reset original plotSPC options
    options(.aqp.plotSPC.args = .opArgs)
    
    # for R CMD check
    aqp.env <- NULL
  })
  
}

