## TODO 
# * generalize to other gemorphic summaries
# * arguments to plotProfileDendrogram
# * encode Shannon entropy: values are computed row-wise, data plotted as columns


#' @title Present a `SoilProfileCollection` aligned to a geomorphic summary as cross-section.
#'
#' @param x resulting list from `soilDB::fetchOSD(..., extended = TRUE)`
#' @param type character, 'line' for line plot or 'bar' for barplot of geomorphic proportions
#' @param which character, select a geomorphic summary. Currently 'hillpos' (2D hillslope position) is the only supported choice.
#' @param col character vector of colors
#' 
#' @author D.E. Beaudette
#' @export
#'
plotGeomorphCrossSection <- function(x, type = c('line', 'bar'), which = 'hillpos', col = c("#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#E41A1C")) {
  
  # satisfy R CMD check
  series <- NULL
  
  # sanity checks
  type <- match.arg(type)
  which <- match.arg(which)
  
  # reconcile possible missing IDs
  o <- reconcileOSDGeomorph(x, 'hillpos')
  
  # perform clustering, ignore figure
  res <- vizHillslopePosition(o$geom)
  
  # the latest soilDB::fetchOSD() will automatically encode horizon distinctness offset
  # backwards compatibility
  if(is.null(x$SPC$hzd)) {
    
    # convert horizon boundary distinctness -> vertical distance
    x$SPC$hzd <- hzDistinctnessCodeToOffset(
      x$SPC$distinctness, 
      codes = c('very abrupt', 'abrubt', 'clear', 'gradual', 'diffuse')
    )
  }
  
  
  # re-order geomorphic proportions according to clustering
  hp <- o$geom[res$order, ]
  nm <- names(hp[, 2:6])
  
  if(type == 'line') {
    
    # setup figure panes
    layout(
      matrix(c(1,2)), 
      widths = c(1,1), 
      heights = c(2,1)
    )
    
    # dendrogram + profiles
    plotProfileDendrogram(
      o$SPC, 
      clust = res$clust, 
      dend.y.scale = 3, 
      scaling.factor = 0.012, 
      y.offset = 0.2, 
      width = 0.32, 
      name.style = 'center-center', 
      cex.names = 0.7, 
      shrink = TRUE, 
      cex.id = 0.55,
      hz.distinctness.offset = 'hzd'
    )
    
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
      xlim = c(0.5, length(o$SPC) + 1)
    )
    
    # proportion axis
    axis(side = 4, line = -1, las = 1, cex.axis = 0.7, col.axis = par('fg'))
    
    # legend
    legend('topleft', legend = rev(nm), col = rev(col), pch = 16, bty = 'n', cex = 0.8, pt.cex = 2, horiz = TRUE, inset = c(0.01, 0.01))
    mtext('Probability', side = 2, line = -2, font = 2)  
  }
  
  if(type == 'bar') {
    
    # setup figure panes
    layout(
      matrix(c(1,2)), 
      widths = c(1,1), 
      heights = c(2,1)
    )
    
    # dendrogram + profiles
    plotProfileDendrogram(
      o$SPC, 
      clust = res$clust, 
      dend.y.scale = 3, 
      scaling.factor = 0.012, 
      y.offset = 0.2, 
      width = 0.32, 
      name.style = 'center-center', 
      cex.names = 0.7, 
      shrink = TRUE, 
      cex.id = 0.55, 
      hz.distinctness.offset = 'hzd'
    )
    
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
      xlim = c(0.5, length(o$SPC) + 1), 
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
    mtext('Probability', side = 2, line = -2, font = 2)
    
  }
  
  
}

