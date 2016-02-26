
## TODO: move more hard-coded geom elements to arguments / heuristics
## NOTE: distance matrix must be scaled to approximately {0,1}
plotProfileDendrogram <- function(x, d, scaling.factor=0.01, width=0.1, y.offset=0.1, dend.y.scale=2, ...) {
  # convert to phylo class
  d.hclust <- as.hclust(diana(d))
  dend <- as.phylo(d.hclust)
  
  # setup plot and add dendrogram
  plot(dend, cex=0.8, direction='up', y.lim=c(dend.y.scale, 0), x.lim=c(0.5, length(x)+1), show.tip.label=FALSE)
  
  # get the last plot geometry
  lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  
  # vector of indices for plotting soil profiles below leaves of dendrogram
  new_order <- d.hclust$order
  
  # plot the profiles, in the ordering defined by the dendrogram
  # with a couple fudge factors to make them fit
  plot(x, plot.order=new_order, add=TRUE, width=width, scaling.factor=scaling.factor, y.offset=max(lastPP$yy) + y.offset, ...)
}

