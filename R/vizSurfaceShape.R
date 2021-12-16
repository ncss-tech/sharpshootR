## TODO: provide examples for adjusting legend size / spacing

#' @title Visual Summary of Surface Shape
#' 
#' @description A unique display of surface shape (typically curvature) probability, suitable for across-slope or down-slope shape. Use the `title` argument to make this clear.
#' 
#' @param x \code{data.frame} as created by \code{soilDB::fetchOSD(..., extended=TRUE)}, see details
#' 
#' @param s an optional soil series name, highlighted in the figure
#' 
#' @param title a reasonable title for the figure
#' 
#' @param annotations logical, add number of record and normalized Shannon entropy values
#' 
#' @param annotation.cex annotation label scaling factor
#' 
#' @param cols vector of colors
#' 
#' 
#' @return
#' A `list` with the following elements:
#'    * `fig`: lattice object (the figure)
#'    * `order`: 1D ordering from `cluster::diana`
#'    * `clust`: clustering object returned by `cluster::diana`
#' 
#' @details See the \href{http://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.html}{Soil Series Query Functions} tutorial for more information.
#' 
#' @author D.E. Beaudette
#' 
#' @examples 
#' 
#' \donttest{
#' if(requireNamespace("curl") &
#'    curl::has_internet() &
#'    require(aqp) & 
#'    require(soilDB)) {
#'   
#'   # soils of interest
#'   s.list <- c('musick', 'cecil', 'drummer', 'amador', 'pentz', 'reiff', 
#'               'san joaquin','montpellier','grangeville','pollasky','ramona')
#'   
#'   # fetch and convert data into an SPC
#'   s <- fetchOSD(s.list, extended=TRUE)
#'   
#'   res <- vizSurfaceShape(s$shape_across, title = 'Surface Shape (Across)')
#'   print(res$fig)
#'   
#' }
#' }
#' 
vizSurfaceShape <- function(x, title = 'Surface Shape', s = NULL, annotations = TRUE, annotation.cex = 0.75, cols = c("#2B83BA", "#FFFFBF", "#D7191C", "#808080", "darkgreen")) {
  
  # check for required packages
  if(!requireNamespace('dendextend', quietly=TRUE) | !requireNamespace('latticeExtra', quietly=TRUE))
    stop('please install the `dendextend` and `latticeExtra` packages', call.=FALSE)
  
  # CRAN CHECK hack
  surface_shape <- NULL
  
  # save row names as they are lost in the distance matrix calc
  row.names(x) <- x$series
  
  # save number of records
  n.records <- x$n
  
  # number of series
  n.series <- nrow(x)
  
  # save normalized Shannon entropy
  H <- x$shannon_entropy
  
  # mask-out some columns we don't need
  x$n <- NULL
  x$shannon_entropy <- NULL
  
  ## convert proportions to long format for plotting
  x.long <- melt(x, id.vars = 'series')
  # fix names: second column contains labels
  names(x.long)[2] <- 'surface_shape'
  
  # make some colors, and set style
  # cols <- rev(brewer.pal(5, 'Spectral'))
  # cols <- c(cols[c(1, 3, 5)], grey(0.5), 'darkgreen')
  # colorspace::swatchplot(cols)
  
  # setup plot styling
  tps <- list(superpose.polygon = list(col = cols, lwd = 2, lend = 2))
  
  ## all of the fancy ordering + dendrogram require > 1 series
  if(n.series > 1) {
    # re-order labels based on sorting of proportions: "hydrologic" ordering
    hyd.order <- order(rowSums(sweep(x[, -1], 2, STATS = c(-4, 1, 4, 5, 6), FUN = '*')), decreasing = TRUE)
    
    # cluster proportions: results are not in "hydrologic" order, but close
    x.d <- as.hclust(diana(daisy(x[, -1])))
    
    # rotate clustering according to hydrologic ordering
    x.d.hydro <- dendextend::rotate(x.d, order = x$series[hyd.order]) # dendextend approach
    
    # re-order labels levels based on clustering
    x.long$series <- factor(x.long$series, levels = x.long$series[x.d.hydro$order])
    
    # dendrogram synced to bars
    leg <- list(
      right = list(
        fun = latticeExtra::dendrogramGrob,
        args = list(
          x = as.dendrogram(x.d.hydro), 
          side="right", 
          size=10)
      )
    )
    
  } else {
    # singleton
    x.long$series <- factor(x.long$series) 
    
    # no dendrogram legend
    leg <- list()
    
    # simulate output from clustering
    x.d.hydro <- list(order = 1L)
  }
  
  
  # hack to ensure that simpleKey works as expected
  suppressWarnings(trellis.par.set(tps))
  
  # must manually create a key, for some reason auto.key doesn't work with fancy dendrogram
  sk <- simpleKey(space='top', columns=5, text=levels(x.long$surface_shape), rectangles = TRUE, points=FALSE, between.columns=2, between=1, cex=0.75)
  
  
  
  # a single series can be highlighted via argument 's'
  ## TODO: check for missing / mis-specified series names
  pp <- barchart(series ~ value, groups = surface_shape, data=x.long, horiz=TRUE, stack=TRUE, 
                 xlab = 'Proportion', 
                 main = title,
                 scales = list(cex=1), 
                 key = sk,
                 legend = leg, 
                 panel = function(...) {
                   panel.barchart(...)
                   
                   if(annotations) {
                     # annotation coords
                     x.pos.N <- unit(0.03, 'npc')
                     x.pos.H <- unit(0.97, 'npc')
                     y.pos <- unit((1:nrow(x)) - 0.25, 'native')
                     y.pos.annotation <- unit(nrow(x) + 0.25, 'native')
                     
                     # annotate with number of records
                     grid.text(
                       as.character(n.records[x.d.hydro$order]), 
                       x = x.pos.N, 
                       y = y.pos,
                       gp = gpar(cex = annotation.cex, font = 1)
                     )
                     
                     # annotate with H
                     grid.text(
                       as.character(round(H[x.d.hydro$order], 2)), 
                       x = x.pos.H, 
                       y = y.pos,
                       gp = gpar(cex = annotation.cex, font = 3)
                     )
                     
                     # annotation labels
                     grid.text(
                       c('N', 'H'), 
                       x = c(x.pos.N, x.pos.H), 
                       y = y.pos.annotation,
                       gp = gpar(cex = annotation.cex, font = c(2, 4))
                     )  
                   }
                   
                 },
                 yscale.components=function(..., s.to.bold=s) {
                   temp <- yscale.components.default(...) 
                   
                   if(!is.null(s.to.bold)) {
                     temp$left$labels$labels <-   
                       sapply( temp$left$labels$labels, 
                               function(x) {
                                 if(grepl(s.to.bold, x, ignore.case = TRUE)) { 
                                   as.expression(bquote( bold(.(x)))) 
                                 } else { 
                                   as.expression(bquote(.(x)))
                                 }
                               }
                       )  
                   }
                   
                   return(temp)
                 })
  
  # embed styling
  pp <- update(pp, par.settings = tps)
  
  # figure, ordering, clustering object
  res <- list(fig = pp, order = x.d.hydro$order, clust = x.d.hydro)
  return(res)
}

