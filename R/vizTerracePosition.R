## TODO: provide examples for adjusting legend size / spacing

#' @title Visual Summary of Terraced Landform Positions
#' 
#' @description A unique display of terraced landform position probability.
#' 
#' @param x \code{data.frame} as created by \code{soilDB::fetchOSD(..., extended=TRUE)}, see details
#' 
#' @param s an optional soil series name, highlighted in the figure
#' 
#' @param annotations logical, add number of record and normalized Shannon entropy values
#' 
#' @param annotation.cex annotation label scaling factor
#' 
#' @param cols vector of colors
#' 
#' @param \dots additional arguments to `[iterateHydOrder]`: `target = 0.9, maxIter = 20, j.amount = 0.05, verbose = FALSE`
#' 
#' @return
#' A `list` with the following elements:
#'    * `fig`: lattice object (the figure)
#'    * `order`: 1D ordering from `cluster::diana`
#'    * `clust`: `hclust` object
#'    * `score`: scoring of hydrologic ordering of dendrogram 
#' 
#' @details See the \href{http://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.html}{Soil Series Query Functions} tutorial for more information.
#' 
#' @author D.E. Beaudette
#' 
#' 
vizTerracePosition <- function(x, s = NULL, annotations = TRUE, annotation.cex = 0.75, cols = c("#2B83BA", "#FDAE61"), ...) {
  
  # sanity checks on input
  if(!inherits(x, 'data.frame')) {
    stop('x must be a data.frame', call. = FALSE)
  }
  
  if(nrow(x) < 1) {
    stop('x must contain at least 1 row of data', call. = FALSE)
  }
  
  # check for required packages
  if(!requireNamespace('dendextend', quietly=TRUE) | !requireNamespace('latticeExtra', quietly=TRUE))
    stop('please install the `dendextend` and `latticeExtra` packages', call.=FALSE)
  
  # CRAN CHECK hack
  terrace_position <- NULL
  
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
  names(x.long)[2] <- 'terrace_position'
  
  # plot style
  tps <- list(superpose.polygon = list(col = cols, lwd = 2, lend = 2))
  
  ## all of the fancy ordering + dendrogram require > 1 series
  if(n.series > 1) {
    
    # iteratively apply hydrologic ordering, 
    .res <- iterateHydOrder(x, g = 'terrace', ...)
    x.d.hydro <- .res$clust
    .hydScore <- .res$score
    
    # re-order labels levels based on clustering
    x.long$series <- factor(x.long$series, levels = x$series[x.d.hydro$order])
    
    # dendrogram synced to bars
    leg <- list(
      right = list(
        fun = latticeExtra::dendrogramGrob,
        args = list(
          x = as.dendrogram(x.d.hydro), 
          side = "right", 
          size = 10)
      )
    )
    
  } else {
    # singleton
    x.long$series <- factor(x.long$series) 
    
    # no dendrogram legend
    leg <- list()
    
    # simulate output from clustering
    x.d.hydro <- list(order = 1L)
    .hydScore <- NA
  }
  
  # hack to ensure that simpleKey works as expected
  suppressWarnings(trellis.par.set(tps))
  
  # must manually create a key, for some reason auto.key doesn't work with fancy dendrogram
  sk <- simpleKey(space='top', columns=2, text=levels(x.long$terrace_position), rectangles = TRUE, points=FALSE, between.columns=2, between=1, cex=0.75)
  
  
  # a single series can be highlighted via argument 's'
  ## TODO: check for missing / mis-specified series names
  pp <- barchart(series ~ value, groups=terrace_position, data=x.long, horiz=TRUE, stack=TRUE, 
                 xlab='Proportion', 
                 scales=list(cex=1), 
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
  
  # re-pack results
  res <- list(
    fig = pp, 
    order = x.d.hydro$order, 
    clust = x.d.hydro, 
    score = .hydScore
  ) 
  
  return(res)
}

