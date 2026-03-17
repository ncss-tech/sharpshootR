
# default colors:
# dput(c('grey', hcl.colors(n = 9, palette = 'spectral')))

# updated colors
# .cols <- c(grey(0.8), rev(PNWColors::pnw_palette('Bay', n = 9)))
# .cols <- colorspace::desaturate(.cols, amount = 0.2)
# dput(.cols)




#' @title Visual Summary of Hill Landform Positions
#' 
#' @description A unique display of landform position probability.
#' 
#' @details See the \href{http://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.html}{Soil Series Query Functions} tutorial for more information.
#' 
#' @note Default colors are from `PNWColors::pnw_palette('Bay')`.
#' 
#' 
#' @param x `data.frame` as created by `soilDB::fetchOSD(..., extended = TRUE)`, see details
#' 
#' @param s an optional soil series name, highlighted in the figure
#' 
#' @param annotations logical, add number of record and normalized Shannon entropy values
#' 
#' @param annotation.cex annotation label scaling factor
#' 
#' @param clust logical, order rows using divisive hierarchical clustering and include dendrogram?
#' 
#' @param dend.size numeric, space reserved for dendrogram when `clust = TRUE`: values between 3-10 are about right
#' 
#' @param dend.type character, one of 'rectangle' or 'triangle', when `clust = TRUE`
#' 
#' @param cols vector of colors
#' 
#' @param \dots additional arguments to [iterateHydOrder()]: `target = 0.9, maxIter = 20, j.amount = 0.001, verbose = FALSE`
#' 
#' @return
#' A `list` with the following elements:
#'    * `fig`: lattice object (the figure)
#'    * `order`: 1D ordering from `cluster::diana`
#'    * `clust`: `hclust` object
#'    * `match.rate`: fraction of series matching target hydrologic ordering, after clustering + rotation
#' 
#' 
#' @author D.E. Beaudette
#' 
#' 
vizGeomorphons <- function(x, s = NULL, annotations = TRUE, annotation.cex = 0.75, clust = TRUE, dend.size = 5, dend.type = c('rectangle', 'triangle'), cols = c("#CCCCCC", "#CF4F3F", "#D86D40", "#E29048", "#E5B35A", "#E9D772", "#86AC7D", "#3D8399", "#2E657F", "#1F4867"), ...) {
  
  # sanity check
  dend.type <- match.arg(dend.type)
  
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
  geomorphons <- NULL
  
  # save row names as they are lost in the distance matrix calc
  row.names(x) <- x$series
  
  # save number of records
  # n.records <- x$n
  
  # number of series
  n.series <- nrow(x)
  
  # save normalized Shannon entropy
  H <- x$shannon_entropy
  
  # mask-out some columns we don't need
  # x$n <- NULL
  x$shannon_entropy <- NULL
  
  ## convert proportions to long format for plotting
  x.long <- melt(x, id.vars = 'series')
  # fix names: second column contains labels
  names(x.long)[2] <- 'geomorphons'
  
  # make some colors, and set style
  tps <- list(superpose.polygon = list(col = cols, lwd = 0.5, lend = 2))
  
  ## all of the fancy ordering + dendrogram require > 1 series
  if(n.series > 1) {
    
    if(clust) {
      # iteratively apply hydrologic ordering, 
      .res <- iterateHydOrder(x, g = 'geomorphons', ...)
      x.d.hydro <- .res$clust
      .match.rate <- .res$match.rate
      
      # re-order labels levels based on clustering
      x.long$series <- factor(x.long$series, levels = x$series[x.d.hydro$order])
      
      # dendrogram synced to bars
      leg <- list(
        right = list(
          fun = latticeExtra::dendrogramGrob,
          args = list(
            x = as.dendrogram(x.d.hydro), 
            side = "right", 
            size = dend.size, 
            type = dend.type
          )
        )
      )
    } else {
      # apply hydrologic ordering, 
      .res <- hydOrder(x, g = 'geomorphons', clust = FALSE)
      
      # re-order labels levels based on clustering
      x.long$series <- factor(x.long$series, levels = .res)
      
      # convert hydrologic ordering of series names -> integer order for figure annotation
      x.d.hydro <- list(order = match(.res, x$series))
      .match.rate <- NA
      
      # no dendrogram legend
      leg <- list()
    }
    
    
    
  } else {
    # singleton
    x.long$series <- factor(x.long$series) 
    
    # no dendrogram legend
    leg <- list()
    
    # simulate output from clustering
    x.d.hydro <- list(order = 1L)
    .match.rate <- NA
  }
  
  # hack to ensure that simpleKey works as expected
  suppressWarnings(trellis.par.set(tps))
  
  # must manually create a key, for some reason auto.key doesn't work with fancy dendrogram
  sk <- simpleKey(space = 'top', columns = 5, text = levels(x.long$geomorphons), rectangles = TRUE, points = FALSE, between.columns = 1, between = 1, cex = 0.75)
  
  
  
  pp <- barchart(series ~ value, 
                 groups = geomorphons, 
                 data = x.long, 
                 horiz = TRUE, 
                 stack = TRUE, 
                 xlab='Proportion',
                 scales = list(cex=1), 
                 key = sk, 
                 legend = leg,
                 panel = function(...) {
                   panel.barchart(...)
                   
                   if(annotations) {
                     # annotation coords
                     # x.pos.N <- unit(0.03, 'npc')
                     x.pos.H <- unit(0.97, 'npc')
                     y.pos <- unit((1:nrow(x)) - 0.25, 'native')
                     y.pos.annotation <- unit(nrow(x) + 0.25, 'native')
                     
                     # # annotate with number of records
                     # grid.text(
                     #   as.character(n.records[x.d.hydro$order]), 
                     #   x = x.pos.N, 
                     #   y = y.pos,
                     #   gp = gpar(cex = annotation.cex, font = 1)
                     # )
                     
                     # annotate with H
                     grid.text(
                       as.character(round(H[x.d.hydro$order], 2)), 
                       x = x.pos.H, 
                       y = y.pos,
                       gp = gpar(cex = annotation.cex, font = 1)
                     )
                     
                     # annotation labels
                     grid.text(
                       c('H'), 
                       x = c(x.pos.H), 
                       y = y.pos.annotation,
                       gp = gpar(cex = annotation.cex, font = 2)
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
    match.rate = .match.rate
  ) 
  
  return(res)
}

