## TODO: return clustering object instead of cluster$order
## TODO: provide examples for adjusting legend size / spacing

#' @title Visual Summary of Hill Landform Positions
#' 
#' @description A unique display of landform position probability.
#' 
#' @param x \code{data.frame} as created by \code{soilDB::fetchOSD(..., extended=TRUE)}, see details
#' 
#' @param s an optional soil series name, highlighted in the figure
#' 
#' @param annotations logical, add number of record and normalized Shannon entropy values
#' 
#' @param annotation.cex annotation label scaling factor
#' 
#' @return a \code{list} with the following elements:
#' 
#' \item{fig}{lattice object (the figure)}
#' \item{order}{ordering of soil series}
#' 
#' @details See the \href{http://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.html}{Soil Series Query Functions} tutorial for more information.
#' 
#' @author D.E. Beaudette
#' 
#' 
vizGeomorphicComponent <- function(x, s=NULL, annotations = TRUE, annotation.cex = 0.75) {
  
  # check for required packages
  if(!requireNamespace('dendextend', quietly=TRUE) | !requireNamespace('latticeExtra', quietly=TRUE))
    stop('please install the `dendextend` and `latticeExtra` packages', call.=FALSE)
  
  # CRAN CHECK hack
  geomcomp <- NULL
  
  # save row names as they are lost in the distance matrix calc
  row.names(x) <- x$series
  
  # save number of records
  n.records <- x$n
  
  # save normalized Shannon entropy
  H <- x$shannon_entropy
  
  # mask-out some columns we don't need
  x$n <- NULL
  x$shannon_entropy <- NULL
  
  ## convert proportions to long format for plotting
  x.long <- melt(x, id.vars = 'series')
  # fix names: second column contains labels
  names(x.long)[2] <- 'geomcomp'
  
  # make some colors, and set style
  cols <- brewer.pal(6, 'Spectral')
  tps <- list(superpose.polygon=list(col=cols, lwd=2, lend=2))
  
  # re-order labels based on sorting of proportions: "hydrologic" ordering
  hyd.order <- order(rowSums(sweep(x[, -1], 2, STATS=c(4, 2, 1, 1, -2, -4), FUN = '*')), decreasing = TRUE)
  
  # cluster proportions: results are not in "hydrologic" order, but close
  x.d <- as.hclust(diana(daisy(x[, -1])))
  
  # rotate clustering according to hydrologic ordering
  x.d.hydro <- dendextend::rotate(x.d, order = x$series[hyd.order]) # dendextend approach
  
  # re-order labels levels based on clustering
  x.long$series <- factor(x.long$series, levels=x.long$series[x.d.hydro$order])
  
  # hack to ensure that simpleKey works as expected
  suppressWarnings(trellis.par.set(tps))
  
  # must manually create a key, for some reason auto.key doesn't work with fancy dendrogram
  sk <- simpleKey(space='top', columns=6, text=levels(x.long$geomcomp), rectangles = TRUE, points=FALSE, between.columns=1, between=1, cex=0.75)
  
  leg <- list(right=list(fun=latticeExtra::dendrogramGrob, args=list(x = as.dendrogram(x.d.hydro), side="right", size=10)))
  
  pp <- barchart(series ~ value, groups=geomcomp, data=x.long, horiz=TRUE, stack=TRUE, xlab='Proportion',
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
  
  # the figure and ordering are returned
  return(list(fig=pp, order=x.d.hydro$order))
}

