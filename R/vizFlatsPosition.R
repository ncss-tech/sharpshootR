## TODO: return clustering object instead of cluster$order
## TODO: provide examples for adjusting legend size / spacing

# still testing "hydrologic" order
vizFlatsPosition <- function(x, s=NULL) {
  
  # check for required packages
  if(!requireNamespace('dendextend', quietly=TRUE) | !requireNamespace('latticeExtra', quietly=TRUE))
    stop('please install the `dendextend` and `latticeExtra` packages', call.=FALSE)
  
  # CRAN CHECK hack
  flats_position <- NULL
  
  # save row names as they are lost in the distance matrix calc
  row.names(x) <- x$series
  
  # save number of records
  n.records <- x$n
  
  # mask-out some columns we don't need
  x$n <- NULL
  x$shannon_entropy <- NULL
  
  ## convert proportions to long format for plotting
  x.long <- melt(x, id.vars = 'series')
  # fix names: second column contains labels
  names(x.long)[2] <- 'flats_position'
  
  # make some colors, and set style
  cols <- rev(brewer.pal(5, 'Spectral'))
  
  # plot style
  tps <- list(superpose.polygon=list(col=cols, lwd=2, lend=2))
  
  # re-order labels based on sorting of proportions: "hydrologic" ordering
  hyd.order <- order(rowSums(sweep(x[, -1], 2, STATS=c(-2, 0, 0, 2), FUN = '*')), decreasing = TRUE)
  
  # cluster proportions: results are not in "hydrologic" order, but close
  x.d <- as.hclust(diana(daisy(x[, -1])))
  # x.d <- as.hclust(agnes(daisy(x[, -1]), method = 'gaverage'))
  
  # rotate clustering according to hydrologic ordering
  x.d.hydro <- dendextend::rotate(x.d, order = x$series[hyd.order]) # dendextend approach
  
  # re-order labels levels based on clustering
  x.long$series <- factor(x.long$series, levels=x.long$series[x.d.hydro$order])
  
  ## TODO: is this the right place to set trellis options?
  trellis.par.set(tps)
  
  sk <- simpleKey(space='top', columns=4, text=levels(x.long$flats_position), rectangles = TRUE, points=FALSE, between.columns=2, between=1, cex=0.75)
  
  leg <- list(right=list(fun=latticeExtra::dendrogramGrob, args=list(x = as.dendrogram(x.d.hydro), side="right", size=10)))
  
  # a single series can be highlighted via argument 's'
  ## TODO: check for missing / mis-specified series names
  pp <- barchart(series ~ value, groups=flats_position, data=x.long, horiz=TRUE, stack=TRUE, 
                 xlab='Proportion', 
                 scales=list(cex=1), 
                 key = sk, 
                 legend = leg,
                 panel = function(...) {
                   panel.barchart(...)
                   grid.text(
                     as.character(n.records[x.d.hydro$order]), 
                     x = unit(0.03, 'npc'), 
                     y = unit(1:nrow(x), 'native'),
                     gp = gpar(cex = 0.75))
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
  
  # the figure and ordering are returned
  return(list(fig=pp, order=x.d.hydro$order))
}

