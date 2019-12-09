## TODO: return clustering object instead of cluster$order
## TODO: provide examples for adjusting legend size / spacing

## this doesn't make much sense...
vizGeomorphicComponent <- function(x, s=NULL) {
  
  # check for required packages
  if(!requireNamespace('dendextend', quietly=TRUE))
    stop('please install the `dendextend` packages', call.=FALSE)
  
  # CRAN CHECK hack
  geomcomp <- NULL
  
  # save row names as they are lost in the distance matrix calc
  row.names(x) <- x$series
  
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
  
  ## TODO: is this the right place to set trellis options?
  # musym are re-ordered according to clustering
  trellis.par.set(tps)
  
  pp <- barchart(series ~ value, groups=geomcomp, data=x.long, horiz=TRUE, stack=TRUE, xlab='Proportion', 
                 scales=list(cex=1), 
                 key=simpleKey(space='top', columns=6, text=levels(x.long$geomcomp), rectangles = TRUE, points=FALSE, between.columns=1, between=0.5, cex=0.75), 
                 legend=list(right=list(fun=dendrogramGrob, args=list(x = as.dendrogram(x.d.hydro), side="right", size=10))),
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

