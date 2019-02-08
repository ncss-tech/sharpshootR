


# climate.data: quantiles of annual climate, as returned by fetchOSD(..., extended=TRUE)
# s: series name
# IQR.width: width of IQR band
vizAnnualClimate <- function(climate.data, s=NULL, IQR.width=0.025) {
  
  ## TODO:
  # automatically adjust IQR width based on number of series
  # automatically adjust median cex
  # label series
  
  # widen for clustering of medians
  climate.data.wide <- dcast(climate.data, series ~ climate_var, value.var = 'q50')
  # save row names for labeling later
  row.names(climate.data.wide) <- climate.data.wide$series
  
  # re-order based on divisive hierarchical clustering of medians
  # keep cluster pkg class for dendrogram + profile sketches
  climate.data.wide.d <- diana(daisy(climate.data.wide[, -1], stand = TRUE))
  
  # convert to hclust class for ordering of series in climate summaries
  climate.data.wide.h <- as.hclust(climate.data.wide.d)
  climate.data$series <- factor(climate.data$series, levels=climate.data.wide.h$labels[climate.data.wide.h$order])
  
  # used for horizontal lines in figure
  n.series <- length(levels(climate.data$series))
  
  # save for later
  pp <- segplot(series ~ q05 + q95 | factor(climate_var), 
                centers=q50, data=climate.data, 
                main='Annual Climate Summary', 
                draw.bands=FALSE, segments.fun=panel.arrows, ends='both', angle=90, length=1, unit='mm', 
                scales=list(y=list(alternating=3), x=list(relation='free')), 
                as.table=TRUE, col='RoyalBlue', 
                strip=strip.custom(bg=grey(0.85), par.strip.text=list(cex=0.7)), 
                xlab='5th-25th-50th-75th-95th Percentiles', 
                panel=function(x, y, z, q25=climate.data$q25, q75=climate.data$q75, subscripts, ...) {
                  # basic plot
                  panel.grid(h=FALSE, v=-1, col='grey', lty=3)
                  panel.abline(h=1:n.series, col='grey', lty=3)
                  panel.segplot(x, y, z, subscripts=subscripts, ...)
                  
                  # add interquartile range
                  q25 <- q25[subscripts]
                  q75 <- q75[subscripts]
                  zz <- z[subscripts]
                  panel.rect(xleft=q25, xright=q75, ybottom=as.numeric(zz) - IQR.width, ytop=as.numeric(zz) + IQR.width, border='RoyalBlue', col='RoyalBlue')
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
  
  
  # return figure, and clustering results
  return(list(fig=pp, clust=x.wide.d))
  
}