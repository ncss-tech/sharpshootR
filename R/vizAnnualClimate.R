


# climate.data: quantiles of annual climate, as returned by fetchOSD(..., extended=TRUE)
# s: series name
# IQR.width: width of IQR band
# ...: further arguments to segplot
vizAnnualClimate <- function(climate.data, IQR.cex=1, s=NULL, s.col='firebrick', ...) {
  
  # get extra arguments: length of 0 if no extra arguments
  extra.args <- list(...)
  
  # widen for clustering of medians
  climate.data.wide <- dcast(climate.data, series ~ climate_var, value.var = 'q50')
  # save row names for labeling later
  row.names(climate.data.wide) <- climate.data.wide$series
  
  # re-order based on divisive hierarchical clustering of medians
  # keep cluster pkg class for dendrogram + profile sketches
  # suppressing warnings due to cases where daisy reports: 
  # "binary variable(s) 4 treated as interval scaled"
  climate.data.wide.d <- suppressWarnings(diana(daisy(climate.data.wide[, -1], stand = TRUE)))
  
  # convert to hclust class for ordering of series in climate summaries
  climate.data.wide.h <- as.hclust(climate.data.wide.d)
  climate.data$series <- factor(climate.data$series, levels=climate.data.wide.h$labels[climate.data.wide.h$order])
  
  # used for horizontal lines in figure
  n.series <- length(levels(climate.data$series))
  
  # get current settings
  tps <- trellis.par.get()
  
  ## TODO: figure this logic out
  ## 
  ## essentially: use the color specified as an argument if present, otherwise use bette defaults than standard lattice
  ##
  # # override some trellis defaults if not specified
  # if(! is.null(extra.args$col)) {
  #   tps$plot.line$col <- extra.args$col
  # }
  
  # get color vector from current trellis settings
  tps.cols <- tps$plot.line$col
  
  # attempt to highlight named series by lowering chroma of other series
  if(! is.null(s)) {
    
    ### !!! whoa, the color vector has to match the original label order, NOT the new ordering -- WTF !!!
    # index to the series of interest
    s.idx <- grep(s, x=climate.data.wide.h$labels, ignore.case = TRUE)
    
    # expand color vector to the total number of series
    # this should account for multiple colors
    tps.cols <- rep_len(tps.cols, length.out = n.series)
    
    # replace with highlight color
    tps.cols[s.idx] <- s.col
  }


  # attempt to create an idea IQR width
  # generally, fewer series will require a smaller width
  # 0.05 is about right for ~ 10 series
  IQR.width <- IQR.cex * (n.series * 0.0025)
  
  # save for later
  pp <- segplot(series ~ q05 + q95 | factor(climate_var),
                centers=q50, data=climate.data, 
                main='Annual Climate Summary', 
                draw.bands=FALSE, segments.fun=panel.arrows, ends='both', angle=90, length=1, unit='mm', 
                scales=list(y=list(alternating=3), x=list(relation='free')), 
                as.table=TRUE,
                par.settings=list(plot.line=list(col=tps.cols)),
                strip=strip.custom(bg=grey(0.85), par.strip.text=list(cex=0.7)), 
                xlab='5th-25th-50th-75th-95th Percentiles', 
                panel=function(x, y, z, q25=climate.data$q25, q75=climate.data$q75, subscripts, ...) {
                  # grid and horizontal guides
                  panel.grid(h=FALSE, v=-1, col='grey', lty=3)
                  panel.abline(h=1:n.series, col='grey', lty=3)
                  
                  # IQR bars
                  q25 <- q25[subscripts]
                  q75 <- q75[subscripts]
                  zz <- z[subscripts]
                  
                  # !!! note: colors are in the original order of series labels !!!
                  panel.rect(xleft=q25, xright=q75, 
                             ybottom=as.numeric(zz) - IQR.width, 
                             ytop=as.numeric(zz) + IQR.width, 
                             border=tps.cols, col=tps.cols)
                  
                  # segplot
                  panel.segplot(x, y, z, subscripts=subscripts, ...)
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
  }, ... )
  
  
  # return figure, and clustering results
  return(list(fig=pp, clust=climate.data.wide.d))
  
}

