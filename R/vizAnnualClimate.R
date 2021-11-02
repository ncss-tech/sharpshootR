


# climate.data: quantiles of annual climate, as returned by fetchOSD(..., extended=TRUE)
# s: series name
# IQR.width: width of IQR band
# ...: further arguments to segplot

#' @title Annual Climate Summaries for Soil Series Data
#' 
#' @description Annual climate summaries for soil series, based on `latticeExtra::segplot`, based on 5th, 25th, 50th, 75th, and 95th percentiles. Input data should be from `soilDB::fetchOSD`.
#'
#' @param climate.data Annual climate summaries, as returned from `soilDB::fetchOSD(..., extended=TRUE)`
#' @param IQR.cex scaling factor for bar representing interquartile range
#' @param s a soil series name, e.g. "LUCY", to highlight
#' @param s.col color for highlighted soil series
#' @param ... further arguments passed to `latticeExtra::segplot`
#' 
#' @details This function was designed for use with `soilDB::fetchOSD`. It might be possible to use with other sources of data but your mileage may vary. See the \href{http://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.html}{Soil Series Query Functions} tutorial for more information.
#' 
#' @author D.E. Beaudette
#' 
#' @seealso `vizHillslopePosition`
#' 
#' @return
#' 
#' A `list` with the following elements:
#'    * `fig`: lattice object (the figure)
#'    * `clust`: clustering object returned by `cluster::diana`
#' 
#' 
#' @export
#'
#' @examples
#' 
#' \donttest{
#' if(requireNamespace("curl") &
#' curl::has_internet() &
#'   require(soilDB) & 
#'   require(aqp) & 
#'   require(latticeExtra)
#' ) {
#'   
#'   # soil series of interest
#'   soil <- 'ARBUCKLE'
#'   
#'   # get competing series
#'   sdata <- fetchOSD(soil, extended = TRUE)
#'   
#'   # get competing series' data
#'   sdata.competing <- fetchOSD(c(soil, sdata$competing$competing))
#'   
#'   # only use established series
#'   idx <- which(sdata.competing$series_status == 'established')
#'   
#'   # subset as needed
#'   if(length(idx) < length(sdata.competing)) {
#'     sdata.competing <- sdata.competing[idx, ]
#'     
#'   }
#'   
#'   # now get the extended data
#'   sdata.competing.full <- fetchOSD(site(sdata.competing)$id, extended = TRUE)
#'   
#'   # extract SPC
#'   spc <- sdata.competing.full$SPC
#'   
#'   # full set of series names
#'   s.names <- unique(site(spc)$id)
#'   
#'   # todo: probably better ways to do this...
#'   # note: need to load lattice for this to work
#'   trellis.par.set(plot.line=list(col='RoyalBlue'))
#'   
#'   # control center symbol and size here
#'   res <- vizAnnualClimate(
#'   sdata.competing.full$climate.annual, 
#'   s = soil, 
#'   IQR.cex = 1.1, 
#'   cex = 1.1, 
#'   pch = 18
#'   )
#'   
#'   # plot figure
#'   print(res$fig)
#'   
#'   # check clustering
#'   str(res$clust)
#'   
#'   # do something with clustering
#'   op <- par(no.readonly = TRUE)
#'   
#'   par(mar=c(0,0,1,1))
#'   plotProfileDendrogram(spc, clust = res$clust, scaling.factor = 0.075, width = 0.2, y.offset = 0.5)
#'   mtext('sorted by annual climate summaries', side = 3, at = 0.5, adj = 0, line = -1.5, font=3)
#'   
#'   par(op)
#'   
#' }
#' 
#' }
#' 
vizAnnualClimate <- function(climate.data, IQR.cex = 1, s = NULL, s.col = 'firebrick', ...) {
  
  # check for required packages
  if(!requireNamespace('latticeExtra', quietly=TRUE))
    stop('please install the `latticeExtra` package', call.=FALSE)
  
  
  # for R CMD check
  q50 <- NULL
  
  # get extra arguments: length of 0 if no extra arguments
  extra.args <- list(...)
  
  # widen for clustering of medians
  climate.data.wide <- dcast(climate.data, series ~ climate_var, value.var = 'q50')
  # save row names for labeling later
  row.names(climate.data.wide) <- climate.data.wide$series
  
  # clustering is only possible with >1 series
  if(nrow(climate.data.wide) > 1) {
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
  } else {
    # modify / construct objects that would have been created by clustering / ordering
    climate.data$series <- factor(climate.data$series)
    climate.data.wide.d <- NULL
    climate.data.wide.h <- list(labels = climate.data.wide$series)
    n.series <- 1
  }
  
  
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
  pp <- latticeExtra::segplot(series ~ q05 + q95 | factor(climate_var),
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
                  latticeExtra::panel.segplot(x, y, z, subscripts=subscripts, ...)
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
  res <- list(fig = pp, clust = climate.data.wide.d)
  return(res)
  
}

