
## TODO:
## * implement non-linear re-scaling of distances / variables
## * perform all re-scaling via linear model?
## * consider sorting by distance vs. var (requires a starting point)
## * consider manually defined vector of indices
## * see new function in aqp: alignTransect() for relative positions along x-axis

# function for computing gradient vs. distance along gradient
dist.along.grad <- function(coords, var, grad.order, grad.scaled.min, grad.scaled.max) {
  
  # order coordinates by variable
  coords <- coords[grad.order, ]
  
  # compute cumulative distance along gradient (horizontal units of CRS)
  grad.distances <- cumsum(c(0, sqrt(diff(coords[, 1])^2 + diff(coords[, 2])^2)))
  
  # rescale distances to number of profiles in collection
  scaled.distances <- scales::rescale(grad.distances, to=c(1, nrow(coords)))
  
  # rescale gradients to profile-scale
  # note that range is inverted because we are plotting depth as positive values
  scaled.grad <- scales::rescale(var[grad.order], to=c(grad.scaled.max, grad.scaled.min))
  
  # composite result
  res <- data.frame(
    scaled.grad=scaled.grad, 
    scaled.distance=scaled.distances, 
    distance=grad.distances, 
    variable=var[grad.order], 
    x=coords[grad.order, 1], 
    y=coords[grad.order, 2], 
    grad.order=grad.order
  )
  
  return(res)
}



#' @title Arrange Profiles along a Transect 
#'
#' @description Plot a collection of Soil Profiles linked to their position along some gradient (e.g. transect).
#'
#' @param s `SoilProfileCollection` object
#' 
#' @param grad.var.name the name of a site-level attribute containing gradient values
#' 
#' @param grad.var.order optional indexing vector used to override sorting along `grad.var.name`
#' 
#' @param transect.col color used to plot gradient (transect) values
#' 
#' @param tick.number number of desired ticks and labels on the gradient axis
#' 
#' @param y.offset vertical offset used to position profile sketches
#' 
#' @param scaling.factor scaling factor applied to profile sketches
#' 
#' @param distance.axis.title a title for the along-transect distances
#' 
#' @param crs an optional `CRS` object (sp package) used to convert coordinates into a projected coordinate reference system
#' 
#' @param grad.axis.title a title for the gradient axis 
#' 
#' @param dist.scaling.factor scaling factor (divisor) applied to linear distance units, default is conversion from m to km (1000)
#' @param spacing profile sketch spacing style: "regular" (profiles aligned to an integer grid) or "re
#' lative" (relative distance along transect)
#' 
#' @param fix.relative.pos adjust relative positions in the presence of overlap, `FALSE` to suppress, otherwise list of arguments to `aqp::fixOverlap`
#' 
#' @param ... further arguments passed to `aqp::plotSPC`.
#'
#' @details Depending on the nature of your `SoilProfileCollection` and associated gradient values, it may be necessary to tinker with figure margins, `y.offset` and `scaling.factor`.
#' 
#' @note This function is very much a work in progress, ideas welcome!
#'
#' @return An invisibly-returned `data.frame` object:
#'
#'   * scaled.grad: scaled gradient values
#'   * scaled.distance: cumulative distance, scaled to the interval of `0.5, nrow(coords) + 0.5`
#'   * distance: cumulative distance computed along gradient, e.g. transect distance
#'   * variable: sorted gradient values
#'   * x: x coordinates, ordered by gradient values
#'   * y: y coordinate, ordered by gradient values
#'   * grad.order: a vector index describing the sort order defined by gradient values
#'  
#' @author D.E. Beaudette   
#' 
#' @export
#'
#' @examples
#' 
#' \donttest{
#' 
#' if(require(aqp) & 
#' require(sp) &
#'   require(soilDB)
#' ) {
#'   
#'   # sample data
#'   data("mineralKing", package = "soilDB")
#'   
#'   # device options are modified locally, reset when done
#'   op <- par(no.readonly = TRUE)
#'   
#'   # quick overview
#'   par(mar=c(1,1,2,1))
#'   groupedProfilePlot(mineralKing, groups='taxonname', print.id=FALSE)
#'   
#'   # init coords and CRS
#'   coordinates(mineralKing) <- ~ x_std + y_std
#'   proj4string(mineralKing) <- '+proj=longlat +datum=NAD83'
#'   
#'   # projected CRS, units of meters
#'   crs.utm <- CRS('+proj=utm +zone=11 +datum=NAD83')
#'   
#'   # adjust margins
#'   par(mar=c(4.5,4,4,1))
#'   
#'   # standard transect plot, profile sketches arranged along integer sequence
#'   plotTransect(mineralKing, grad.var.name='elev_field', crs=crs.utm, 
#'                grad.axis.title='Elevation (m)', label='pedon_id', name='hzname')
#'   
#'   # default behavior, attempt adjustments to prevent over-plot and preserve relative spacing
#'   # use set.seed() to fix outcome
#'   plotTransect(mineralKing, grad.var.name='elev_field', crs=crs.utm, 
#'                grad.axis.title='Elevation (m)', label='pedon_id', 
#'                name='hzname', width=0.15, spacing = 'relative')
#'   
#'   # attempt relative positioning based on scaled distances, no corrections for overlap
#'   # profiles are clustered in space and therefore over-plot
#'   plotTransect(mineralKing, grad.var.name='elev_field', crs=crs.utm, 
#'                grad.axis.title='Elevation (m)', label='pedon_id', name='hzname', 
#'                width=0.15, spacing = 'relative', fix.relative.pos = FALSE)
#'   
#'   # customize arguments to aqp::fixOverlap()
#'   plotTransect(mineralKing, grad.var.name='elev_field', crs=crs.utm, 
#'                grad.axis.title='Elevation (m)', label='pedon_id', name='hzname', 
#'                width=0.15, spacing = 'relative', 
#'                fix.relative.pos = list(maxIter=6000, adj=0.2, thresh=0.7))
#'   
#'   plotTransect(mineralKing, grad.var.name='elev_field', crs=crs.utm, 
#'                grad.axis.title='Elevation (m)', label='pedon_id', name='hzname', 
#'                width=0.2, spacing = 'relative', 
#'                fix.relative.pos = list(maxIter=6000, adj=0.2, thresh=0.6),
#'                name.style = 'center-center')
#'                
#'  par(op)
#'   
#' }
#' }
#' 
#' 
plotTransect <- function(s, grad.var.name, grad.var.order = order(site(s)[[grad.var.name]]), transect.col = 'RoyalBlue', tick.number = 7, y.offset = 100, scaling.factor = 0.5, distance.axis.title = 'Distance Along Transect (km)', crs = NULL, grad.axis.title = NULL, dist.scaling.factor = 1000, spacing = c('regular', 'relative'), fix.relative.pos = list(thresh = 0.6, maxIter = 5000), ...){
  
  # sanity checks
  spacing <- match.arg(spacing)
  
  
  # internal offsets
  
  # optionally convert to planar CRS
  if(!missing(crs)) {
    # need rgdal
    if(!requireNamespace('rgdal'))
      stop('Transformation of coordinates requires the `rgdal` package.', call.=FALSE)
    
    # perform transformation and extract coordinates from SPC
    coords <- suppressMessages(coordinates(spTransform(as(s, 'SpatialPointsDataFrame'), crs)))
  }
  # extract coordinates from SPC without transformation, CRS must be planar
  else {
    
    # check for projected
    if(sp::is.projected(s@sp) == FALSE) {
      warning('coordinates must use a projected CRS, distances will be meaningless', call. = FALSE)
    }
    
    # use coordinates as-is
    coords <- coordinates(s)
  }
    
  
  # create transect
  transect <- dist.along.grad(
    coords = coords, 
    var = site(s)[[grad.var.name]], 
    grad.order = grad.var.order, 
    grad.scaled.min = 0, 
    grad.scaled.max = y.offset-15
    )
  
  # use a linear model to translate original gradient -> scaled gradient 
  l <- lm(scaled.grad ~ variable, data = transect)
  
  # establish gradient axis tick marks
  axis.labels <- pretty(transect$variable, n = tick.number)
  axis.pos <- round(predict(l, data.frame(variable=axis.labels)))
  
  # spacing style
  if(spacing == 'regular') {
    # this var will be used to position arrows / segments
    x.pos <- 1:length(s)
    # standard, regular spacing along x-asis
    plot(s, plot.order=transect$grad.order, y.offset=y.offset, scaling.factor=scaling.factor, id.style='side', ...)
    
  } else if(spacing == 'relative') {
    # this var will be used to position sketches, arrows, segments
    # note that it has already been sorted for the final ordering of sketches
    x.pos <- transect$scaled.distance
    
    # attempt to fix relative positions in the case of overlap
    if(is.list(fix.relative.pos)) {
      # add positions to the list of arguments, via `x` argument to fixOverlap()
      fix.relative.pos$x <- x.pos
      
      # force trace = FALSE so that results are a vector
      fix.relative.pos$trace <- FALSE
      
      # fun with arguments
      x.pos <- do.call(fixOverlap, fix.relative.pos)
    }
    
    # plot sketches according to relative spacing along x-axis
    plot(s, plot.order=transect$grad.order, y.offset=y.offset, scaling.factor=scaling.factor, id.style='side', relative.pos=x.pos, ...)
  } else {
    stop('spacing must be specified as `regular` or `relative`', call. = FALSE)
  }
  
  
  
  # add gridlines
  abline(h=axis.pos, lty=2, col='grey')
  
  # add gradient axis
  axis(side=2, at=axis.pos, labels=axis.labels, las=2, line=-0.5, cex.axis=0.75)
  
  # optionally label gradient axis
  if(!missing(grad.axis.title))
    mtext(grad.axis.title, at=median(transect$scaled.grad), side=2, line=2.5, font=2, cex=0.75)
  
  # add distance along gradient
  axis(side=1, at=x.pos, labels=round(transect$distance/dist.scaling.factor), cex.axis=0.75)
  mtext(distance.axis.title, side=1, line=2, font=2, cex=0.75)
  
  # link gradient points to profiles
  segments(x0=x.pos, y0=y.offset-15, x1=transect$scaled.distance, y1=transect$scaled.grad, lty=1)
  
  # add arrows
  arrows(x0=x.pos, y0=y.offset-15, x1=x.pos, y1=y.offset-2, length = 0.1, code=2, lty=1)
  
  # add gradient line
  lines(transect$scaled.distance, transect$scaled.grad, lwd=2, col=transect.col)
  
  # add gradient points
  points(transect$scaled.distance, transect$scaled.grad, lwd=2, pch=21, cex=1.25, bg=transect.col, col='black')
  
  # invisibly return computed transect geometry
  invisible(transect)
}
