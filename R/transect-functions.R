
## TODO:
## * implement non-linear re-scaling of distances / variables
## * perform all re-scaling via linear model?
## * consider sorting by distance vs. var (requires a starting point)
## * consider manually defined vector of indices
## * modified relative distances to avoid overlap


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

# plot a transect with profiles below
plotTransect <- function(s, grad.var.name, grad.var.order=order(site(s)[[grad.var.name]]), transect.col='RoyalBlue', tick.number=7, y.offset=100, scaling.factor=0.5, distance.axis.title='Distance Along Transect (km)', crs=NULL, grad.axis.title=NULL, dist.scaling.factor=1000, spacing='regular', fix.relative.pos=list(thresh = 0.6, trace = TRUE, maxIter = 5000), ...){
  
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
  transect <- dist.along.grad(coords, site(s)[[grad.var.name]], grad.var.order, grad.scaled.min=0, grad.scaled.max=y.offset-15)
  
  # use a linear model to translate original gradient -> scaled gradient 
  l <- lm(scaled.grad ~ variable, data=transect)
  
  # establish gradient axis tick marks
  axis.labels <- pretty(transect$variable, n=tick.number)
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
      x.pos <- do.call(aqp::fixOverlap, fix.relative.pos)
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
