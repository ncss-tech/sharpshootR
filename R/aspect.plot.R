


#' @title Plot Aspect Data
#' 
#' @description Plot a graphical summary of multiple aspect measurements on a circular diagram.
#'
#' @param p a vector of aspect angles in degrees, measured clock-wise from North
#' @param q a vector of desired quantiles
#' @param p.bins number of bins to use for circular histogram
#' @param p.bw bandwidth used for circular density estimation
#' @param stack `logical`, should the individual points be stacked into `p.bins` number of bins and plotted
#' @param p.axis a sequence of integers (degrees) describing the circular axis
#' @param plot.title an informative title
#' @param line.col density line color
#' @param line.lwd density line width
#' @param line.lty density line line style
#' @param arrow.col arrow color
#' @param arrow.lwd arrow line width
#' @param arrow.lty arrow line style
#' @param arrow.length arrow head length
#' @param ... further arguments passed to `circular::plot.circular`
#'
#' @details Spread and central tendency are depicted with a combination of circular histogram and kernel density estimate. The circular mean, and relative confidence in that mean are depicted with an arrow: longer arrow lengths correspond to greater confidence in the mean.
#' 
#' @note Manual adjustment of `p.bw` may be required in order to get an optimal circular density plot. This function requires the package `circular`, version 0.4-7 or later.
#' 
#' @keywords hplots
#' 
#' @author D.E. Beaudette
#' 
#' @return invisibly returns circular stats
#' @export
#'
#' @examples
#' # simulate some data
#' p.narrow <- runif(n=25, min=215, max=280)
#' p.wide <- runif(n=25, min=0, max=270)
#' 
#' # set figure margins to 0, 2-column plot
#' op <- par(mar = c(0,0,0,0), mfcol = c(1,2))
#' 
#' # plot, save circular stats 
#' x <- aspect.plot(p.narrow, p.bw=10, plot.title='Soil A', pch=21, col='black', bg='RoyalBlue')
#' y <- aspect.plot(p.wide, p.bw=10, plot.title='Soil B', pch=21, col='black', bg='RoyalBlue')
#' 
#' # reset output device options
#' par(op) 
#' 
#' x
#' 
#' 
aspect.plot <- function(p, q=c(0.05, 0.5, 0.95), p.bins=60, p.bw=30, stack=TRUE, p.axis=seq(0, 350, by=10), plot.title=NULL, line.col='RoyalBlue', line.lwd=1, line.lty=2, arrow.col=line.col, arrow.lwd=1, arrow.lty=1, arrow.length=0.15, ...) {
	# remove NA
	p <- na.omit(p)
	
	# make a circular class object: using native degrees, template sets zero and direction
	c.p <- circular(p, units='degrees', template="geographics", modulo='2pi')
  
  # compute quantiles
	q.p <- quantile(c.p, probs=q)
  
	# compute mean resultant length
	rho.p <- rho.circular(c.p)
	
	# setup custom axis
	a.p <- circular(p.axis, units='degrees', template="geographics")
	
	# circular histogram
	plot(c.p, axes=FALSE, stack=stack, bins=p.bins, shrink=1.45, sep=0.06, ...)
	
	# add circular density, bw is th  e smoothness parameter
	lines(density(c.p, bw=p.bw), col=line.col, lty=line.lty, lwd=line.lwd)
	
	# add axes:
	axis.circular(at=a.p, labels=a.p, cex=0.6) #  buggy  in circular <= 0.4-3 (2011-07-18)	
	
	# annotate north
	text(0, 1.125, 'North', cex=0.85, font=1)
	
	# annotate stats with a arrows
	arrows.circular(q.p, shrink=rho.p, length=arrow.length, col=arrow.col, lwd=arrow.lwd, lty=arrow.lty)
	
	# add title
	text(0, -0.25, plot.title, cex=0.85, font=2)
  
	## TODO: this is kind of noisy, consider adding to the plot or output
	# compare relative to uniform circular distribution
	rt <- rayleigh.test(c.p)
	print(rt)
	
	# invisibly return stats
  invisible(q.p)
}
