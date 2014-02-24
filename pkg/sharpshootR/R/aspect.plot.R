# plot a vector of aspect measurements, with units of degrees, measured via compass
aspect.plot <- function(p, p.bins=60, p.bw=30, stack=TRUE, p.axis=seq(0, 350, by=10), plot.title=NULL, line.col='RoyalBlue', line.lwd=1, line.lty=2, arrow.col=line.col, arrow.lwd=1, arrow.lty=1, ...) {
	# remove NA
	p <- na.omit(p)
	
	# make a circular class object: using native degrees, template sets zero and direction
	c.p <- circular(p, units='degrees', template="geographics")
	
	# compute the mean
	m.p <- mean(c.p)
	
	# compute mean resultant length
	rho.p <- rho.circular(c.p)
	
	# setup custom axis
	a.p <- circular(p.axis, units='degrees', template="geographics")
	
	# circular histogram
	plot(c.p, axes=FALSE, stack=stack, bins=p.bins, shrink=1.45, sep=0.06, ...)
	
	# add circular density, bw is th  e smoothness parameter
	lines(density(c.p, bw=p.bw), col=line.col, lty=line.lty, lwd=line.lwd)
	
	# add axes: note work-around for strange bug...
	axis.circular(at=a.p, labels=a.p, cex=0.6) #  buggy  in circular <= 0.4-3 (2011-07-18)	
	
	# annotate north
	text(0, 1.125, 'North', cex=0.85, font=1)
	
	# annotate mean with an arrow
	arrows.circular(m.p, shrink=rho.p, length=0.15, col=arrow.col, lwd=arrow.lwd, lty=arrow.lty)
	
	# add title
	text(0, -0.25, plot.title, cex=0.85, font=2)
}