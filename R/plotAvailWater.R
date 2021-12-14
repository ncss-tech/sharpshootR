## TODO: better documentation

# inspired by Figure 2 in: https://www.nature.com/scitable/knowledge/library/soil-water-dynamics-103089121/



#' @title Visual Demonstration of Available Soil Water
#' 
#' @description Generate a simplistic diagram of the various fractions of water held within soil pore-space. Largely inspired by \href{https://www.nature.com/scitable/knowledge/library/soil-water-dynamics-103089121/}{Figure 2 from O'Geen (2013)}.
#'
#' @param x a `data.frame` containing sample names and water retention data, see examples below
#' @param width vertical width of each bar graph
#' @param cols a vector of colors used to symbolize 'solid phase', 'unavailable water', 'available water', and 'gravitational water'
#' @param name.cex character scaling of horizon names, printed on left-hand side of figure
#' @param annotate logical, annotate AWC
#' 
#' @author D.E. Beaudette
#' 
#' @references O'Geen, A. T. (2013) Soil Water Dynamics. Nature Education Knowledge 4(5):9.
#'
#' @export
#' 
#' @return nothing, function is called to generate graphical output
#'
#' @examples
#' 
#' 
#' # demonstration
#' s <- data.frame(
#'   name = c('loamy sand', 'sandy loam', 'silt loam', 'clay loam'), 
#'   pwp = c(0.05, 0.1, 0.18, 0.2), 
#'   fc = c(0.1, 0.2, 0.38, 0.35), 
#'   sat = c(0.25, 0.3, 0.45, 0.4))
#' s$solid <- with(s, 1-sat)
#' 
#' par(mar=c(5, 6, 0.5, 0.5))
#' plotAvailWater(s, name.cex=1.25)
#' 
#' 
#' \donttest{
#'   
#'   if(requireNamespace("aqp")) {
#'     
#'     # demonstration using idealized AWC by soil texture
#'     data("ROSETTA.centroids", package = "aqp")
#'     
#'     # subset columns
#'     x <- ROSETTA.centroids[, c('texture', 'pwp', 'fc', 'sat', 'awc')]
#'     
#'     # adjust to expected names / additional data required by plotAvailWater
#'     names(x)[1] <- 'name'
#'     x$solid <- with(x, 1 - sat)
#'     
#'     # re-order based on approximate AWC
#'     x <- x[order(x$awc), ]
#'     
#'     op <- par(no.readonly = TRUE)
#'     
#'     par(mar=c(5, 6.5, 0.5, 0.5))
#'     plotAvailWater(x, name.cex = 1)
#'     
#'     par(op)
#'     
#'   }
#'   
#'   
#'   # use some real data from SSURGO
#'   if(requireNamespace("curl") &
#'      curl::has_internet() &
#'      require(soilDB)) {
#'     
#'     q <- "SELECT hzdept_r as hztop, hzdepb_r as hzbottom, 
#' hzname as name, wsatiated_r/100.0 as sat, 
#' wthirdbar_r/100.0 as fc, wfifteenbar_r/100.0 as pwp, awc_r as awc
#' FROM chorizon 
#' WHERE cokey IN (SELECT cokey from component where compname = 'dunstone') 
#' AND wsatiated_r IS NOT NULL 
#' ORDER BY cokey, hzdept_r ASC;"
#'     
#'     x <- SDA_query(q)
#'     x <- unique(x)
#'     x <- x[order(x$name), ]
#'     x$solid <- with(x, 1-sat)
#'     
#'     op <- par(no.readonly = TRUE)
#'     
#'     par(mar=c(5, 5, 0.5, 0.5))
#'     plotAvailWater(x)
#'     
#'     par(op)
#'   }
#'   
#' }
#' 
plotAvailWater <- function(x, width=0.25, cols=c(grey(0.5), 'DarkGreen', 'LightBlue', 'RoyalBlue'), name.cex=0.8, annotate=TRUE) {
	
  # compute fractions
  x1 <- x$solid
	x2 <- x$solid + x$pwp
	x3 <- x$solid + x$fc
	x4 <- x$solid + x$sat
	n <- nrow(x)
	
	# rough approximation of AWC, not likely correct
	avail.water <- round(x$fc - x$pwp, 2)
	avail.water.lab.x <- ((x$solid + x$pwp) + (x$solid + x$fc)) / 2
	
	# setup axes
	s.xaxis <- seq(from=0, to=1, by=0.1)
	idx <- 1:n
	
	# setup plot
	plot(0,0, xlim=c(0, 1), ylim=c(1-width, n+0.5+width), type='n', ylab='', xlab='Volume Fraction', axes=FALSE)
	axis(1, at=s.xaxis)
	mtext(x$name, side=2, las=1, at=idx, cex=name.cex)
	
	# vertical grid
	segments(x0=s.xaxis, x1=s.xaxis, y0=0, y1=n, col=grey(0.25), lty=3)
	
	# stacked bars
	rect(0, idx-width, x1, idx+width, col=cols[1])
	rect(x1, idx-width, x2, idx+width, col=cols[2])
	rect(x2, idx-width, x3, idx+width, col=cols[3])
	rect(x3, idx-width, x4, idx+width, col=cols[4])
	
	# optional annotation
	if(annotate) {
	  text(x=avail.water.lab.x, y=idx, label=avail.water, cex=0.75, font=2) 
	}
	
	# legend: http://r.789695.n4.nabble.com/legend-outside-plot-area-td2325864.html
	legend("top", legend=c('Solid', 'Unavailable', 'Available', 'Gravitational'), pt.bg=cols, pch=22, bty='n', horiz=TRUE, pt.cex=2.5, cex=1.1)
	}





