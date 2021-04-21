
# http://hydromad.catchment.org/#bucket
# https://github.com/josephguillaume/hydromad/blob/master/src/bucket.c

## consider implementation as a GRASS t.* routine or t.mapcalc expression


#' @title Monthly Water Balances
#' 
#' @description Perform a monthly water balance by "leaky bucket" model, provided by the `hydromad` package.
#' 
#' @note This function depends on the \href{http://hydromad.catchment.org/}{hydromad package}.
#' 
#' @author D.E. Beaudette
#' 
#' @param AWC available water-holding capacity (mm)
#' 
#' @param PPT time-series of monthly PPT (mm), calendar year ordering
#' 
#' @param PET time-series of monthly PET (mm), calendar year ordering
#' 
#' @param S_init initial fraction of \code{AWC} filled with water
#' 
#' @param starting_month starting month index, 1=January, 9=September
#' 
#' @param rep number of cycles to run water balance
#' 
#' @param keep_last keep only the last iteration of the water balance
#' 
#' 
#' @return a \code{data.frame} with the following elements:
#' 
#' \itemize{
#' \item{PPT: }{monthly PPT values}
#' \item{PET: }{monthly PET values}
#' \item{U: }{monthly U values}
#' \item{S: }{monthly S values}
#' \item{ET: }{monthly ET values}
#' \item{D: }{monthly D values}
#' \item{month: }{month number}
#' \item{mo: }{month label}   
#' }
#' 
#' @examples 
#' 
#' if(requireNamespace('hydromad')) {
#' 
#' # AWC in mm
#' AWC <- 200
#' 
#' # monthly PET and PPT in mm
#' PET <- c(0,0,5,80,90,120,130,140,110,90,20,5)
#' PPT <- c(0, 150, 200, 120, 20, 0, 0, 0, 10, 20, 30, 60)
#' 
#' # run water balance
#' # start with soil AWC "empty"
#' (x.wb <- monthlyWB(AWC, PPT, PET, S_init = 0))
#' 
#' # plot the results
#' par(mar=c(4,4,2,1), bg = 'white')
#' plotWB(WB = x.wb, AWC = AWC)
#' 
#' # compute fraction of AWC filled after the last month of simulation
#' (last.S <- x.wb$S[12] / AWC)
#' 
#' # re-run the water balance with this value
#' (x.wb <- monthlyWB(AWC, PPT, PET, S_init = last.S))
#' 
#' # not much difference
#' par(mar=c(4,4,2,1), bg = 'white')
#' plotWB(WB = x.wb, AWC = AWC)
#' 
#' }
#' 
monthlyWB <- function(AWC, PPT, PET, S_init = AWC, starting_month = 1, rep = 1, keep_last = FALSE) {
  
  # sanity check: package requirements
  if(!requireNamespace('hydromad'))
    stop('please install the hydromad package', call. = FALSE)
  
  # number of time steps in the original series
  n <- length(PPT)
  
  # re-order monthly data according to starting month
  if(starting_month == 1) {
    idx <- seq(from=starting_month, to=12, by = 1)
  } else {
    idx <- c(seq(from=starting_month, to=12, by=1), seq(from=1, to=(starting_month - 1), by=1))
  }
  
  # replicate as needed
  idx <- rep(idx, times=rep)
  
  # re-index months as needed
  PPT <- PPT[idx]
  PET <- PET[idx]
  
  # combine into format suitable for simulation
  d <- data.frame(P = PPT, E = PET)
  
  ## TODO: consider exposing more hydromad arguments
  ## TODO: investigate use of `fc` here
  ## TODO: consider the same abstraction as the daily version: simpleWB()
  
  # Sb: total water storage (mm)
  # fc field capacity fraction: fraction of Sb
  # S_0 initial moisture content as fraction of Sb 
  m <- hydromad::hydromad(d, sma = "bucket", routing = NULL)
  m <- update(m, Sb = AWC, fc = 1, S_0 = S_init, a.ss = 0, M=0, etmult=1, a.ei=0)
  res <- predict(m, return_state = TRUE)
  
  res <- data.frame(d, res)
  
  names(res) <- c('PPT', 'PET', 'U', 'S', 'ET')
  res$D <- with(res, ET - PET)
  
  # add month index
  res$month <- idx
  res$mo <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')[idx]
  
  # optionally keep the last cycle
  if(keep_last) {
    keep.idx <- seq(from=nrow(res) - (n-1), to = nrow(res), by = 1)
    res <- res[keep.idx, ]
  }
  
  # done
  return(res)
}



#' @title Visualize Monthly Water Balance
#' 
#' @description This function offers one possible visualization for the results of `monthlyWB`.
#' 
#' @note You may have to adjust figure margins and size to get all of the elements to "look right".
#' 
#' @author D.E. Beaudette and J.M. Skovlin
#' 
#' @param WB output from \code{monthlyWB}
#' 
#' @param AWC available water-holding capacity (mm)
#' 
#' @param showAWC styling to show box for AWC below (default) or above zero point of axis
#'
#' @param sw.col color for soil water
#' 
#' @param surplus.col color for surplus water
#' 
#' @param et.col color for ET
#' 
#' @param deficit.col color for deficit
#' 
#' @param pch plotting character for PPT and PET points (`c('P', 'E')`)
#' 
#' @param pt.cex character expansion factor for PPT and PET points
#' 
#' @param lwd line width for PPT and PET curves
#' 
#' @param n.ticks approximate number of tick marks on positive and negative y-axis
#' 
#' @param grid.col horizontal grid line color
#' 
#' @param month.cex character expansion factor for month labels (x-axis)
#' 
#' @keywords hplots
#' 
#' @examples 
#' 
#' if(requireNamespace('hydromad')) {
#' 
#' ## 100mm (4") AWC
#' # monthly PPT and PET extracted from monthly estimates at Sonora, CA
#' AWC <- 100
#' PPT <- c(171, 151, 138, 71, 36, 7, 1, 2, 11, 48, 102, 145)
#' PET <- c(15.17, 18.26, 30.57, 42.95, 75.37, 108.05, 139.74, 128.9, 93.99, 59.84, 26.95, 14.2)
#' 
#' # water-year
#' # three years
#' x.wb <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 9, rep = 3)
#' x.wb[x.wb$mo == 'Sep', ]
#' 
#' # new-style representation, with AWC "above 0"
#' plotWB(x.wb, AWC, showAWC = 'below')
#' 
#' # old-style representation, with AWC "above 0"
#' plotWB(x.wb, AWC, showAWC = 'above')
#' 
#' 
#' # water year
#' # last iteration
#' x.wb <- monthlyWB(AWC, PPT, PET, S_init = 0, 
#'                   starting_month = 9, rep = 3, 
#'                   keep_last = TRUE
#' )
#' 
#' plotWB(x.wb, AWC)
#' 
#' }
#' 
plotWB <- function(WB, AWC, showAWC = 'below', sw.col = '#377EB8', surplus.col = '#4DAF4A', et.col = '#E41A1C', deficit.col = '#FF7F00', pch = c('P', 'E'), pt.cex = 0.85, lwd = 2, n.ticks = 8, grid.col = grey(0.65), month.cex = 1) {
  
  # number of time steps, usually months
  n <- nrow(WB)
  
  if (showAWC == 'above') {
  
  # left-side vertical scales
  # y.axt <- pretty(c(AWC + WB$U, WB$PPT, 0 - WB$PET), n = 15)
  y.min <- min(- WB$PET)
  y.max <- max(AWC + WB$U)
  
  # max for PPT|AWC
  ppt.awc.max <- max(AWC + WB$U)
  
  # specific axis
  # surplus.axis <- pretty(WB$U, n = 5)
  ppt.axis <- pretty(c(0, ppt.awc.max), n = n.ticks)
  pet.axis <- pretty(WB$PET, n = n.ticks)
  
  # remove '0' from surplus and pet axes
  pet.axis <- pet.axis[pet.axis != 0]
  # surplus.axis <- surplus.axis[surplus.axis != 0]

  # init barplot x-axis
  bp <- barplot(AWC + WB$S, plot=FALSE)
  bp <- as.vector(bp)
  
  # init canvas
  plot(1, 1, type='n', ylim=c(y.min, y.max), xlim=c(0, max(bp)),  axes=FALSE, ylab='', xlab='')
  
  # horizontal helper lines
  segments(x0 = 0, y0 = ppt.axis, x1 = max(bp), y1 = ppt.axis, lty = 2, col = grid.col)
  segments(x0 = 0, y0 = -pet.axis, x1 = max(bp), y1 = -pet.axis, lty=2, col = grid.col)
  
  # plot surplus
  barplot(AWC + WB$U, axes=FALSE, ylab='', xlab='', col=surplus.col, border=NA, add=TRUE)
  
  # overlay AWC with white to erase surplus within AWC
  barplot(rep(AWC, times=n), axes=FALSE, col=par('bg'), border=NA, add=TRUE)
  
  # overlay soil water
  barplot(WB$S, axes=FALSE, col=sw.col, border=NA, add=TRUE)
  
  # utilization of soil water
  # barplot(WB$u_i, axes=FALSE, col='orange', border=NA, add=TRUE)
  
  # add deficit
  barplot((0 - WB$E) + WB$D, axes=FALSE, col=deficit.col, border=NA, add=TRUE)
  
  # overlay actual ET
  barplot(0 - WB$E, axes=FALSE, col=et.col, border=NA, add=TRUE)
  
  # annotate AWC
  rect(xleft = 0.2, ybottom = 0, xright = n + (n * 0.2), ytop = AWC)
  
  } 
  
if(showAWC == 'below') {
  
  # left-side vertical scales
  # y.min <- min(- WB$PET - AWC/2)
  # y.max <- max(AWC/2 + WB$U)
  
  y.min <- min(c(- WB$PET, - AWC))
  y.max <- max(c(WB$U, WB$PPT, WB$PET))
  
  # specific axis
  ppt.axis <- pretty(c(0, y.max), n = n.ticks)
  pet.axis <- pretty(c(0, (0 - y.min)), n = n.ticks)
  
  # remove '0' from surplus and pet axes
  pet.axis <- pet.axis[pet.axis != 0]
  # surplus.axis <- surplus.axis[surplus.axis != 0]
  
  # init barplot x-axis
  bp <- barplot(AWC + WB$S, plot=FALSE)
  bp <- as.vector(bp)
  
  # init canvas
  plot(1, 1, type='n', ylim=c(y.min, y.max), xlim=c(0, max(bp)),  axes=FALSE, ylab='', xlab='')
  
  # horizontal helper lines
  segments(x0 = 0, y0 = ppt.axis, x1 = max(bp), y1 = ppt.axis, lty = 2, col = grid.col)
  segments(x0 = 0, y0 = -pet.axis, x1 = max(bp), y1 = -pet.axis, lty=2, col = grid.col)
  
  # overlay AWC with white to erase surplus within AWC
  barplot(rep((0 - AWC), times=n), axes=FALSE, col=par('bg'), border=NA, add=TRUE)
  
  # add deficit
  barplot((0 - WB$E) + WB$D, axes=FALSE, col=deficit.col, border=NA, add=TRUE)
  
  # overlay soil water
  barplot((0 - WB$S), axes=FALSE, col=sw.col, border=NA, add=TRUE)
  
  # utilization of soil water
  # barplot(WB$u_i, axes=FALSE, col='orange', border=NA, add=TRUE)
  
  # overlay actual PET
  barplot(WB$PET, axes=FALSE, col=et.col, border=NA, add=TRUE)
  
  # overlay actual ET
  #barplot(WB$E, axes=FALSE, col=aet.col, border=NA, add=TRUE)
  
  # plot surplus
  barplot(WB$U, axes=FALSE, ylab='', xlab='', col=surplus.col, border=NA, add=TRUE)
  
  # annotate AWC
  rect(xleft = 0.2, ybottom = (0 - AWC), xright = n + (n * 0.2), ytop = 0)
  
} 
  
  # left-hand axes
  combined.at <- c(-pet.axis, ppt.axis)
  combined.lab <- c(pet.axis, ppt.axis)
  axis(side = 2, las=1, at = combined.at, labels = combined.lab, cex.axis=0.85, line=-0.5)
  
  # # right-hand axis
  # axis(side = 4, las=1, at = AWC+surplus.axis, labels = surplus.axis, cex.axis=0.85, line=-1)
  
  # annotate left-hand axes
  # positive side
  # mtext(text = 'PPT | Soil Water', side = 2, line = 2.25, at=10, adj=0, cex=0.85, font=2)
  # mtext(text = 'Surplus', side = 4, line = 2.25, at=y.max, adj=1, cex=0.85, font=2)
  # negative side
  mtext(text = 'Soil Water | Deficit | Surplus | PET | ET    (mm)', side = 2, line = 2.25, cex=0.85, font=2)
  
  # annotate AWC
  mtext(sprintf("AWC: %smm", AWC), side = 3, at = 0, cex = 0.85, font = 3, adj = 0)
  
  # month axis: no line, just ticks
  axis(side = 1, at = bp, labels = WB$mo, line = 0, tick = TRUE, font = 2, cex = month.cex, col = NA, col.ticks = par('fg'))
  
  # using a bg-colored plotting symbol to paint behind PPT and PET plotting symbols
  # must be a little smaller than the symbol
  bg.cex <- strheight(pch[1], font = 2, cex = pt.cex) / 6
  
  # PPT
  points(bp, WB$PPT, col=par('bg'), pch = 15, cex = bg.cex)
  lines(bp, WB$PPT, type='b', col=par('fg'), lwd = lwd, pch = pch[1], cex = pt.cex, font = 2)
  
  # PET
  points(bp, 0 - WB$PET, col=par('bg'), pch = 15, cex = bg.cex)
  lines(bp, 0 - WB$PET, type='b', col=par('fg'), lwd = lwd, pch = pch[2], cex = pt.cex, font = 2)
  
  # legend
  legend(x = max(bp), y = y.max, horiz = TRUE, legend = c('Soil Water', 'Surplus', 'ET', 'Deficit'), col = c(sw.col, surplus.col, et.col, deficit.col), pch = 15, bty='n', pt.cex = 1.5, xpd = NA, cex = 1.125, xjust = 1, yjust = -0.25)
  
}


