#' @title Visualize Monthly Water Balance
#' 
#' @description This function offers one possible visualization for the results of `monthlyWB()`. Note that "surplus" water is stacked on top of "actual ET", and "deficit" water is stacked below "storage". Calculate actual values for "surplus" and "deficit" from the figure like this:
#'    * surplus value = surplus - AET
#'    * deficit value = deficit - storage
#' 
#' @note You may have to adjust figure margins and size to get all of the elements to "look right".
#' 
#' @author D.E. Beaudette and J.M. Skovlin
#' 
#' @param WB output from `monthlyWB()`
#' 
#' @param AWC available water-holding capacity (mm), typically the value used in `monthlyWB()` and stored as an attribute of `WB`
#' 
#' @param showAWC now deprecated, always 'below'
#'
#' @param sw.col color for soil water ("storage)
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
#' @param month.cex scaling factor for month labels (x-axis)
#' 
#' @param legend.cex scaling factor for legend
#' 
#' @keywords hplots
#' 
#' @return nothing, function is called to generate graphical output
#' 
#' @examples 
#' 
#' if(requireNamespace('hydromad')) {
#' 
#' ## A shallow / droughty soil near Sonora CA 
#' # 100mm (4") AWC
#' AWC <- 100
#' PPT <- c(171, 151, 138, 71, 36, 7, 1, 2, 11, 48, 102, 145)
#' PET <- c(15.17, 18.26, 30.57, 42.95, 75.37, 108.05, 139.74, 128.9, 93.99, 59.84, 26.95, 14.2)
#' 
#' # water-year
#' # three years
#' x.wb <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 9, rep = 3)
#' x.wb[x.wb$mo == 'Sep', ]
#' 
#' # plot all three years
#' plotWB(x.wb)
#' 
#' # water-year / last iteration
#' x.wb <- monthlyWB(AWC, PPT, PET, S_init = 0, 
#'                   starting_month = 9, rep = 3, 
#'                   keep_last = TRUE
#' )
#' 
#' # plot
#' plotWB(x.wb)
#' 
#' 
#' ## Drummer series (Fine-silty, mixed, superactive, mesic Typic Endoaquolls), southern IL
#' 
#' AWC <- 244
#' PPT <- c(36, 37, 54, 82, 98, 96, 92, 75, 69, 70, 65, 50)
#' PET <- c(0, 0, 12, 46, 90, 130, 145, 128, 88, 46, 14, 0)
#' 
#' # using calendar year
#' x.wb <- monthlyWB(AWC, PPT, PET, S_init = 0,
#'                   starting_month = 1, rep = 3,
#'                   keep_last = TRUE
#' )
#' 
#' plotWB(x.wb)
#' 
#' }
#' 
plotWB <- function(WB, AWC = attr(WB, 'AWC'), showAWC = 'below', sw.col = '#377EB8', surplus.col = '#4DAF4A', et.col = '#E41A1C', deficit.col = '#FF7F00', pch = c('P', 'E'), pt.cex = 0.85, lwd = 2, n.ticks = 8, grid.col = grey(0.65), month.cex = 1, legend.cex = 0.9) {
  
  # number of time steps, usually months
  n <- nrow(WB)
  
  ## always plotting "below" c/o J.Skovlin
  ## this method is more intuitive and simpler to maintain
  
  y.min <- min(c(WB$D, -AWC))
  y.max <- max(c(WB$U, WB$PPT, WB$PET))
  
  # specific axis
  ppt.axis <- pretty(c(0, y.max), n = n.ticks)
  pet.axis <- pretty(c(0, (0 - y.min)), n = n.ticks)
  
  # remove '0' from left-hand side axis
  pet.axis <- pet.axis[pet.axis != 0]
  
  # init barplot x-axis
  bp <- barplot(AWC + WB$S, plot=FALSE)
  bp <- as.vector(bp)
  
  # init canvas
  plot(1, 1, type='n', ylim=c(y.min, y.max), xlim=c(0, max(bp)),  axes=FALSE, ylab='', xlab='')
  
  # horizontal helper lines
  segments(x0 = 0, y0 = ppt.axis, x1 = max(bp), y1 = ppt.axis, lty = 2, col = grid.col)
  segments(x0 = 0, y0 = -pet.axis, x1 = max(bp), y1 = -pet.axis, lty=2, col = grid.col)
  
  # stack: soil water + deficit
  #        negative so: -S + D
  barplot(rbind(-WB$S, WB$D), col = c(sw.col, deficit.col), beside = FALSE, border = NA, add = TRUE, axes = FALSE)
  
  # stack: actual ET + surplus
  #        positive so: ET + U
  barplot(rbind(WB$ET, WB$U), col = c(et.col, surplus.col), beside = FALSE, border = NA, add = TRUE, axes = FALSE)
  
  # annotate AWC
  rect(xleft = 0.2, ybottom = (0 - AWC), xright = n + (n * 0.2), ytop = 0)
  
  # left-hand axes
  combined.at <- c(-pet.axis, ppt.axis)
  combined.lab <- c(pet.axis, ppt.axis)
  axis(side = 2, las=1, at = combined.at, labels = combined.lab, cex.axis=0.85, line=-0.5)
  
  # annotate left-hand axes
  mtext(text = 'Inputs | Outputs | Storage    (mm)', side = 2, line = 2.25, cex=0.85, font=2)
  
  # annotate AWC
  mtext(sprintf("AWC: %smm", AWC), side = 3, at = 0, cex = 0.85, font = 3, adj = 0)
  
  # month axis: no line, just ticks
  axis(side = 1, at = bp, labels = WB$mo, line = 0, tick = TRUE, font = 2, cex = month.cex, col = NA, col.ticks = par('fg'))
  
  # using a bg-colored plotting symbol to paint behind PPT and PET plotting symbols
  # must be a little smaller than the symbol
  bg.cex <- strheight(pch[1], font = 2, cex = pt.cex) / 7
  
  # PPT
  points(bp, WB$PPT, col=par('bg'), pch = 15, cex = bg.cex)
  lines(bp, WB$PPT, type='b', col=par('fg'), lwd = lwd, pch = pch[1], cex = pt.cex, font = 2)
  
  # PET
  points(bp, WB$PET, col=par('bg'), pch = 15, cex = bg.cex)
  lines(bp, WB$PET, type='b', col=par('fg'), lwd = lwd, pch = pch[2], cex = pt.cex, font = 2)
  
  # legend
  legend(
    x = max(bp), 
    y = y.max, 
    horiz = TRUE, 
    legend = c('Storage', 'Surplus', 'AET', 'Deficit', 'PPT|PET'), 
    col = c(sw.col, surplus.col, et.col, deficit.col, 1, 1), 
    pch = c(15, 15, 15, 15, NA), 
    lty = c(NA, NA, NA, NA, 1), 
    bty='n', 
    pt.cex = 1.5, 
    xpd = NA, 
    cex = legend.cex, 
    xjust = 1, 
    yjust = -0.25
  )
  
}
