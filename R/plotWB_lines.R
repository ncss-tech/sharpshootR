


#' @title Line / Area Visualization for Monthly Water Balance
#' 
#' 
#' @author J.M. Skovlin and D.E. Beaudette
#'
#' @param WB output from [`monthlyWB()`]
#' @param cols vector of three colors used for area under PPT, PET, and AET curves
#' @param interpolator spline or linear interpolation of monthly values, use of `spline` may lead to minor smoothing artifacts in shaded areas
#' @param spline.method when `interpolator = 'spline'`, argument passed to `splinefun(..., method = spline.method)`
#' @param month.cex scaling factor for month labels
#'
#' @export
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
#' # calendar-year
#' # three year warm-up
#' x.wb <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 1, rep = 3, keep_last = TRUE)
#'  
#' # plot
#' plotWB_lines(x.wb)
#' 
#' }
#'
plotWB_lines <- function(WB, cols = c("#ACC0F0", "#E88C8C", "#65BF65"), interpolator = c('spline', 'linear'), spline.method = c('natural', 'periodic'), month.cex = 1) {
  
  ## TODO: better colors
  
  ## first draft colors
  # cols <- c('royalblue', 'firebrick', 'darkgreen')
  # cols <- desaturate(lighten(cols, amount = 0.25), 0.25)
  
  # sanity checks
  interpolator <- match.arg(interpolator)
  spline.method <- match.arg(spline.method)
  
  # extract colors
  col.ppt <- cols[1]
  col.pet <- cols[2]
  col.utilization <- cols[3]
  
  # generate interpolation functions for estimating intersection
  # approxfun - linear interpolation, pointy, exact
  # splinefun - spline interpolation: pretty, overshoots
  # method = 'periodic' <- ideal / requires constant first-last values
  # negative overshoots -> truncate at 0
  # positive overshoots...?
  
  if(interpolator == 'exact') {
    ppt.interp <- approxfun(WB$month, WB$PPT)
    pet.interp <- approxfun(WB$month, WB$PET)
    aet.interp <- approxfun(WB$month, WB$ET)
    def.interp <- approxfun(WB$month, -WB$D)
  }
  
  if(interpolator == 'spline') {
    ppt.interp <- splinefun(WB$month, WB$PPT, method = spline.method)
    pet.interp <- splinefun(WB$month, WB$PET, method = spline.method)
    aet.interp <- splinefun(WB$month, WB$ET, method = spline.method)
    def.interp <- splinefun(WB$month, -WB$D, method = spline.method)
  }
  
  
  
  # need to figure out range from data
  y.range <- range(c(WB$PET, WB$PPT))
  
  # interpolate between month centers
  month.start <- WB$month[1]
  month.end <- WB$month[12]
  
  # use a 0.1 month grid for smoother lines / more stable integration
  month.seq <- seq(from=month.start, to=month.end, by=0.1)
  
  # interpolate with a spline function, truncate at 0
  ppt.seq <- pmax(ppt.interp(month.seq), 0)
  pet.seq <- pmax(pet.interp(month.seq), 0)
  aet.seq <- pmin(pmax(aet.interp(month.seq), 0), pmax(pet.interp(month.seq), 0))
  def.seq <- pmax(def.interp(month.seq), 0)
  
  # 
  # ## TODO: PET - AET crossings are the real targets
  # 
  # # locate crossings - isolate area of intersection where PET >= AET
  # surplus_deficit.flag <- sign(pet.seq - aet.seq)
  # crossings.idx <- which(abs(diff(surplus_deficit.flag)) > 0)
  # 
  # # locate additional crossings - isolate the intersection of AET > PPT
  # surplus_deficit.flag <- sign(pet.seq - aet.seq)
  # crossings1.idx <- which(abs(diff(surplus_deficit.flag)) > 0)
  
  
  # setup plot area
  plot(0, 0, type='n', xlim=c(1, 12), ylim=c(y.range), ylab='Water (mm)', xlab='', las = 1, axes = FALSE)
  
  # 
  # # iterate over crossings
  # for(i in 1:(length(crossings.idx) - 1)) {
  #   
  #   # determine PPT and PET total between crossings
  #   ppt.i <- integrate(ppt.interp, lower=month.seq[crossings.idx[i]], upper=month.seq[crossings.idx[i+1]])
  #   pet.i <- integrate(pet.interp, lower=month.seq[crossings.idx[i]], upper=month.seq[crossings.idx[i+1]])
  #   
  #   # generate color based on PPT surplus / deficit
  #   if((ppt.i$value - pet.i$value) > 0)
  #     col.i <- NA else col.i <- col.pet
  #   
  #   # compute x and y coordinates for polygon defined by PPT and PET functions
  #   p.1.x <- month.seq[crossings.idx[i]:crossings.idx[i+1]]
  #   p.1.y <- ppt.interp(p.1.x)
  #   p.2.x <- rev(p.1.x)
  #   p.2.y <- pet.interp(p.2.x)
  #   
  #   # add polygon + color
  #   polygon(c(p.1.x, p.2.x), c(p.1.y, p.2.y), col=col.i, border=NA)
  # }
  
  # shade area under the PPT line
  p.1.x <- month.seq
  p.1.y <- rep(0, length(month.seq))
  p.2.x <- rev(p.1.x)
  p.2.y <- pmax(ppt.interp(p.2.x), 0)
  polygon(c(p.1.x, p.2.x), c(p.1.y, p.2.y), col=col.ppt, border=NA)
  
  # shade area under the PET line
  p.1.x <- month.seq
  p.1.y <- rep(0, length(month.seq))
  p.2.x <- rev(p.1.x)
  p.2.y <- pmax(pet.interp(p.2.x), 0)
  polygon(c(p.1.x, p.2.x), c(p.1.y, p.2.y), col=col.pet, border=NA)
  
  # shade area under the AET line
  p.1.x <- month.seq
  p.1.y <- rep(0, length(month.seq))
  p.2.x <- rev(p.1.x)
  p.2.y <- pmin(pmax(aet.interp(p.2.x), 0), pmax(pet.interp(p.2.x), 0))
  polygon(c(p.1.x, p.2.x), c(p.1.y, p.2.y), col = col.utilization, border = NA)
  
  
  # # shades the AET > PPT polygon - determine color scheme to make this area look like area under PPT line, uncolor then re-shade it?
  # p.1.x <- month.seq[crossings1.idx[1]:crossings1.idx[2]]
  # p.1.y <- aet.interp(p.1.x)
  # p.2.x <- rev(p.1.x)
  # p.2.y <- ppt.interp(p.2.x)
  # # set area to no color then shade area the same color as PPT area
  # polygon(c(p.1.x, p.2.x), c(p.1.y, p.2.y), col=0, border=NA)
  # polygon(c(p.1.x, p.2.x), c(p.1.y, p.2.y), col=col.utilization, border=NA)
  
  # add original PPT and PET series
  lines(ppt.seq ~ month.seq, type='l', lwd=2, col='blue')
  lines(pet.seq ~ month.seq, type='l', lwd=2, lty=2, col='brown')
  lines(aet.seq ~ month.seq, type='l', lwd=2, lty=4, col='black')
  
  ## how can we use / make sense of WB deficit?
  # lines(def.seq ~ month.seq, type='l', lwd=1, lty=1, col='black')
  
  # month axis
  axis(side = 1, at = month.start:month.end, labels = WB$mo, line = 0, tick = TRUE, font = 2, cex = month.cex, col = NA, col.ticks = par('fg'))
  
  # PPT / PET axis
  axis(side = 2, at = pretty(y.range, n = 8), las = 1)
  
  # simple grid
  grid()
  
  # shaded area legend
  legend(x = 0, y = y.range[2], legend=c('Surplus / Recharge', 'Utilization', 'Deficit'), col=c(col.ppt, col.utilization, col.pet), pch=c(15, 15, 15), pt.cex=2, bty='n', horiz = TRUE, xpd = NA, xjust = 0, yjust = -0.25)
  
  # line legend
  legend(x = 12, y = y.range[2], legend = c('PPT', 'PET', 'AET'), col = c( 'blue', 'brown', 'black'), lwd = 2, lty=c(1, 2, 4), bty='n', horiz = TRUE, xpd = NA, xjust = 1, yjust = -0.25)
  
  # annotate AWC
  AWC <- attr(WB, 'AWC')
  mtext(sprintf("AWC: %s mm", AWC), side = 1,  at = 1, cex = 0.85, adj = 0, line = 2.5)
  
  # annotate total deficit
  sumD <- bquote(sum(Deficit)  ==  .(round(sum(WB$D)))~mm)
  mtext(sumD, side = 1,  at = 12, cex = 0.85,  adj = 1, line = 2.5)
  
}

