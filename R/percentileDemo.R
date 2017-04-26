# graphical demo of percentiles, mean, SD, idealized normal, etc.

percentileDemo <- function(x, labels.signif=3, pctile.color='RoyalBlue', mean.color='Orange', range.color='DarkRed', hist.breaks=30, boxp=FALSE, ...) {
  
  # convenient summary of vector x, can contain NA
  .summary <- function(x, pr=c(0.05, 0.1, 0.5, 0.9, 0.95)) {
    n <- length(na.omit(x))
    x.mean <- mean(x, na.rm=TRUE)
    x.skew <- skewness(x, na.rm=TRUE)
    x.sd <- sd(x, na.rm=TRUE)
    x.min <- min(x, na.rm=TRUE)
    x.max <- max(x, na.rm=TRUE)
    q <- t(Hmisc::hdquantile(x, probs = pr, na.rm=TRUE))
    dimnames(q)[[2]] <- paste0('P', pr * 100)
    # return as matrix so that we can use non-standard column names
    d <- cbind(min=x.min, "-2SD"=x.mean - (2*x.sd), P05=q[1], P10=q[2], P50=q[3], mean=x.mean, P90=q[4], P95=q[5], "+2SD"=x.mean + (2*x.sd), max=x.max, sd=x.sd, skew=x.skew, n=n)
    return(d)
  }
  
  ## borrowed from MU reports
  # remove NA
  # re-scale to {0,1}
  # return x,y values
  .scaled.density <- function(x) {
    res <- stats::density(na.omit(x), cut=1)
    return(data.frame(x=res$x, y=scales::rescale(res$y)))
  }
  
  # prep values
  s <- .summary(x) # result is a 1-row matrix
  x <- na.omit(x)
  
  # plot parameters
  y.base.pctiles <- -0.05
  y.base.normal <- -0.15
  y.base.range <- -0.25
  label.offset <- 0.04
  
  
  # density plot of the data, scaled to {0,1}
  d.x <- .scaled.density(x)
  
  # idealized normal over interval of the data
  x.seq <- seq(s[, 'mean'] - 2.5 * s[, 'sd'], s[, 'mean'] + 2.5 * s[, 'sd'], length.out = 100)
  ideal.normal <- cbind(x.seq, scales::rescale(dnorm(x.seq, mean=s[, 'mean'], sd=s[, 'sd'])))
  
  # histogram with "counts" re-scaled to {0,1}
  x.hist <- graphics::hist(x, plot=FALSE, breaks=hist.breaks)
  x.hist$counts <- scales::rescale(x.hist$counts)
  
  # setup plot
  plot(d.x, type='n', ylim=c(-0.3, 1.1), axes=FALSE, ...)
  abline(h=c(0, 1), col=grey(0.85))
  plot(x.hist, add=TRUE, col=grey(0.9), border=grey(0.85))
  lines(d.x, lwd=2)
  lines(ideal.normal)
  
  # add obs as vertical ticks
  # points(x, rep(1, times=length(x)), pch='|', cex=0.5, col=grey(0.75))
  
  
  ## boxplot for reference
  if(boxp) {
    y.base.range <- y.base.range - 0.02
    y.base.normal <- y.base.normal - 0.02
    y.base.pctiles <- y.base.pctiles - 0.02
    
    boxplot(x, at=-0.02, add=TRUE, axes=FALSE, horizontal=TRUE, boxwex=0.05, lwd=1, cex=0.5)
    # boxplot(rnorm(5000, mean=s[, 'mean'], sd=s[, 'sd']), at=1, add=TRUE, axes=FALSE, horizontal=TRUE, boxwex=0.05, col='grey')
  }
  
  
  ## percentiles
  # lower / upper pctile
  segments(x0=s[, 'P10'], x1=s[, 'P90'], y0=y.base.pctiles, y1=y.base.pctiles, col=pctile.color, lwd=2)
  segments(x0=c(s[, 'P10'], s[, 'P90']), x1=c(s[, 'P10'], s[, 'P90']), y0=y.base.pctiles, y1=1, lty=3, col=pctile.color)
  # median
  segments(x0=s[, 'P50'], x1=s[, 'P50'], y0=y.base.pctiles, y1=1, lty=3, col=pctile.color)
  points(s[, 'P50'], y.base.pctiles, cex=1.2, pch=22, bg=pctile.color)
  # add values
  text(x=c(s[, 'P10'], s[, 'P50'], s[, 'P90']), y=y.base.pctiles - label.offset, label=signif(c(s[, 'P10'], s[, 'P50'], s[, 'P90']), labels.signif), cex=0.65)
  
  ## idealized normal
  # +/- SD
  segments(x0=(-2 * s[, 'sd']) + s[, 'mean'], x1=(2 * s[, 'sd']) + s[, 'mean'], y0=y.base.normal, y1=y.base.normal, col=mean.color, lwd=2)
  segments(x0=(-2 * s[, 'sd']) + s[, 'mean'], x1=(-2 * s[, 'sd']) + s[, 'mean'], y0=y.base.normal, y1=1, lty=3, col=mean.color)
  segments(x0=(2 * s[, 'sd']) + s[, 'mean'], x1=(2 * s[, 'sd']) + s[, 'mean'], y0=y.base.normal, y1=1, lty=3, col=mean.color)
  # mean
  segments(x0=s[, 'mean'], x1=s[, 'mean'], y0=y.base.normal, y1=1, lty=3, col=mean.color)
  points(s[, 'mean'], y.base.normal, cex=1.25, pch=22, bg=mean.color)
  # add values
  text(x=c(((-2 * s[, 'sd']) + s[, 'mean']), s[, 'mean'], ((2 * s[, 'sd']) + s[, 'mean'])), y=y.base.normal - label.offset, label=signif(c(((-2 * s[, 'sd']) + s[, 'mean']), s[, 'mean'], ((2 * s[, 'sd']) + s[, 'mean'])), labels.signif), cex=0.65)
  
  ## min / max
  # +/- SD
  segments(x0=s[, 'min'], x1=s[, 'max'], y0=y.base.range, y1=y.base.range, col=range.color, lwd=2)
  segments(x0=s[, 'min'], x1=s[, 'min'], y0=y.base.range, y1=1, lty=3, col=range.color)
  segments(x0=s[, 'max'], x1=s[, 'max'], y0=y.base.range, y1=1, lty=3, col=range.color)
  # add values
  text(x=c(s[, 'min'], s[, 'max']), y=y.base.range - label.offset, label=signif(c(s[, 'min'], s[, 'max']), labels.signif-1), cex=0.65)
  
  # finish basic axix / box
  axis(side=1, at = pretty(x, n = 10))
  box()
  
  # combined legend
  legend('top', lwd=c(2,2,2,2,1, NA), lty=c(1,1,1,1,1, NA), col=c(pctile.color, mean.color, range.color, 'black', 'black'), legend = c('10th-50th-90th', 'Mean +/- 2SD', 'Min / Max', 'Data', 'Ideal', paste('Obs:', s[, 'n'])), horiz=TRUE, bty='n', cex=0.8)
  
  invisible(s)
}


