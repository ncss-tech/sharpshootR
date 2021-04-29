## functions related to the estimation of FFD


# many calculations need an array of 365 values, some may be NA
# sometime missing records result in < 365 values / year
# this function will align the data to vector of length 365
# 
# dt: anything that can be coerced to a Date class object
# value: vector of values associated with a single year
# fill: attempt to estimate NA (not implemented)
.alignDOY <- function(dt, value, fill=FALSE) {
  
  # 366 used to account for leap-years
  if(length(value) > 366)
    stop('this function expects a single year of data')
  
  # DOY template: extend to 366 days for leap-years
  doy.template <- vector(mode = 'numeric', length = 366)
  doy.template <- rep(NA, times=366)
  
  # get DOY from source data
  doy <- as.integer(format(as.Date(dt), "%j"))
  
  # insert data into doy template via doy index
  doy.template[doy] <- value
  
  # data aligned to DOY
  return(doy.template)
}


## TODO: better accounting for 0 days of spring / fall frost

# locate the last spring frost / first fall frost DOY
# v: vector of values that has been aligned to vector of 366 values
# frostTemp: critical temperature that defines "frost"
# endSpringDOY: day that marks end of "spring" (typically Jan 1 -- June 30)
# startFallDOY: day that marks start of "fall" (typically Aug 1 -- Dec 31)
.findFirstLastFrostDOY <- function(v, frostTemp=32, endSpringDOY=182, startFallDOY=213) {
  
  # 366 used to account for leap-years
  if(length(v) > 366)
    stop('this function expects a single year of data')
  
  # split year
  v.spring <- v[1:endSpringDOY]
  v.fall <- v[startFallDOY:366]
  
  # vector of DOY so that fall indexing is correct
  doy <- 1:366
  doy.spring <- doy[1:endSpringDOY]
  doy.fall <- doy[startFallDOY:366]
  
  # last spring frost: the index 
  spring.idx <- which(v.spring < frostTemp)
  # the last julian day below freezing is the last spring frost
  # if there are no days below critical temperature, then use first day of the year (1)
  last.spring.frost <- ifelse(length(spring.idx) < 1, 1, max(doy.spring[spring.idx], na.rm=TRUE))
  
  # first fall frost
  fall.idx <- which(v.fall < frostTemp)
  # the first julian day below freezing is the first fall frost
  # if there are no days below critical temperature, then use last day of the year (366)
  first.fall.frost <- ifelse(length(fall.idx) < 1, 366, min(doy.fall[fall.idx], na.rm=TRUE))
  
  res <- data.frame(
    lastSpring = last.spring.frost, 
    firstFall = first.fall.frost
  )
  
  return(res)
}


# generate a frost / not frost matrix for each year (row) in 'fl'
# fl: data.frame with last spring / first fall DOY
.makeFrostMatrix <- function(fl) {
  
  # number of years
  n <- nrow(fl)
  
  # rows are years, columns are days
  m <- matrix(0, nrow=n, ncol=366)
  
  # iterate over years and load frost matrix
  for(i in 1:n) {
    # load 1's from first of year to day of last spring frost
    m[i, 1:fl$lastSpring[i]] <- 1
    # load 1's from day of first fall frost to end of year
    m[i, fl$firstFall[i]:366] <- 1
  }
  
  # copy over year to row names
  dimnames(m) <- list(fl$year)
  
  return(m)
}


# identify the frost-free period for a single year
# d: data.frame with 'datetime' and 'value' columns
# minDays: rule for min number of days required (ea. spring|fall) for estimation
# \dots: further arguments passed to .findFirstLastFrostDOY()
# result is a data.frame with first/last frost DOY
.frostFreePeriod <- function(d, minDays, frostTemp, endSpringDOY, startFallDOY) {
  # align values with DOY in the presence of missing data
  v <- .alignDOY(d$datetime, d$value)
  
  # sanity check: need at least 164 days of data / semi-annual period
  n.spring <- length(which(!is.na(v[1:182])))
  n.fall <- length(which(!is.na(v[183:366])))
  
  if(any(c(n.spring < minDays, n.fall < minDays)))
    return(NULL)
  
  # get the last spring and first fall frost DOY
  fl <- .findFirstLastFrostDOY(v, frostTemp = frostTemp, endSpringDOY = endSpringDOY, startFallDOY = startFallDOY)
  
  return(fl) 
}


# estimate FFD using quantiles of last spring / first fall DOY
# d: data.frame with columns 'datetime' 'year', and 'value'
# returnDailyPr: optionally return list with daily summaries
# minDays: min number of days / spring|fall required for estimates


#' @title Frost-Free Day Evaluation
#' 
#' @description Evaluation frost-free days and related metrics from daily climate records.
#'
#' @param d `data.frame` with columns 'datetime' 'year', and 'value'; 'value' being daily minimum temperature, see details
#' @param returnDailyPr optionally return `list` with daily summaries
#' @param minDays min number of days of non-NA data in spring | fall, required for a reasonable estimate of FFD
#' @param frostTemp critical temperature that defines "frost" (same units as `d$value`)
#' @param endSpringDOY day of year that marks end of "spring" (typically Jan 1 -- June 30)
#' @param startFallDOY day of year that marks start of "fall" (typically Aug 1 -- Dec 31)
#' 
#' @details The default `frostTemp=32` is suitable for use with minimum daily temperatures in degrees Fahrenheit. Use `frostTemp=0` for temperatures in degrees Celsius.
#' 
#' [FFD tutorial](http://ncss-tech.github.io/AQP/sharpshootR/FFD-estimates.html)
#' 
#' @return a `data.frame` when a `returnDailyPr=FALSE`, otherwise a `list` with the following elements:
#'   * summary: FFD summary statistics as a `data.frame`
#'   * fm: frost matrix
#'   * Pr.frost: Pr(frost|day): daily probability of frost
#' 
#' @author D.E. Beaudette
#' 
#' @export
#'
#' @examples
#' 
#' # 11 years of data from highland meadows
#' data('HHM', package = 'sharpshootR')
#' x.ffd <- FFD(HHM, returnDailyPr = FALSE, frostTemp=32)
#' 
#' str(x.ffd)
#' 
FFD <- function(d, returnDailyPr = TRUE, minDays = 165, frostTemp = 32, endSpringDOY = 182, startFallDOY = 213) {
  
  ## TODO: replace ddply with split/lapply
  
  # get frost-free period for over all years
  ffp <- ddply(d, 'year', .frostFreePeriod, minDays = minDays, frostTemp = frostTemp, endSpringDOY = endSpringDOY, startFallDOY = startFallDOY)
  
  # years of data
  n.yrs <- nrow(ffp)
  
  # sanity check: if nrow(ffp) < 1 then there were not enough data
  if(n.yrs < 1)
    return(NULL)
  
  # last spring frost doy
  # 50th, 80th, 90th percentiles computed over all years
  # days where Pr(no frost) = 50%, 80%, 90%
  q.spring <- quantile(ffp$lastSpring, probs=c(0.5, 0.8, 0.9))
  
  # first fall frost doy
  # 50th, 20th, 10th percentiles computed over all years
  # days where Pr(no frost) = 50%, 80%, 90%
  q.fall <- quantile(ffp$firstFall, probs=c(0.5, 0.2, 0.1))
  
  # compute probabilistic estimates of FFD
  ffd.q <- q.fall - q.spring
  
  # combine FFD, last spring / first fall DOY, number of years data
  res <- data.frame(t(ffd.q), t(q.spring), t(q.fall), n.yrs=n.yrs)
  names(res) <- c(
    'ffd.50', 'ffd.80', 'ffd.90', 
    'spring.50', 'spring.80', 'spring.90', 
    'fall.50', 'fall.80', 'fall.90',
    'n.yrs'
  )
  
  # convert to integer DOY
  res <- round(res)
  
  # optional estimation of frost matrix / Pr(frost|day) vector
  if(returnDailyPr) {
    
    # TODO: copy year to matrix row names
    # create frost matrix
    fm <- .makeFrostMatrix(ffp)
    
    # estimate Pr(frost|day) over all years of data
    Pr.frost <- colSums(fm) / nrow(fm)
    
    # package into a list
    return(list(
      summary=res,
      fm=fm,
      Pr.frost=Pr.frost
    ))
    
  } else {
    return(res)
  }
  
  
}






#' @title Plot output from FFD()
#'
#' @param s output from [`FFD`], with `returnDailyPr = TRUE`
#' @param sub.title figure subtitle
#'
#' @return nothing, function is called to generate graphical output
#' @export
#'
#' @examples
#' 
#' # 11 years of data from highland meadows
#' data('HHM', package = 'sharpshootR')
#' x.ffd <- FFD(HHM, returnDailyPr = TRUE, frostTemp=32)
#' 
#' FFDplot(x.ffd)
#' 
FFDplot <- function(s, sub.title=NULL) {
  
  n.yrs <- nrow(s$fm)
  ffd.vals <- unlist(s$summary[, c('ffd.50', 'ffd.80', 'ffd.90')])
  q.spring <- unlist(s$summary[, c('spring.50', 'spring.80', 'spring.90')])
  q.fall <- unlist(s$summary[, c('fall.50', 'fall.80', 'fall.90')])
  prob.seq <- seq(0, 1, by=0.1)
  date.seq <- seq.Date(from=as.Date('2011-01-15'), to=as.Date('2011-12-31'), by='1 month')
  
  # device options are modified locally, reset when done
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  par(mfcol=c(1,2))
  
  image(1:366, 1:n.yrs, t(s$fm), col=c(grey(0.85), 'royalblue'), axes=FALSE, xlab='', ylab='')
  abline(v=q.spring, lty=3:1)
  abline(v=q.fall, lty=3:1)
  
  # segments(x0=q.spring[1], y0=(1:n.yrs)-0.25, x1=q.fall[1], y1=(1:n.yrs)-0.25, col='black', lty=3)
  # segments(x0=q.spring[2], y0=1:n.yrs, x1=q.fall[2], y1=1:n.yrs, col='black', lty=2)
  # segments(x0=q.spring[3], y0=(1:n.yrs)+0.25, x1=q.fall[3], y1=(1:n.yrs)+0.25, col='black', lty=1)
  # 
  abline(h=(1:n.yrs-1) + 0.5)
  
  axis(side=1, at=as.integer(format(date.seq, "%j")), labels = format(date.seq, "%b"), cex.axis=0.65)
  axis(side=2, at=1:n.yrs, labels = row.names(s$fm), las=2, cex.axis=0.85, tick=FALSE, line=-0.5)
  box()
  title('Frost-Free Period by Year')
  if(!is.null(sub.title)) title(sub=sub.title, line=2.5, font.sub=4, cex.sub=0.8)
  
  
  
  # right-hand plot: Pr(frost)
  plot(1:366, s$Pr.frost, type='l', axes=FALSE, xlab='', ylab='', lwd=2, col=grey(0.65))
  mtext(side=2, expression(Pr(frost-free, DOY)), line=2, cex=0.85)
  
  points(q.spring, c(0.5, 0.2, 0.1), col='red', pch='|', cex=1.5)
  points(q.fall, c(0.5, 0.2, 0.1), col='red', pch='|', cex=1.5)
  
  arrows(x0=q.spring, y0=c(0.5, 0.2, 0.1), x1=q.fall, y1=c(0.5, 0.2, 0.1), col='black', length=0.1, code = 3)
  
  text(q.spring, c(0.5, 0.2, 0.1), q.spring, cex=0.75, pos=2)
  text(q.fall, c(0.5, 0.2, 0.1), q.fall, cex=0.75, pos=4)
  text((q.spring + q.fall) / 2, c(0.5, 0.2, 0.1), ffd.vals, font=2, cex=0.75, pos=3)
  
  axis(side=1, at=as.integer(format(date.seq, "%j")), labels = format(date.seq, "%b"), cex.axis=0.65)
  axis(side=2, at=prob.seq, labels = paste0(1-prob.seq), las=2, cex.axis=0.75, line=0)
  box()
  title('Probability of "no-frost" by DOY')
  if(!is.null(sub.title)) title(sub=sub.title, line=2.5, font.sub=4, cex.sub=0.8)
  
}




