## functions related to the estimation of FFD


# many calculations need and array of 365 values, some may be NA
# sometime missing records result in < 365 values / year
# this function will align the data to vector of length 365
# 
# dt: anything that can be coerced to a Date class object
# value: vector of values associated with a single year
# fill: attempt to estimate NA (not implemented)
alignDOY <- function(dt, value, fill=FALSE) {
  
  # 366 used to account for leap-years
  if(length(value) > 366)
    stop('this function expects a single year of data')
  
  # DOY template: extend to 366 days for leap-years
  doy.template <- vector(mode = 'numeric', length = 366)
  doy.template <- rep(NA, times=366)
  
  # get DOY from source data
  doy <- as.integer(format(as.Date(dt), "%j"))
  
  # instert data into doy template via doy index
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
findFirstLastFrostDOY <- function(v, frostTemp=32, endSpringDOY=182, startFallDOY=213) {
  
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
  
  return(data.frame(lastSpring=last.spring.frost, firstFall=first.fall.frost))
}


# generate a frost / not frost matrix for each year (row) in 'fl'
# fl: data.frame with last spring / first fall DOY
makeFrostMatrix <- function(fl) {
  
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
# \dots: further arguments passed to findFirstLastFrostDOY()
# result is a data.frame with first/last frost DOY
frostFreePeriod <- function(d, minDays=165, ...) {
  # align values with DOY in the presence of missing data
  v <- alignDOY(d$datetime, d$value)
  
  # sanity check: need at least 164 days of data / semi-annual period
  n.spring <- length(which(!is.na(v[1:182])))
  n.fall <- length(which(!is.na(v[183:366])))
  
  if(any(c(n.spring < minDays, n.fall < minDays)))
    return(NULL)
  
  # get the last spring and first fall frost DOY
  fl <- findFirstLastFrostDOY(v, ...)
  
  return(fl) 
}


# estimate FFD using quantiles of last spring / first fall DOY
# d: data.frame with columns 'datetime' 'year', and 'value'
# returnDailyPr: optionally return list with daily summaries
# minDays: min number of days / spring|fall required for estimates
FFD <- function(d, returnDailyPr=TRUE, minDays=165, ...) {
  
  # get frost-free period for over all years
  ffp <- ddply(d, 'year', frostFreePeriod, minDays=minDays, ...)
  
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
  
  # compute probabalistic estimates of FFD
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
    fm <- makeFrostMatrix(ffp)
    
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






FFDplot <- function(s, title='') {
  
  n.yrs <- nrow(s$fm)
  ffd.vals <- unlist(s$summary[, c('ffd.50', 'ffd.80', 'ffd.90')])
  q.spring <- unlist(s$summary[, c('spring.50', 'spring.80', 'spring.90')])
  q.fall <- unlist(s$summary[, c('fall.50', 'fall.80', 'fall.90')])
  prob.seq <- seq(0, 1, by=0.1)
  
  par(mar=c(3,3.5,3,1), mfcol=c(1,2))
  
  image(1:366, 1:n.yrs, t(s$fm), col=c(grey(0.85), 'royalblue'), axes=FALSE, xlab='', ylab='')
  abline(v=q.spring, lty=3:1)
  abline(v=q.fall, lty=3:1)
  
  # adj.params <- c(1, 1, -0.5)
  # for(i in 1:3)
  #   text(q.spring[i], n.yrs-i, q.spring[i], cex=0.65, adj=adj.params[i])
  # 
  # for(i in 1:3)
  #   text(q.fall[i], n.yrs-i, q.fall[i], cex=0.65, adj=rev(adj.params)[i])
  
  
  axis.Date(side=1, seq.Date(from=as.Date('2011-01-01'), to=as.Date('2011-12-31'), by='2 weeks'), cex.axis=0.85)
  axis(side=2, at=1:n.yrs, labels = row.names(s$fm), las=2, cex.axis=0.85)
  title(title)
  
  # right-hand plot: Pr(frost)
  plot(1:366, s$Pr.frost, type='l', axes=FALSE, xlab='', ylab='', lwd=2, col=grey(0.65))
  mtext(side=2, expression(Pr(frost-free, DOY)), line=2, cex=0.85)
  
  points(q.spring, c(0.5, 0.2, 0.1), col='red', pch='|', cex=1.5)
  points(q.fall, c(0.5, 0.2, 0.1), col='red', pch='|', cex=1.5)
  
  arrows(x0=q.spring, y0=c(0.5, 0.2, 0.1), x1=q.fall, y1=c(0.5, 0.2, 0.1), col='black', len=0.1, code = 3)
  
  text(q.spring, c(0.5, 0.2, 0.1), q.spring, cex=0.75, pos=2)
  text(q.fall, c(0.5, 0.2, 0.1), q.fall, cex=0.75, pos=4)
  text((q.spring + q.fall) / 2, c(0.5, 0.2, 0.1), ffd.vals, font=2, cex=0.75, pos=3)
  
  axis.Date(side=1, seq.Date(from=as.Date('2011-01-01'), to=as.Date('2011-12-31'), by='1 month'), cex.axis=0.85)
  axis(side=2, at=prob.seq, labels = paste0(1-prob.seq), las=2, cex.axis=0.75, line=-0.5)
  title(title)
  
}




