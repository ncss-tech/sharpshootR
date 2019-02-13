
# cumulative PPT within a single water year
.cumulativePPT <- function(i) {
  i$cumulative_ppt <- cumsum(ifelse(is.na(i$value), 0, i$value))
  # number of days in summation
  i$n <- nrow(i)
  
  ## TODO: subset to rows of interest
  return(i)
}

# compute cumulative PPT by water year
.prepareCumulativePPT <- function(d) {
  ## NOTE: requires sharpshootR >= 1.4.02
  # re-order just in case
  d <- d[order(d$water_year, d$water_day), ]
  
  # compute cumulative PPT by water year, ordered by water day
  dd <- lapply(split(d, d$water_year), .cumulativePPT)
  dd <- do.call('rbind', dd)
  
  return(dd)
}



# get years and total PPT which are closest to given percentiles of annual PPT
.exemplarYears <- function(d) {
  
  # annual PPT by water year
  ppt.by.wy <- tapply(d$cumulative_ppt, d$water_year, max, na.rm=TRUE)
  # interesting percentiles of annual PPT
  ppt.q <- quantile(ppt.by.wy, probs=c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE)
  
  # find exemplars for select percentiles
  ppt.abs.diff <- abs(outer(ppt.by.wy, ppt.q, FUN = "-"))
  ppt.closest.yrs <- apply(ppt.abs.diff, 2, which.min)
  
  # this is a named vector of annual PPT (names are water_year)
  exemplar.data <- ppt.by.wy[ppt.closest.yrs]
  
  return(exemplar.data)
}


## TODO: generalize to other sources of data: SCAN / SNOTEL / Henry / etc.
## TODO: add percentiles-by-water-day method

# ugh, terrible name
# percentiles of cumulative precipitation plot
# x: result from CDECquery for now, will need to generalize to other sources
# this.year: current year
# method: exemplar|daily
# ...: further arguments to plot()
PCP_plot <- function(x, this.year=2019, method='exemplar', q.color='RoyalBlue', c.color='firebrick', ...) {
  
  # water year range
  wy.range <- range(x$water_year)
  
  # prepare data for plotting / extract summaries
  xx <- .prepareCumulativePPT(x)
  e <- .exemplarYears(xx)
  
  # convenience objects for plotting 
  exemplar.yrs <- as.numeric(names(e))
  this.year.data <- xx[xx$water_year == this.year, ]
  
  # reasonable axis in "real" dates
  date.axis <- data.frame(d=seq.Date(from=as.Date('2000-10-01'), to=as.Date('2001-09-30'), by = '2 weeks'))
  
  # integrate water year / day
  wyd <- waterDayYear(date.axis$d)
  date.axis$wr <- wyd$wy
  date.axis$wd <- wyd$wd
  
  # generic labeling of months and days
  date.axis$lab <- format(date.axis$d, "%d\n%b")
  
  
  # all data, establish plot area
  xlab.text <- sprintf("Water Years %s - %s", wy.range[1], wy.range[2])
  plot(cumulative_ppt ~ water_day, data=xx, col=grey(0.9), type='n', axes=FALSE, xlim=c(0, 370), xlab=xlab.text, ... )
  
  # grid
  grid(ny = NULL, nx=0, col='lightgray', lty=3)
  abline(v=date.axis$wd, col='lightgray', lty=3)
  
  # exemplar years based on quantiles
  lines(cumulative_ppt ~ water_day, data=xx[xx$water_year == exemplar.yrs[1], ], lwd=1, lty=3, col=q.color, type='l')
  lines(cumulative_ppt ~ water_day, data=xx[xx$water_year == exemplar.yrs[2], ], lwd=1, lty=2, col=q.color, type='l')
  lines(cumulative_ppt ~ water_day, data=xx[xx$water_year == exemplar.yrs[3], ], lwd=2, lty=1, col=q.color, type='l')
  lines(cumulative_ppt ~ water_day, data=xx[xx$water_year == exemplar.yrs[4], ], lwd=1, lty=2, col=q.color, type='l')
  lines(cumulative_ppt ~ water_day, data=xx[xx$water_year == exemplar.yrs[5], ], lwd=1, lty=3, col=q.color, type='l')
  
  # current year
  lines(cumulative_ppt ~ water_day, data=this.year.data, lwd=2, col=c.color, type='l')
  
  
  # add axes
  axis(side=1, at = date.axis$wd, labels = date.axis$lab, cex.axis=0.55, las=1)
  axis(side=2, las=1)
  
  # annotate exemplar years
  text(x = 365, y=e, labels = as.character(exemplar.yrs), pos=1, cex=0.75, font=3)
  text(x = 365, y=e, labels = as.character(round(e)), pos=4, cex=0.65, font=2)
  
  # current year positional elements
  mwd <- max(this.year.data$water_day)
  mcp <- max(this.year.data$cumulative_ppt)
  
  # TODO: add current percentile via ecdf()
  # annotate current year
  points(x=mwd, y=mcp, pch=22, bg=c.color)
  text(x=0, y=mcp, labels = round(mcp), col=c.color, font=2, cex=0.85)
  
  # helper arrows
  # TODO: condition on mwd: not helpful if < 10
  arrows(x0 = 10, y0 = mcp, x1 = mwd, y1 = mcp, col = c.color, length = 0.05, code=1)
  
  # TODO: return information used to make figure
}


## alternative approach: percentiles by water day, over all water years
## NOT monotonic functions

# # TODO: would be nice to know number of years
# # TODO: filtering on number of days in summation?
# # percentiles over all years, by water day
# .PPT_pctiles <- function(i) {
#   res <- quantile(i$cumulative_ppt, probs=c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE)
#   return(res)
# }
# 
# ## not quite right, leap years aren't adequately handled     
# xx <- ddply(x, 'water_year', .fun=.cumulativePPT)
# xxx <- ddply(xx, 'water_day', .fun=.PPT_pctiles)  
# 
# zz <- subset(xx, subset=water_year == 2019)
# zzz <- ddply(zz, 'water_day', .fun=.PPT_pctiles)  
# 
# # almost right
# matplot(xxx[1:365, 1], xxx[1:365, -1], type='l', col='RoyalBlue', 
#         lwd=c(1,1,2,1,1), lty=c(3,2,1,2,3), 
#         las=1, xlab='Water Day', ylab='Cumulative PPT (inches)', 
#         main='Sonora Ranger Station (SOR)\n1981--2019')
# 
# matlines(zzz[1:365, 1], zzz[1:365, 4], lwd=2, col='firebrick')


