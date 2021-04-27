library(aqp)
library(soilDB)
library(sharpshootR)
library(hydromad)

## get basic morphology and series-level summaries of climate etc.
# http://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.html

s <- 'Redding'

x <- fetchOSD(s, extended = TRUE)

# get representative, profile-total AWC from SSURGO
sql <- sprintf("SELECT chorizon.cokey AS cokey, 
SUM(awc_r * (hzdepb_r - hzdept_r)) AS ws 
FROM component 
JOIN chorizon ON component.cokey = chorizon.cokey 
WHERE compname = '%s' 
GROUP BY chorizon.cokey;", s)


# get via SDA
res <- SDA_query(sql)

# median AWC in mm
# over all components correlated to named series 
AWC <- round(median(res$ws, na.rm = TRUE) * 10)


# monthly climate data from series summary
PPT <- x$climate.monthly$q05[x$climate.monthly$variable == 'Precipitation (mm)']
PET <- x$climate.monthly$q50[x$climate.monthly$variable == 'Potential ET (mm)']

# tighter margins
par(mar=c(4,4,2,1), bg = 'white')

# water year
# last iteration
x.wb <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 1, rep = 3, keep_last = TRUE)
plotWB(WB = x.wb)


d <- x.wb




# colors
col.ppt <- rgb(0, 0, 1, alpha=0.25)
col.pet <- rgb(1, 0, 0, alpha=0.25)

# generate interpolation functions for estimating intersection
# approxfun - linear interpolation
# splinefun - spline interpolation
ppt.interp <- approxfun(d$month, d$PPT)
pet.interp <- approxfun(d$month, d$PET)
aet.interp <- approxfun(d$month, d$ET)

# need to figure out range from data
y.range <- range(c(d$PET, d$PPT))

# interpolate between month centers
month.start <- 1
month.end <- 12
month.seq <- seq(from=month.start, to=month.end, by=0.1)
ppt.seq <- ppt.interp(month.seq)
pet.seq <- pet.interp(month.seq)
aet.seq <- aet.interp(month.seq)

# locate crossings - isolate area of intersection where PET > PPT
surplus_deficit.flag <- sign(ppt.seq - pet.seq)
crossings.idx <- which(abs(diff(surplus_deficit.flag)) > 0)
# add first and last indices for boundaries - not necessary!
#crossings.idx <- c(1, crossings.idx, length(month.seq))
# locate additional crossings - isolate the intersection of AET > PPT
surplus_deficit.flag <- sign(ppt.seq - aet.seq)
crossings1.idx <- which(abs(diff(surplus_deficit.flag)) > 0)

# setup plot area
plot(0, 0, type='n', xlim=c(0.5, 12.5), ylim=c(y.range), ylab='Water (mm)', xlab='', las = 1, axes = FALSE)

grid()

# iterate over crossings
for(i in 1:(length(crossings.idx) - 1)) {
  
  # determine PPT and PET total between crossings
  ppt.i <- integrate(ppt.interp, lower=month.seq[crossings.idx[i]], upper=month.seq[crossings.idx[i+1]])
  pet.i <- integrate(pet.interp, lower=month.seq[crossings.idx[i]], upper=month.seq[crossings.idx[i+1]])
  
  # generate color based on PPT surplus / deficit
  if((ppt.i$value - pet.i$value) > 0)
    col.i <- NA else col.i <- col.pet
  
  # compute x and y coordinates for polygon defined by PPT and PET functions
  p.1.x <- month.seq[crossings.idx[i]:crossings.idx[i+1]]
  p.1.y <- ppt.interp(p.1.x)
  p.2.x <- rev(p.1.x)
  p.2.y <- pet.interp(p.2.x)
  
  # add polygon + color
  polygon(c(p.1.x, p.2.x), c(p.1.y, p.2.y), col=col.i, border=NA)
}

# this shades all areas under the PPT line
p.1.x <- month.seq
p.1.y <- rep(1, length(month.seq))
p.2.x <- rev(p.1.x)
p.2.y <- ppt.interp(p.2.x)
polygon(c(p.1.x, p.2.x), c(p.1.y, p.2.y), col=col.ppt, border=NA)

# shades the AET > PPT polygon - determine color scheme to make this area look like area under PPT line, uncolor then re-shade it?
p.1.x <- month.seq[crossings1.idx[1]:crossings1.idx[2]]
p.1.y <- aet.interp(p.1.x)
p.2.x <- rev(p.1.x)
p.2.y <- ppt.interp(p.2.x)
# set area to no color then shade area the same color as PPT area
polygon(c(p.1.x, p.2.x), c(p.1.y, p.2.y), col=0, border=NA)
polygon(c(p.1.x, p.2.x), c(p.1.y, p.2.y), col=col.ppt, border=NA)

# add original PPT and PET data
lines(ppt.seq ~ month.seq, type='l', lwd=2, col='blue')
lines(pet.seq ~ month.seq, type='l', lwd=2, lty=2, col='brown')
lines(aet.seq ~ month.seq, type='l', lwd=2, lty=4, col='black')

## TODO: adapt to use function arguments
# month axis
month.cex <- 1
axis(side = 1, at = month.start:month.end, labels = d$mo, line = 0, tick = TRUE, font = 2, cex = month.cex, col = NA, col.ticks = par('fg'))


## TODO: figure out how to place these automatically
text(11,50,c("Recharge"))
text(1.5,37, c("Surplus"))
text(7.5,70, c("Deficit"))


# add legend
legend('topleft', legend=c('Soil Moisture Gain', 'Soil Moisture Loss', 'PPT', 'PET', 'AET'), col=c(col.ppt, col.pet, 'blue', 'brown', 'black'), pch=c(15, 15, NA, NA, NA), pt.cex=2, lwd=c(NA, NA, 2, 2, 2), lty=c(NA, NA, 1, 2, 4), bty='n')

#add title
title(sprintf('Annual Water Balance: %s Series', toupper(s)))



