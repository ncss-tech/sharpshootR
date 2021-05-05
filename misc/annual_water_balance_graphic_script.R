# generate plot of annual water balance data

# load water balance data
d <- read.csv('J:\\water_balance_data\\recent_data\\MT632_summarized_monthly_WB_all.txt', stringsAsFactors=FALSE)

# select for individual sites
d <- d[1:12, ]
d <- d[which(d$site_usiteid == '05MT637099024'), ]
d <- d[which(d$site_usiteid == '02MT637111040'), ]
d <- d[which(d$site_usiteid == '08MT637016008'), ]

# colors
col.ppt <- rgb(0, 0, 1, alpha=0.25)
col.pet <- rgb(1, 0, 0, alpha=0.25)

# generate interpolation functions for estimating intersection
# approxfun - linear interpolation
# splinefun - spline interpolation
ppt.interp <- approxfun(d$month, d$sump)
pet.interp <- approxfun(d$month, d$sumpet)
aet.interp <- approxfun(d$month, d$sumet)

# need to figure out range from data
y.range <- range(c(d$sumpet, d$sump))

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
plot(0, 0, type='n', xlim=c(0.5, 12.5), ylim=c(y.range), ylab='Water (mm)', xlab='Months')

grid()
text(11,50,c("Recharge"))
text(1.5,37, c("Surplus"))
text(7.5,70, c("Deficit"))

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

# add legend
legend('topleft', legend=c('Soil Moisture Gain', 'Soil Moisture Loss', 'PPT', 'PET', 'AET'), col=c(col.ppt, col.pet, 'blue', 'brown', 'black'), pch=c(15, 15, NA, NA, NA), pt.cex=2, lwd=c(NA, NA, 2, 2, 2), lty=c(NA, NA, 1, 2, 4), bty='n')

#add title
title(paste(c('Annual Water Balance for Site', levels(factor(d$site_usiteid)), sep=' '))) 




# additional aggregated plots
matplot(0, 0, type='n', xlim=c(0.5, 12.5), ylim=c(y.range), ylab='Water (mm)', xlab='Months')

matplot(data$month, data$sumpet, type='l', col=rgb(0,0,0, alpha=0.5), lty=1, ylab='Monthly Deficit (mm)', xlab='Month')
matplot(data$month, data$sump, type='l', col=rgb(0,0,0, alpha=0.5), lty=1, ylab='Monthly Deficit (mm)', xlab='Month')
matplot(d$month, d$sumet, type='l', col=rgb(0,0,0, alpha=0.5), lty=1, ylab='Monthly Deficit (mm)', xlab='Month')
matplot(d$month, d$sumd, type='l', col=rgb(0,0,0, alpha=0.5), lty=1, ylab='Monthly Deficit (mm)', xlab='Month')

lines(d$month, d$sumd)
lines(d$month, d$sumet)
lines(d$month, d$sumpet)
lines(d$month, d$sump)
      
xyplot(d$month ~ d$sumd, data=d, type='n', xlim=c(0.5, 12.5), ylim=c(y.range), ylab='Water (mm)', xlab='Months')
lines(month ~ sumd, data=d)



      