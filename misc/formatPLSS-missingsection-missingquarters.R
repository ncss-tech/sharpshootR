library(sp)
library(sharpshootR)

d <- data.frame(
  id = 1:3,
  qq = c('SW', NA, NA),
  q = c('NE', NA, NA),
  s = c(17, NA, 30),
  t = c('T36N', 'T35N', 'T35N'),
  r = c('R29W', 'R28W', 'R28W'),
  type = 'SN',
  m = 'MT20',
  stringsAsFactors = FALSE
)
  
# get centroids for PLSS extents
res1 <- PLSS2LL(data.frame(id = 1:3, plssid = formatPLSS(d)))
res1

# returns quarter-quarter section at whatever centroid was returned
res2 <- LL2PLSS(res1$lon, res1$lat)
res2
plot(res2$geom)
