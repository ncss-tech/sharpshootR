require(sp))

# BLM PLSS API needs a long timeout
options(timeout = 60000)

# create coordinates
x <- -115.3823
y <- 48.88228

# fetch PLSS geometry for these coordinates
p.plss <- LL2PLSS(x, y)

# plot geometry
if (length(p.plss$geom) > 0)
  plot(p.plss$geom)


create some data
d <- data.frame(
  id = 1:3,
  qq = c('SW', 'SW', 'SE'),
  q = c('NE', 'NW', 'SE'),
  s = c(17, 32, 30),
  t = c('T36N', 'T35N', 'T35N'),
  r = c('R29W', 'R28W', 'R28W'),
  type = 'SN',
  m = 'MT20',
  stringsAsFactors = FALSE
)

# generate formatted PLSS codes
d$plssid <- formatPLSS(d)

# fetch lat/long coordinates
PLSS2LL(d)

