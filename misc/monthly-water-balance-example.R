library(aqp)
library(soilDB)
library(sharpshootR)
library(hydromad)


## overly simplistic simulation

AWC <- 50
PPT <- rep(5, times = 12)
PET <- rep(10, times = 12)

x <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 1, rep = 1, keep_last = TRUE)

x

plotWB(x)

# note spline overshoots
plotWB_lines(x, interpolator = 'spline')
plotWB_lines(x, interpolator = 'linear')

# important: ET is scaled by VWC




## get basic morphology and series-level summaries of climate etc.
# http://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.html

s <- 'pierre'

x <- fetchOSD(s, extended = TRUE)

# get representative, profile-total AWC from SSURGO
sql <- sprintf("
SELECT chorizon.cokey AS cokey, 
SUM(awc_r * (hzdepb_r - hzdept_r)) AS ws 
FROM 
legend JOIN mapunit ON legend.lkey = mapunit.lkey
JOIN component ON mapunit.mukey = component.mukey
JOIN chorizon ON component.cokey = chorizon.cokey 
WHERE compname = '%s'
AND areasymbol != 'US'
GROUP BY chorizon.cokey;", s
)


# get via SDA
res <- SDA_query(sql)

# median AWC in mm
# over all components correlated to named series 
AWC <- round(median(res$ws, na.rm = TRUE) * 10)


# monthly climate data from series summary
PPT <- x$climate.monthly$q05[x$climate.monthly$variable == 'Precipitation (mm)']
PET <- x$climate.monthly$q50[x$climate.monthly$variable == 'Potential ET (mm)']

# tighter margins
par(mar=c(4,4,3,1), bg = 'white')

# water year
# last iteration
x.wb <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 9, rep = 3, keep_last = TRUE)
plotWB(WB = x.wb)

# convert total ETa into inches
sum(x.wb$ET) * 0.03937


# calendar year, 3 cycles, keeping all results
x.wb <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 1, rep = 3)
x.wb[x.wb$mo == 'Jan', ]
plotWB(WB = x.wb)

# calendar year
# last iteration
x.wb <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 1, rep = 3, keep_last = TRUE)
plotWB(WB = x.wb)

plotWB_lines(x.wb)
title('Pierre Soil Series', line = 2)
