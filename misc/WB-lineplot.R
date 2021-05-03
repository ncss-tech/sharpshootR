library(aqp)
library(soilDB)
library(sharpshootR)
library(hydromad)

## get basic morphology and series-level summaries of climate etc.
# http://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.html

## useful examples:
# BOONE: no deficit
# 


s <- 'auburn'

x <- fetchOSD(s, extended = TRUE)

# get representative, profile-total AWC from SSURGO
sql <- sprintf("SELECT chorizon.cokey AS cokey, 
SUM(awc_r * (hzdepb_r - hzdept_r)) AS ws 
FROM 
legend JOIN mapunit ON legend.lkey = mapunit.lkey
JOIN component ON mapunit.mukey = component.mukey
JOIN chorizon ON component.cokey = chorizon.cokey 
WHERE compname = '%s'
AND areasymbol != 'US'
GROUP BY chorizon.cokey;", s)


# get via SDA
res <- SDA_query(sql)

# median AWC in mm
# over all components correlated to named series 
AWC <- round(median(res$ws, na.rm = TRUE) * 10)


# monthly climate data from series summary
PPT <- x$climate.monthly$q05[x$climate.monthly$variable == 'Precipitation (mm)']
PET <- x$climate.monthly$q50[x$climate.monthly$variable == 'Potential ET (mm)']

# 3 warm-up cycles
# keep last iteration
# calendar year
x.wb <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 1, rep = 3, keep_last = TRUE)

# tighter margins
par(mar=c(4,4,3,1), bg = 'white')

plotWB(x.wb)


## TODO: only works with calendar year
plotWB_lines(x.wb)

#add title
title(sprintf('Monthly Water Balance: %s Series', toupper(s)), line = 2)



