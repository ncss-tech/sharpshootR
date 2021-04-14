library(aqp)
library(soilDB)
library(sharpshootR)
library(hydromad)

## get basic morphology and series-level summaries of climate etc.
# http://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.html

s <- 'drummer'


x <- fetchOSD(s, extended = TRUE)

# get representative, profile-total AWC from SSURGO
sql <- sprintf("
SELECT chorizon.cokey AS cokey, 
SUM(awc_r * (hzdepb_r - hzdept_r)) AS ws 
FROM component 
JOIN chorizon ON component.cokey = chorizon.cokey 
WHERE compname = '%s' 
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
par(mar=c(4,4,2,1), bg = 'white')

# water-year
x.wb <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 9, rep=3)
x.wb[x.wb$mo == 'Sep', ]

# plot
plotWB(WB = x.wb, AWC = AWC, showAWC = 'above')
plotWB(WB = x.wb, AWC = AWC, showAWC = 'below')


# water year
# last iteration
x.wb <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 9, rep = 3, keep_last = TRUE)
plotWB(WB = x.wb, AWC = AWC)

# convert total ETa into inches
sum(x.wb$ET) * 0.03937


# calendar year
x.wb <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 1, rep = 5)
x.wb[x.wb$mo == 'Jan', ]
plotWB(WB = x.wb, AWC = AWC, showAWC = 'above')
plotWB(WB = x.wb, AWC = AWC, showAWC = 'below')

# calendar year
# last iteration
x.wb <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 1, rep = 3, keep_last = TRUE)
plotWB(WB = x.wb, AWC = AWC, showAWC = 'above')
plotWB(WB = x.wb, AWC = AWC, showAWC = 'below')


