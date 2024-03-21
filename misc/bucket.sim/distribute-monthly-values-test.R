library(hydromad)


## near Sonora, CA
S_0 <- 1
AWC <- 100
PET <- c(0, 0, 5,80, 90, 120, 130, 140, 110, 90, 20, 5)
PPT <- c(0, 150, 200, 120, 20, 0, 0, 0, 10, 20, 30, 60)


## huge pulse of rain, constant PET
S_0 <- 1
AWC <- 100
PPT <- c(100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
PET <- c(0, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20)

## 
S_0 <- 1
AWC <- 198
PPT <- c(0, 0, 0, 0, 60, 100, 70, 0, 0, 0, 0, 0)
PET <- c(0, 0, 0, 0, 120, 120, 60, 0, 0, 0, 0, 0)


## huge pulse of rain, and deficit beyond AWC? 
## results seem wrong
S_0 <- 1
AWC <- 100
PPT <- c(20, 0, 0, 0, 0, 0, 0, 100, 0, 0, 0, 0)
PET <- c(0, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20)


## Coweeta
S_0 <- 1
AWC <- 75
PPT <- c(200, 200, 183, 159, 158, 164, 140, 139, 173, 140, 204, 194)
PET <- c(4, 9, 27, 52, 83, 111, 125, 116, 83, 50, 23, 8)

# LUCY soil series data
S_0 <- 0
AWC <- 207
PPT <- c(98, 88, 99, 72, 65, 99, 107, 97, 85, 66, 70, 82)
PET <- c(12, 18, 40, 65, 113, 151, 171, 157, 115, 66, 33, 15)

# LUCY soil series data, constant PPT
# S_0 <- 0
# AWC <- 207
# PPT <- rep(60, times = 12)
# PET <- c(12, 18, 40, 65, 113, 151, 171, 157, 115, 66, 33, 15)


w1 <- monthlyWB(AWC = AWC, PPT = PPT, PET = PET, S_init = S_0, rep = 3, keep_last = TRUE, distribute = FALSE)
attr(w1, 'mass.balance')

w2 <- monthlyWB(AWC = AWC, PPT = PPT, PET = PET, S_init = S_0, rep = 3, keep_last = TRUE, distribute = TRUE, k = 10, method = 'equal')
attr(w2, 'mass.balance')

w3 <- monthlyWB(AWC = AWC, PPT = PPT, PET = PET, S_init = S_0, rep = 3, keep_last = TRUE, distribute = TRUE, k = 10, method = 'random')
attr(w3, 'mass.balance')

par(mfrow = c(1, 2), mar = c(4, 3.5, 3, 2))
plotWB(w1, legend.cex = 0.7, month.cex = 0.8) ; title('Monthly Totals', cex.main = 1)
plotWB(w2, legend.cex = 0.7, month.cex = 0.8) ; title('Distributed', cex.main = 1)





knitr::kable(
  lattice::make.groups(
    monthly = monthlyWB_summary(w1),
    distributed = monthlyWB_summary(w2)
  ), digits = 0
)




par(mfrow = c(1, 3))

plotWB(w1, legend.cex = 0.7, month.cex = 0.8) ; title('Monthly Totals', cex.main = 1)
plotWB(w2, legend.cex = 0.7, month.cex = 0.8) ; title('Distributed', cex.main = 1)
plotWB(w3, legend.cex = 0.7, month.cex = 0.8) ; title('Distributed / Random', cex.main = 1)


plotWB_lines(w1)
plotWB_lines(w2)
plotWB_lines(w3)




## water year check
library(soilDB)

s <- 'amador'
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
PPT <- x$climate.monthly$q50[x$climate.monthly$variable == 'Precipitation (mm)']
PET <- x$climate.monthly$q50[x$climate.monthly$variable == 'Potential ET (mm)']


w1 <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 9, rep = 3, keep_last = TRUE)
w2 <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 9, rep = 3, keep_last = TRUE, distribute = TRUE)

par(mfrow = c(1, 2), mar = c(4, 3.5, 3, 2))
plotWB(w1, legend.cex = 0.7, month.cex = 0.8) ; title('Monthly Totals', cex.main = 1)
plotWB(w2, legend.cex = 0.7, month.cex = 0.8) ; title('Distributed', cex.main = 1)



w1 <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 1, rep = 3, keep_last = TRUE)
w2 <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 1, rep = 3, keep_last = TRUE, distribute = TRUE)

par(mfrow = c(1, 2), mar = c(4, 3.5, 3, 2))
plotWB(w1, legend.cex = 0.7, month.cex = 0.8) ; title('Monthly Totals', cex.main = 1)
plotWB(w2, legend.cex = 0.7, month.cex = 0.8) ; title('Distributed', cex.main = 1)

knitr::kable(
  lattice::make.groups(
    monthly = monthlyWB_summary(w1),
    distributed = monthlyWB_summary(w2)
  ), digits = 0
)



