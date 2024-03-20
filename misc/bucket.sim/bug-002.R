
# https://github.com/ncss-tech/sharpshootR/issues/54

# https://github.com/josephguillaume/hydromad/issues/190


# test with Morley WB:
# https://casoilresource.lawr.ucdavis.edu/sde/?series=morley#water-balance


library(sharpshootR)
library(hydromad)
library(aqp)
library(soilDB)


## original test case, MORLEY series

s <- 'morley'
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

S_0 <- 1


w <- monthlyWB(AWC = AWC, PPT = PPT, PET = PET, S_init = S_0, rep = 1, keep_last = TRUE, distribute = FALSE)
plotWB(w)

attr(w, 'mass.balance')




# initial conditions
S_0 <- 1
AWC <- 198
PPT <- c(0, 0, 0, 0, 60, 100, 70, 0, 0, 0, 0, 0)
PET <- c(0, 0, 0, 0, 120, 120, 60, 0, 0, 0, 0, 0)


S_0 <- 1
AWC <- 200
PPT <- c(0, 0, 0, 0, 60, 100, 70, 0, 0, 0, 0, 0)
PET <- c(0, 0, 0, 0, 120, 120, 120, 0, 0, 0, 0, 0)


# 
S_0 <- 1
AWC <- 100
PPT <- c(20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
PET <- c(0, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20)

# #
# S_0 <- 1
# AWC <- 100
# PPT <- c(0, 0, 0, 0, 0, 100, 0, 0, 0, 0, 0, 0)
# PET <- c(0, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20)





hydromad.options('pure.R.code' = TRUE)

d <- data.frame(P = PPT, E = PET)

m <- hydromad::hydromad(d[, 1:2], sma = "bucket", routing = NULL)

# start with "full" bucket
m <- update(m, Sb = AWC, fc = 1, S_0 = S_0, a.ss = 0.01, M = 0, etmult = 1, a.ei = 0)
res <- predict(m, return_state = TRUE)

# combine climate inputs + water balance
res <- cbind(d, res)

# compute deficit
res$D <- res$ET - res$E

knitr::kable(res)


## test mass balance

# P = U + ET + dS
# for all time steps: .in = .out + dS
# final mass balance: sum(.in - (.out + .dS))

# PPT
.in <- res$P

# AET + U
.out <- res$ET + res$U

# dS
# more complicated than seems, when S_init < 1
# S at time 0 = AWC * S_init 
.dS <- diff(c(AWC * S_0, res$S))

# mass balance
(.mb <- sum(.in - (.out + .dS)))




w <- monthlyWB(AWC = AWC, PPT = PPT, PET = PET, S_init = S_0, rep = 1, keep_last = TRUE, distribute = TRUE)
plotWB(w)

attr(w, 'mass.balance')
attr(w, 'AWC')







