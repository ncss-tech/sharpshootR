library(aqp)
library(soilDB)

s <- 'amador'

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

## opened issue
# https://github.com/josephguillaume/hydromad/issues/188

## bug? in leaky bucket?
# ET should not be greater than PPT when S = 0
# see Aug, Sep, Oct
monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 1, rep = 3, keep_last = TRUE)

# try fixed (?) version
.leakyBucket(data.frame(P = PPT, E = PET), Sb = AWC, fc = 1, S_0 = 0, a.ss = 0.001, M = 0, etmult = 1, a.ei = 0)

# https://github.com/josephguillaume/hydromad/blob/master/R/bucket.R
# problem occurs here:
#
# Ebare <- (1 - M) * (S[t] / Sb) * E[t]
#                     ^^^^^^^^^^^^^^^^^   <- should be truncated at S[t]


dput(d)


m <- hydromad::hydromad(d, sma = "bucket", routing = NULL)
m <- update(m, Sb = 47, fc = 1, S_0 = 1, a.ss = 0.01, M = 0, etmult = 1, a.ei = 0)
res <- predict(m, return_state = TRUE)

# combine original PPT,PET with results
res <- data.frame(d, res)

# cleanup names
names(res) <- c('PPT', 'PET', 'U', 'S', 'ET')

res








