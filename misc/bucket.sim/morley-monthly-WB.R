

library(soilDB)
library(hydromad)

s <- 'morley'
x <- fetchOSD(s, extended = TRUE)

AWC <- 198

# monthly climate data from series summary
PPT <- x$climate.monthly$q50[x$climate.monthly$variable == 'Precipitation (mm)']
PET <- x$climate.monthly$q50[x$climate.monthly$variable == 'Potential ET (mm)']

# 3 warm-up cycles
# keep last iteration
# calendar year
x.wb <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 1, rep = 3, keep_last = TRUE)

# tighter margins
par(mar=c(4,4,3,1), bg = 'white')

plotWB(x.wb)
title(sprintf('Monthly Water Balance: %s Series', toupper(s)), line = 2)

PPT <- c(0, 0, 0, 0, 60, 100, 70, 0, 0, 0, 0, 0)
PET <- c(0, 0, 0, 0, 120, 120, 60, 0, 0, 0, 0, 0)

x.wb <- monthlyWB(AWC, PPT, PET, S_init = 1, starting_month = 1)

# tighter margins
par(mar=c(4,4,3,1), bg = 'white')

plotWB(x.wb)
title(sprintf('Monthly Water Balance: %s Series', toupper(s)), line = 2)
