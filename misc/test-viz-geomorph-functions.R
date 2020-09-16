library(soilDB)
library(sharpshootR)
library(reshape2)

s <- 'honcut'
ss <- siblings(s)
z <- fetchOSD(c(s, ss$sib$sibling), extended = TRUE)

vizHillslopePosition(z$hillpos, s = s)
vizGeomorphicComponent(z$geomcomp, s = s)
vizFlatsPosition(z$flats, s = s)
vizTerracePosition(z$terrace, s = s)

tab <- soilDB.tabulateGeomorphRecords(z)
round(sweep(tab[, -1], MARGIN = 1, STATS = rowSums(tab[, -1]), FUN = '/'), 3)

knitr::kable(m5)
