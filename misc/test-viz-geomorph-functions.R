library(soilDB)
library(sharpshootR)


s <- 'redding'
ss <- siblings(s)
z <- fetchOSD(c(s, ss$sib$sibling), extended = TRUE)

vizHillslopePosition(z$hillpos, s = s)
vizGeomorphicComponent(z$geomcomp, s = s)

s <- 'columbia'
ss <- siblings(s)
z <- fetchOSD(c(s, ss$sib$sibling), extended = TRUE)

vizFlatsPosition(z$flats, s = s)
vizTerracePosition(z$terrace, s = s)

s <- 'gerle'
ss <- siblings(s)
z <- fetchOSD(c(s, ss$sib$sibling), extended = TRUE)

vizMountainPosition(z$mtnpos, s = s)

## this function is still brittle to missing records
tab <- soilDB:::.tabulateGeomorphRecords(z)
round(sweep(tab[, -1], MARGIN = 1, STATS = rowSums(tab[, -1]), FUN = '/'), 3)

knitr::kable(tab)
