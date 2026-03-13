library(soilDB)
library(sharpshootR)
library(PNWColors)

cols <- pnw_palette('Shuksan2', n = 5)

s <- 'redding'
ss <- siblings(s)
z <- fetchOSD(c(s, ss$sib$sibling), extended = TRUE)

vizHillslopePosition(z$hillpos, s = s)

vizGeomorphicComponent(z$geomcomp, s = s)

vizTerracePosition(z$terrace, s = s)

vizFlatsPosition(z$flats, s = s)


.cols <- c(grey(0.8), rev(PNWColors::pnw_palette('Bay', n = 9)))
.cols <- colorspace::desaturate(.cols, amount = 0.2)

vizGeomorphons(z$geomorphons, s = s)


# .cols <- c(PNWColors::pnw_palette('Bay', n = 3), '#808080', 'darkgreen')
# .cols <- colorspace::desaturate(.cols, amount = 0.2)

vizSurfaceShape(z$shape_across)




s <- 'columbia'
ss <- siblings(s)
z <- fetchOSD(c(s, ss$sib$sibling), extended = TRUE)

vizFlatsPosition(z$flats, s = s)
vizTerracePosition(z$terrace, s = s)

vizGeomorphons(z$geomorphons, s = s)


s <- 'gerle'
ss <- siblings(s)
z <- fetchOSD(c(s, ss$sib$sibling), extended = TRUE)

vizMountainPosition(z$mtnpos, s = s)

vizGeomorphons(z$geomorphons, s = s)


s <- 'gwin'
ss <- siblings(s)
z <- fetchOSD(c(s, ss$sib$sibling), extended = TRUE)

# .cols <- rev(pnw_palette('Cascades', n = 6))
vizMountainPosition(z$mtnpos, s = s)

vizGeomorphons(z$geomorphons, s = s)
vizHillslopePosition(z$hillpos, s = s)




s <- 'zook'
ss <- siblings(s)
z <- fetchOSD(c(s, ss$sib$sibling), extended = TRUE)

vizFlatsPosition(z$flats, s = s)

vizGeomorphons(z$geomorphons, s = s)


s <- 'cecil'
ss <- siblings(s)
z <- fetchOSD(c(s, ss$sib$sibling), extended = TRUE)

vizHillslopePosition(z$hillpos, s = s)
vizFlatsPosition(z$flats, s = s)
vizGeomorphons(z$geomorphons, s = s)

s <- 'drummer'
ss <- siblings(s)
z <- fetchOSD(c(s, ss$sib$sibling), extended = TRUE)

vizFlatsPosition(z$flats, s = s)
vizGeomorphons(z$geomorphons, s = s)


s <- 'pierre'
ss <- siblings(s)
z <- fetchOSD(c(s, ss$sib$sibling), extended = TRUE)

vizHillslopePosition(z$hillpos, s = s)

# looks better with even thinner lines
vizGeomorphons(z$geomorphons, s = s)

# no lines
update(vizGeomorphons(z$geomorphons, s = s)$fig, lwd = 0)

vizGeomorphons(z$geomorphons, s = s, clust = TRUE)
vizGeomorphons(z$geomorphons, s = s, clust = FALSE)



## this function is still brittle to missing records
tab <- soilDB:::.tabulateGeomorphRecords(z)
round(sweep(tab[, -1], MARGIN = 1, STATS = rowSums(tab[, -1]), FUN = '/'), 3)

knitr::kable(tab)
