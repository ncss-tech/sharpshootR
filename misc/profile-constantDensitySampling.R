library(proftools)
library(soilDB)
library(sharpshootR)
library(raster)
library(rgdal)

## must load this library for any kind of speedup ?
library(parallel)

# setup
r <- raster('E:/gis_data/CONUS-forms-DEB.tif')
# r <- raster('E:/gis_data/NASS/2016_30m_cdls.img')

# get several polygons
s <- lapply(c('amador', 'pentz', 'peters', 'pardee', 'redding', 'hanford', 'sites'), seriesExtent)
s <- do.call('rbind', s)
s <- spTransform(s, CRS(proj4string(r)))
s$pID <- 1:nrow(s)
nrow(s)

# parallel / serial comparison
system.time(ss <- constantDensitySampling(s, n.pts.per.ac=0.01, parallel = FALSE))
system.time(ss <- constantDensitySampling(s, n.pts.per.ac=0.01, parallel = TRUE))

# hmm... about equal performance
system.time(ss <- constantDensitySampling(s, n.pts.per.ac=0.1, parallel = FALSE))
system.time(ss <- constantDensitySampling(s, n.pts.per.ac=0.1, parallel = TRUE))


## profile
## http://adv-r.had.co.nz/Profiling.html#measure-perf
## https://cran.r-project.org/web/packages/proftools/index.html
# profile
pd.1 <- profileExpr(ss <- constantDensitySampling(s, n.pts.per.ac=0.01, parallel = TRUE))
pd.2 <- profileExpr(ss <- constantDensitySampling(s, n.pts.per.ac=0.01, parallel = FALSE))

hotPaths(pd.1)
hotPaths(pd.2)

flatProfile(pd.1)
flatProfile(pd.2)


par(mar=c(1,1,4,1))
flameGraph(pd.1, cex=0.7)
flameGraph(pd.1, order = 'time', cex=0.7)

flameGraph(pd.2, cex=0.7)
flameGraph(pd.2, order = 'time', cex=0.7)

# drummer @ 0.01 pts / ac. = 16 seconds
system.time(e <- extract(r, ss))

round(prop.table(table(e)), 2)



## try a large number of polygons
s <- readOGR(dsn='e:/gis_data/ca630/FG_CA630_OFFICIAL.gdb', layer='ca630_a')
s$pID <- 1:nrow(s)
system.time(ss <- constantDensitySampling(s, n.pts.per.ac=0.5, parallel = FALSE))
system.time(ss <- constantDensitySampling(s, n.pts.per.ac=0.5, parallel = TRUE))





