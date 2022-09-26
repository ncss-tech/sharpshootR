library(aqp)
library(soilDB)
library(sf)
library(sharpshootR)
library(SoilTaxonomy)


# way too many ties in geomcomp
bb <- '-97.0983 39.3808,-97.0983 39.4127,-97.0282 39.4127,-97.0282 39.3808,-97.0983 39.3808'

# CA630: yikes
bb <- '-120.3551 38.0050,-120.3551 38.0375,-120.2850 38.0375,-120.2850 38.0050,-120.3551 38.0050'

# ND: flats and terraces
bb <- '-100.5758 47.6062,-100.5758 47.6340,-100.5056 47.6340,-100.5056 47.6062,-100.5758 47.6062'


# mountains
bb <- '-120.2273 37.9859,-120.2273 38.1158,-119.9467 38.1158,-119.9467 37.9859,-120.2273 37.9859'


bb <- '-96.7622 30.8644,-96.7622 30.9351,-96.6218 30.9351,-96.6218 30.8644,-96.7622 30.8644'



## assemble AOI polygon into WKT
wkt <- sprintf('POLYGON((%s))', bb)

## init sf polygon
x <- st_as_sfc(wkt)

# set CRS as GCS WGS84
st_crs(x) <- 4326

## get overlapping map unit keys
# could also use SDA_query() with more elaborate SQL
m <- SDA_spatialQuery(x, what = 'mukey')

## compose SQL to return component details for these map unit keys
# return only:
# * map units overlapping with BBOX
# * major components
# * no misc. areas that might share name with a poorly-named soil series
sql <- sprintf(
  "SELECT mukey, cokey, compname, compkind, comppct_r 
  FROM component 
  WHERE mukey IN %s 
  -- AND majcompflag = 'Yes'
  AND compkind != 'Miscellaneous area'
  ", format_SQL_in_statement(as.integer(m$mukey))
)

## send to SDA, result is a data.frame
s <- SDA_query(sql)



## get OSD morphology + extended summaries 
osd <- fetchOSD(unique(s$compname), extended = TRUE)


## check out results
str(osd, 1)


# the latest soilDB::fetchOSD() will automatically encode horizon distinctness offset
# backwards compatibility
if(is.null(osd$SPC$hzd)) {
  
  # convert horizon boundary distinctness -> vertical distance
  osd$SPC$hzd <- hzDistinctnessCodeToOffset(
    osd$SPC$distinctness, 
    codes = c('very abrupt', 'abrubt', 'clear', 'gradual', 'diffuse')
  )
}




## arrange according to classification, accounting for order within KST
## using ordered factors
SoilTaxonomyDendrogram(
  spc = osd$SPC, 
  KST.order = TRUE, 
  y.offset = 0.4, 
  scaling.factor = 0.014, 
  cex.taxon.labels = 0.75,
  cex.id = 0.85,
  cex.names = 0.75,
  width = 0.3, 
  name.style = 'center-center', 
  plot.depth.axis = TRUE,
  axis.line.offset = -3.5,
  hz.distinctness.offset = 'hzd'
)




# hillpos geomorphic summary
o <- reconcileOSDGeomorph(osd, 'hillpos')
res <- vizHillslopePosition(o$geom, maxIter = 100, j.amount = 0.05, verbose = TRUE)
print(res$fig)

# 3D geomorphic summary
o <- reconcileOSDGeomorph(osd, 'geomcomp')
res <- vizGeomorphicComponent(o$geom, maxIter = 100, j.amount = 0.05, verbose = TRUE)
print(res$fig)

# flats geomorphic summary
o <- reconcileOSDGeomorph(osd, 'flats')
res <- vizFlatsPosition(o$geom, maxIter = 100, j.amount = 0.05, verbose = TRUE)
print(res$fig)

# terrace geomorphic summary
o <- reconcileOSDGeomorph(osd, 'terrace')
res <- vizTerracePosition(o$geom, maxIter = 100, j.amount = 0.05, verbose = TRUE)
print(res$fig)

# mountain geomorphic summary
o <- reconcileOSDGeomorph(osd, 'mtnpos')
res <- vizMountainPosition(o$geom)
print(res$fig)

# shape geomorphic summary
o <- reconcileOSDGeomorph(osd, 'shape_across')
res <- vizSurfaceShape(o$geom, title = 'Shape Across', maxIter = 100, j.amount = 0.05, verbose = TRUE)
print(res$fig)

# shape geomorphic summary
o <- reconcileOSDGeomorph(osd, 'shape_down')
res <- vizSurfaceShape(o$geom, title = 'Shape Down', maxIter = 100, j.amount = 0.05, verbose = TRUE)
print(res$fig)










## arrange according to clustering of hillslope position
o <- reconcileOSDGeomorph(osd, 'hillpos')
res <- vizHillslopePosition(o$geom)

par(mar = c(0, 0, 0, 0))
plotProfileDendrogram(
  o$SPC, 
  clust = res$clust, 
  dend.y.scale = 3, 
  y.offset = 0.2,
  scaling.factor = 0.01, 
  width = 0.3, 
  name.style = 'center-center', 
  plot.depth.axis = FALSE, 
  hz.depths = TRUE, 
  hz.distinctness.offset = 'hzd', 
  cex.names = 0.6, 
  cex.id = 0.6
)


par(mar = c(0.5, 0, 0, 2), bg = 'black', fg = 'white')
plotGeomorphCrossSection(osd, type = 'line', maxIter = 100, j.amount = 0.05, verbose = TRUE)
plotGeomorphCrossSection(osd, type = 'bar', maxIter = 100, j.amount = 0.05, verbose = TRUE)

plotGeomorphCrossSection(osd, type = 'line', clust = FALSE)
plotGeomorphCrossSection(osd, type = 'bar', clust = FALSE)



