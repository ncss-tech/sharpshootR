library(aqp)
library(sf)
library(SoilTaxonomy)

# need latest from GH
library(sharpshootR)
library(soilDB)


##
bb <- '-120.8372 38.1860,-120.8372 38.2177,-120.7749 38.2177,-120.7749 38.1860,-120.8372 38.1860'



## assemble AOI polygon into WKT
wkt <- sprintf('POLYGON((%s))', bb)

## init sf polygon
# WGS84 GCS
x <- st_as_sfc(wkt, crs = 4326)


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
  AND majcompflag = 'Yes'
  AND compkind != 'Miscellaneous area'
  ", format_SQL_in_statement(as.integer(m$mukey))
)

## send to SDA, result is a data.frame
s <- SDA_query(sql)


## get OSD morphology + extended summaries 
osd <- fetchOSD(unique(s$compname), extended = TRUE)


## check out results
str(osd, 1)



## classic arrangement, using normal (nominal) factors
SoilTaxonomyDendrogram(
  osd$SPC, 
  KST.order = FALSE,
  y.offset = 0.4, 
  scaling.factor = 0.015, 
  cex.taxon.labels = 1, 
  cex.id = 1,
  cex.names = 0.85,
  width = 0.3, 
  name.style = 'center-center', 
  depth.axis = list(line = -4),
  hz.distinctness.offset = 'hzd',
  max.depth = 200
)

## arrange according to classification, accounting for order within KST
## using ordered factors
SoilTaxonomyDendrogram(
  spc = osd$SPC, 
  KST.order = TRUE, 
  y.offset = 0.4, 
  scaling.factor = 0.015, 
  cex.taxon.labels = 1,
  cex.id = 1,
  cex.names = 0.9,
  width = 0.35, 
  name.style = 'center-center', 
  depth.axis = list(line = -4),
  hz.distinctness.offset = 'hzd',
  max.depth = 200
)


SoilTaxonomyDendrogram(
  spc = osd$SPC, 
  KST.order = TRUE, 
  y.offset = 0.4, 
  scaling.factor = 0.03, 
  cex.taxon.labels = 1,
  cex.id = 1,
  cex.names = 0.9,
  width = 0.35, 
  name.style = 'center-center', 
  depth.axis = list(line = -4),
  hz.distinctness.offset = 'hzd',
  max.depth = 100, 
  shrink = TRUE,
  shrink.thin = 0.03 * 5
)




## updated content, based on latest sharpshootR

# reconcileOSDGeomorph() will perform cross-check between SPC--geomorph summary
# vizHillslopePosition() makes the cross-section

## notes:
# * the following functions will nearly always require tinkering with `scaling.factor`

options(.aqp.plotSPC.args = NULL)
options(.aqp.plotSPC.args = list(width = 0.35, scaling.factor = 0.021, max.depth = 100, cex.id = 0.7))

par(mar = c(0.5, 0, 0, 2), bg = 'black', fg = 'white')
plotGeomorphCrossSection(osd, type = 'line')
plotGeomorphCrossSection(osd, type = 'bar')


options(.aqp.plotSPC.args = list(width = 0.35, max.depth = 100, cex.id = 0.7))
plotGeomorphCrossSection(osd, type = 'line', clust = FALSE)
plotGeomorphCrossSection(osd, type = 'bar', clust = FALSE)

