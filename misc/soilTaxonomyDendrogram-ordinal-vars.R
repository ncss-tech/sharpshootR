library(soilDB)
library(aqp)
library(SoilTaxonomy)
library(sharpshootR)

## TODO: document / examples / more flexibility in SoilTaxonomyDendrogram via argument

## divisive clustering works best (reasonable dendrogram) when encoding taxa as nominal vars

## agglomerative (ward) works best (?) when encoding taxa as ordinal vars


s <- c('fresno', 'musick', 'lucy', 'tristan', 'pierre', 'dylan', 'marion', 'zook', 'palau')
osd <- fetchOSD(s)

## convert horizon boundary distinctness -> vertical distance
# see manual page
osd$hzd <- hzDistinctnessCodeToOffset(
  osd$distinctness, 
  codes = c('very abrupt', 'abrubt', 'clear', 'gradual', 'diffuse')
)

par(mfcol = c(2, 1))


SoilTaxonomyDendrogram(
  spc = osd, 
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

mtext('nominal', side = 2, line=-1.5, font=3, cex=1.25)



## setup factor levels of ST
data("ST_unique_list")

osd$soilorder <- droplevels(factor(osd$soilorder, levels = ST_unique_list$order, ordered = TRUE))
osd$suborder <- droplevels(factor(osd$suborder, levels = ST_unique_list$suborder, ordered = TRUE))
osd$greatgroup <- droplevels(factor(osd$greatgroup, levels = ST_unique_list$greatgroup, ordered = TRUE))
osd$subgroup <- droplevels(factor(osd$subgroup, levels = ST_unique_list$subgroup, ordered = TRUE))


## better with default rotation applied

SoilTaxonomyDendrogram(
  spc = osd, 
  y.offset = 0.4, 
  rotationOrder = profile_id(osd)[order(osd$subgroup)],
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


mtext('ordinal, sorted', side = 2, line=-1.5, font=3, cex=1.25)



## ordination

library(cluster)
library(MASS)

x <- site(osd)
row.names(x) <- x[[idname(osd)]]

d <- daisy(x[, c('soilorder', 'suborder', 'greatgroup', 'subgroup')], metric='gower')

m <- sammon(d)

# plot(m$points[, 1], m$points[, 2], type = 'n')
# text(m$points[, 1], m$points[, 2], x$subgroup, cex = 0.5)
