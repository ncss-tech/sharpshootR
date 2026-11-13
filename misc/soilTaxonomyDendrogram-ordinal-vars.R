library(aqp)
library(soilDB)
library(sharpshootR)
library(SoilTaxonomy)
library(corrplot)

s <- c('AMADOR', 'AUBURN', 'PENTZ', 'galt', 'LOAFERCREEK', 'INKS', 'PARDEE', 'palau', 'lucy', 'leon')

x <- fetchOSD(s)


par(mfrow = c(2, 2))

d4 <- SoilTaxonomyDendrogram(x, KST.order = FALSE, depth.axis = list(line = -2.5, style = 'compact', cex = 0.66), max.depth = 150, font.taxon.labels = 1, cex.taxon.labels = 0.75, width = 0.33)
mtext('Nominal | Equal Weights', side = 1, at = 0.5, font = 2, line = -2, adj = 0)

d1 <- SoilTaxonomyDendrogram(x, KST.order = TRUE, depth.axis = list(line = -2.5, style = 'compact', cex = 0.66), wt = c(1, 1, 1, 1), max.depth = 150, font.taxon.labels = 1, cex.taxon.labels = 0.75, width = 0.33)
mtext('Ordinal | Equal Weights', side = 1, at = 0.5, font = 2, line = -2, adj = 0)

d2 <- SoilTaxonomyDendrogram(x, KST.order = TRUE, depth.axis = list(line = -2.5, style = 'compact', cex = 0.66), wt = c(4, 2, 1, 0.5), max.depth = 150, font.taxon.labels = 1, cex.taxon.labels = 0.75, width = 0.33)
mtext('Ordinal | Decreasing Weights', side = 1, at = 0.5, font = 2, line = -2, adj = 0)

d3 <- SoilTaxonomyDendrogram(x, KST.order = TRUE, depth.axis = list(line = -2.5, style = 'compact', cex = 0.66), wt = c(0.5, 1, 2, 4), max.depth = 150, font.taxon.labels = 1, cex.taxon.labels = 0.75, width = 0.33)
mtext('Ordinal | Increasing Weights', side = 1, at = 0.5, font = 2, line = -2, adj = 0)



m1 <- as.matrix(d1$dist)
diag(m1) <- NA

m2 <- as.matrix(d2$dist)
diag(m2) <- NA

m3 <- as.matrix(d3$dist)
diag(m3) <- NA

m4 <- as.matrix(d4$dist)
diag(m4) <- NA

par(mfcol = c(2, 2))

corrplot(m1, method = 'color', type = 'upper', col = hcl.colors(25, palette = 'zissou1'), is.corr = FALSE, na.label = '-', col.lim = c(0, 1), tl.col = 'black', tl.cex = 0.85)
mtext('Ordinal | Equal Weights', side = 1, at = 1, font = 2, line = -2)

corrplot(m2, method = 'color', type = 'upper', col = hcl.colors(25, palette = 'zissou1'), is.corr = FALSE, na.label = '-', col.lim = c(0, 1), tl.col = 'black', tl.cex = 0.85)
mtext('Ordinal | Decreasing Weights', side = 1, at = 1, font = 2, line = -2)

corrplot(m3, method = 'color', type = 'upper', col = hcl.colors(25, palette = 'zissou1'), is.corr = FALSE, na.label = '-', col.lim = c(0, 1), tl.col = 'black', tl.cex = 0.85)
mtext('Ordinal | Increasing Weights', side = 1, at = 1, font = 2, line = -2)

corrplot(m4, method = 'color', type = 'upper', col = hcl.colors(25, palette = 'zissou1'), is.corr = FALSE, na.label = '-', col.lim = c(0, 1), tl.col = 'black', tl.cex = 0.85)
mtext('Nominal | Equal Weights', side = 1, at = 1, font = 2, line = -2)



par(mfcol = c(1, 2))
corrplot(m1 - m2, method = 'color', type = 'upper', col = hcl.colors(25, palette = 'spectral', rev = TRUE), is.corr = FALSE, na.label = '-', tl.col = 'black')
title('Equal - Decreasing', line = -2)

corrplot(m1 - m3, method = 'color', type = 'upper', col = hcl.colors(25, palette = 'spectral', rev = TRUE), is.corr = FALSE, na.label = '-', tl.col = 'black')
title('Equal - Increasing', line = -2)








res1 <- SoilTaxonomyDendrogram(x, KST.order = TRUE, depth.axis = list(line = -3))
res1$dist



res2 <- SoilTaxonomyDendrogram(x, KST.order = FALSE)
res2$dist


pts <- MASS::sammon(res1$dist)

plot(pts$points)



##

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
