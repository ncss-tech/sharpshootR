library(aqp)
library(soilDB)
library(sharpshootR)

# soils of interest
s.list <- c('musick', 'cecil', 'drummer', 'amador', 'pentz', 'reiff',
            'san joaquin','montpellier','grangeville','pollasky','ramona')

# fetch and convert data into an SPC
h <- fetchOSD(s.list)

# plot dendrogram + profiles
SoilTaxonomyDendrogram(h)

# again, this time save the pair-wise dissimilarity matrix
# note that there isn't a lot of discrimination between soils
(d <- SoilTaxonomyDendrogram(h))


# a different set
soils <- c('cecil', 'altavista', 'lloyd', 'wickham', 'wilkes',
           'chewacla', 'congaree')

# get morphology + extended summaries for sorting of dendrogram
s <- fetchOSD(soils, extended = TRUE)

# get summary and ignore the figure
res <- vizHillslopePosition(s$hillpos)

# compare default sorting to soils sorting according to catenary, e.g.
# hillslope position
op <- par(no.readonly = TRUE)

par(mar=c(0,0,1,1), mfrow=c(2,1))

SoilTaxonomyDendrogram(s$SPC, width=0.25)
mtext('default sorting', side = 2, line=-1, font=3, cex=1.25)

SoilTaxonomyDendrogram(s$SPC, rotationOrder = profile_id(s$SPC)[res$order], width=0.25)
mtext('approx. catenary sorting', side = 2, line=-1, font=3, cex=1.25)


# classic chrono-sequence from the San Joaquin Valley, CA

s <- c('tujunga', 'hanford', 'greenfield', 'snelling', 'san joaquin')
osds <- fetchOSD(s)

idx <- match(toupper(s), profile_id(osds))

# encode horizon boundarydistinctness via vertical offset
osds$hd <- hzDistinctnessCodeToOffset(
  osds$distinctness,
  codes=c('very abrupt', 'abrupt', 'clear', 'gradual', 'diffuse')
)

# encode horizon boundary topography via vertical offset
osds$hzto <- hzTopographyCodeToOffset(
  osds$topography,
  codes = c('smooth', 'wavy', 'irregular', 'broken')
)

# also encode horizon boundary topography las line type
osds$hzto.lty <- hzTopographyCodeToLineType(
  osds$topography,
  codes = c('smooth', 'wavy', 'irregular', 'broken')
)

# label data source, used later
site(osds)$source <- 'OSD'

# concise representation of hz bnd distinctness and topography
# similar to field notes
osds$bnd.code <- sprintf(
  "%s%s",
  substr(osds$distinctness, 1, 1),
  substr(osds$topography, 1, 1)
)

# remove NA
osds$bnd.code <- gsub('NANA', '', osds$bnd.code)

par(mar = c(0, 0, 0, 1), bg = 'black', fg = 'white')

plotSPC(
  osds,
  plot.order = idx,
  width = 0.3,
  name.style = 'center-center',
  cex.names = 0.66,
  plot.depth.axis = FALSE,
  hz.depths = TRUE,
  shrink = TRUE,
  hz.distinctness.offset = 'hd',
  hz.topography.offset = 'hzto',
  hz.boundary.lty = 'hzto.lty'
)

legend(
  'bottomright',
  horiz = TRUE,
  legend = c('Smooth', 'Wavy', 'Irregular', 'Broken'),
  lty = 1:4,
  inset = 0.05,
  bty = 'n',
  cex = 0.85
)

# note that `rotationOrder` uses the ordering of series names (uppercase to match profile IDs)
# to re-order the terminal branches of the dendrogram
SoilTaxonomyDendrogram(
  osds,
  rotationOrder = toupper(s),
  cex.taxon.labels = 0.85,
  width = 0.3,
  name.style = 'center-center',
  cex.names = 0.66,
  plot.depth.axis = FALSE,
  hz.depths = TRUE,
  shrink = TRUE,
  hz.distinctness.offset = 'hd',
  hz.topography.offset = 'hzto',
  hz.boundary.lty = 'hzto.lty'
)

legend(
  'bottomright',
  horiz = TRUE,
  legend = c('Smooth', 'Wavy', 'Irregular', 'Broken'),
  lty = 1:4,
  inset = 0.05,
  bty = 'n',
  cex = 0.85
)

par(op)
