

library(daymetr)
library(Evapotranspiration)
library(elevatr)
library(hydromad)
library(zoo)

library(aqp)
library(soilDB)
# library(sharpshootR)

library(sp)
library(rgeos)


library(pbapply)
library(reshape2)

library(latticeExtra)
library(tactile)
library(knitr)
library(RColorBrewer)
library(viridis)


## prepare some example data via DAYMET

# near Sonora, CA
p <- SpatialPoints(cbind(-120.37673,37.99877), proj4string = CRS('+proj=longlat +datum=WGS84'))
dd <- prepareDailyClimateData(p, start = 2010, end = 2020)

# re-package
daily.data <- data.frame(
  date = dd$DM$date,
  PPT = dd$DM$prcp..mm.day.,
  PET = dd$ET$ET.Daily
)

# example soil material
data("ROSETTA.centroids")
vars <- c('texture', 'sat', 'fc', 'pwp')

# required soil hydraulic parameters + ID
x <- ROSETTA.centroids[c(12, 1, 3, 5), vars]

# thickness and recession coef.
x$thickness <- 100
x$a.ss <- 0.1

x

z <- dailyWB(x, daily.data, id = 'texture')

str(z)

#
msp <- moistureStateProportions(z, id = 'texture', step = 'week')

# colors / style
ll <- levels(msp$state)
n.states <- length(ll)
ms.colors <- brewer.pal(n.states, 'Spectral')

suppressWarnings(
  trellis.par.set(list(superpose.polygon=list(col=ms.colors, border=ms.colors)))
)

sK <- simpleKey(text = ll, space='top', columns=n.states, rectangles = TRUE, points=FALSE, cex=1)



barchart(proportion ~ interval | texture, groups = state, 
         main='Expected Weekly Soil Moisture State\nDAYMET 1988-2018',
         data = msp, horiz = FALSE, stack = TRUE, xlab = '', ylab='Proportion',
         as.table=TRUE,
         box.ratio = 30,
         key=sK,
         strip=strip.custom(bg=grey(0.9)),
         par.strip.text=list(cex=1.25),
         scales=list(y=list(alternating=3, cex=1), x=list(draw = FALSE)),
         par.settings = list(
           superpose.polygon = list(
             col = ms.colors, lwd = 1, lend = 1
           )
         ),
         # this breaks auto.key, thus requires simpleKey()
         panel=function(...) {
           # panel.abline(h=seq(0, 1, by=0.1), v=1:52, col=grey(0.9))
           panel.barchart(...)
         }
)


xyplot(VWC ~ date, groups = texture, data = z, type = 'l', par.settings = tactile.theme(), auto.key = list(lines = TRUE, points = FALSE, columns = 3))

xyplot(VWC ~ date | texture, data = z, type = c('l', 'g'), par.settings = tactile.theme(), auto.key = list(lines = TRUE, points = FALSE, columns = 3))




mst <- moistureStateThreshold(z, id = 'texture', threshold = 'dry', operator = '<=')
str(mst)

# colors / style
stripe.colors <- colorRampPalette(viridis(100), interpolate = 'spline', space = 'Lab')

levelplot(Pr ~ as.numeric(doy) * texture, data = mst, 
          col.regions = stripe.colors,
          par.settings = tactile.theme(),
          xlab = 'Day of the Year',
          ylab = '',
          main='Pr(Soil at Field Capacity or Drier)\n1988-2018',
          scales=list(x=list(tick.number=25))
)



# a different take: "on average, when do the soils
xyplot(Pr ~ as.numeric(doy), groups = texture,
       main='Expected Soil Moisture State\n1988-2018',
       data = mst, type=c('l', 'g'),
       xlab = '', ylab='Proportion',
       as.table=TRUE,
       auto.key=list(lines=TRUE, points=FALSE, columns=length(levels(msp$texture))),
       strip=strip.custom(bg=grey(0.9)),
       par.strip.text=list(cex=1.25),
       scales=list(y=list(alternating=3, cex=1), x=list(relation='free', alternating=1, cex=0.85, rot=0)),
       par.settings = tactile.theme(superpose.line = list(lwd = 2))
)


tapply(mst$Pr, mst$texture, function(i) length(which(i > 0.8)))



##
##
##


p <- SpatialPoints(cbind(-120.37673,37.99877), proj4string = CRS('+proj=longlat +datum=WGS84'))
cokeys <- c('19586277', '19586422', '19586459', '19586387', '19586251')

# investigate source data
s <- prepare_SSURGO_hydro_data(cokeys = cokeys, max.depth = 100)

par(mar = c(0, 0, 3, 0))
plotSPC(s$SPC, color = 'sat', name.style = 'center-center', plot.depth.axis = FALSE, label = 'compname', hz.depths = TRUE, cex.names = 0.8)


d <- dailyWB_SSURGO(x = p, cokeys = cokeys, modelDepth = 50, bufferRadiusMeters = 1)
levels(d$series)



## quick check on 30-yr proportions
kable(prop.table(table(d$series, d$state), margin = 1), digits = 2)

kable(prop.table(table(d$series, d$state < 'dry'), margin = 1), digits = 2)
kable(prop.table(table(d$series, d$state > 'very moist'), margin = 1), digits = 2)


msp <- moistureStateProportions(d, step = 'week')


# colors / style
ll <- levels(msp$state)
n.states <- length(ll)
ms.colors <- brewer.pal(n.states, 'Spectral')

suppressWarnings(
  trellis.par.set(list(superpose.polygon=list(col=ms.colors, border=ms.colors)))
)

sK <- simpleKey(text = ll, space='top', columns=n.states, rectangles = TRUE, points=FALSE, cex=1)



barchart(proportion ~ interval | series, groups = state, 
         main='Expected Weekly Soil Moisture State\nDAYMET 1988-2018',
         data = msp, horiz = FALSE, stack = TRUE, xlab = '', ylab='Proportion',
         as.table=TRUE,
         box.ratio = 30,
         key=sK,
         strip=strip.custom(bg=grey(0.9)),
         par.strip.text=list(cex=1.25),
         scales=list(y=list(alternating=3, cex=1), x=list(draw = FALSE)),
         par.settings = list(
           superpose.polygon = list(
             col = ms.colors, lwd = 1, lend = 1
           )
         ),
         # this breaks auto.key, thus requires simpleKey()
         panel=function(...) {
           # panel.abline(h=seq(0, 1, by=0.1), v=1:12, col=grey(0.9))
           panel.barchart(...)
         }
)


barchart(proportion ~ interval | series, groups = state, 
         main='Expected Weekly Soil Moisture State\n1988-2018',
         subset=series %in% c('Clarno', 'Diablo', 'Drummer', 'Frederick', 'Sierra'),
         data = msp, horiz = FALSE, stack = TRUE, xlab = '', ylab='Proportion',
         as.table=TRUE,
         key=sK,
         strip=strip.custom(bg=grey(0.9)),
         par.strip.text=list(cex=1.25),
         # layout=c(2,2),
         scales=list(y=list(alternating=3, cex=1), x=list(draw=FALSE, relation='free', alternating=1, cex=0.75, rot=90)),
         par.settings=list(superpose.polygon=list(col=ms.colors, lwd = 1, lend = 1)),
         # this breaks auto.key, thus requires simpleKey()
         panel=function(...) {
           # panel.abline(h=seq(0, 1, by=0.1), v=1:12, col=grey(0.9))
           panel.barchart(...)
         }
)





msp <- moistureStateProportions(d, step = 'month')


# colors / style
ll <- levels(msp$state)
n.states <- length(ll)
ms.colors <- brewer.pal(n.states, 'Spectral')

suppressWarnings(
  trellis.par.set(list(superpose.polygon=list(col=ms.colors, border=ms.colors)))
)

sK <- simpleKey(text = ll, space='top', columns=n.states, rectangles = TRUE, points=FALSE, cex=1)


barchart(proportion ~ interval | series, groups = state, 
         main='Expected Weekly Soil Moisture State\nDAYMET 1988-2018',
         data = msp, horiz = FALSE, stack = TRUE, xlab = '', ylab='Proportion',
         as.table=TRUE,
         box.ratio = 30,
         key=sK,
         strip=strip.custom(bg=grey(0.9)),
         par.strip.text=list(cex=1.25),
         scales=list(y=list(alternating=3, cex=1), x=list(cex = 0.75)),
         par.settings = list(
           superpose.polygon = list(
             col = ms.colors, lwd = 1, lend = 1
           )
         ),
         # this breaks auto.key, thus requires simpleKey()
         panel=function(...) {
           # panel.abline(h=seq(0, 1, by=0.1), v=1:12, col=grey(0.9))
           panel.barchart(...)
         }
)

barchart(proportion ~ series | interval, groups = state, 
         main='Expected Soil Moisture State\n1988-2018',
         data = msp, horiz = FALSE, stack = TRUE, xlab = '', ylab='Proportion',
         as.table=TRUE,
         key=sK,
         strip=strip.custom(bg=grey(0.9)),
         par.strip.text=list(cex=1.25),
         layout=c(4,3),
         scales=list(y=list(alternating=3, cex=1), x=list(relation='free', alternating=1, cex=0.75, rot=90)),
         par.settings=list(superpose.polygon=list(col=ms.colors, lwd = 1, lend = 1)),
         # this breaks auto.key, thus requires simpleKey()
         panel=function(...) {
           # panel.abline(h=seq(0, 1, by=0.1), v=1:12, col=grey(0.9))
           panel.barchart(...)
         }
)



# a different take: "on average, when do the soils
xyplot(proportion ~ interval | state, groups = series,
       subset = state < 'moist',
       main='Expected Soil Moisture State\n1988-2018',
       data = msp, type=c('l', 'g'),
       xlab = '', ylab='Proportion',
       as.table=TRUE,
       auto.key=list(lines=TRUE, points=FALSE, columns=length(levels(msp$series))),
       strip=strip.custom(bg=grey(0.9)),
       par.strip.text=list(cex=1.25),
       scales=list(y=list(alternating=3, cex=1), x=list(relation='free', alternating=1, cex=0.85, rot=90)),
       par.settings=list(superpose.line=list(col=c('firebrick', 'orange', 'darkgreen', 'royalblue'), lwd = 2, lend = 1))
)



levels(d$state)
mst <- moistureStateThreshold(d, threshold = 'dry', operator = '<=')

# colors / style
stripe.colors <- colorRampPalette(viridis(100), interpolate = 'spline', space = 'Lab')

levelplot(Pr ~ as.numeric(doy) * series, data = mst, 
          col.regions = stripe.colors,
          par.settings = tactile.theme(),
          xlab = 'Day of the Year',
          ylab = '',
          main='Pr(Soil at Field Capacity or Drier)\n1988-2018',
          scales=list(x=list(tick.number=25))
)



# a different take: "on average, when do the soils
xyplot(Pr ~ as.numeric(doy), groups = series,
       main='Expected Soil Moisture State\n1988-2018',
       data = mst, type=c('l', 'g'),
       xlab = '', ylab='Proportion',
       as.table=TRUE,
       auto.key=list(lines=TRUE, points=FALSE, columns=length(levels(msp$series))),
       strip=strip.custom(bg=grey(0.9)),
       par.strip.text=list(cex=1.25),
       scales=list(y=list(alternating=3, cex=1), x=list(relation='free', alternating=1, cex=0.85, rot=0)),
       par.settings = tactile.theme(superpose.line = list(lwd = 2)))
)

tapply(mst$Pr, mst$series, function(i) length(which(i > 0.8)))



# lamoni <- SpatialPoints(cbind(-92.37841, 41.40046), proj4string = CRS('+proj=longlat +datum=WGS84'))
# holland <- SpatialPoints(cbind(-120.29323, 38.01652), proj4string = CRS('+proj=longlat +datum=WGS84'))
# frederick <- SpatialPoints(cbind(-85.49610, 37.19396), proj4string = CRS('+proj=longlat +datum=WGS84'))
# sierra <- SpatialPoints(cbind(-120.35900, 37.98115), proj4string = CRS('+proj=longlat +datum=WGS84'))
# diablo <- SpatialPoints(cbind(-121.77100, 37.368402), proj4string = CRS('+proj=longlat +datum=WGS84'))
# drummer <- SpatialPoints(cbind(-88.49899, 40.00966), proj4string = CRS('+proj=longlat +datum=WGS84'))
# clarno <- SpatialPoints(cbind(-97.62537, 44.25341), proj4string = CRS('+proj=longlat +datum=WGS84'))
# pierre <- SpatialPoints(cbind(-102.95219, 43.35561), proj4string = CRS('+proj=longlat +datum=WGS84'))



# # Sierra
# p <- SpatialPoints(cbind(-120.35900, 37.98115), proj4string = CRS('+proj=longlat +datum=WGS84'))
# 
# # Pierre
# # p <- SpatialPoints(cbind(-102.95219, 43.35561), proj4string = CRS('+proj=longlat +datum=WGS84'))
# 
# # Centralia
# # p <- SpatialPoints(cbind(-92.11446, 39.23652), proj4string = CRS('+proj=longlat +datum=WGS84'))
# 
# 
# # 
# d <- dailyWB_SSURGO(p, modelDepth = 100, bufferRadiusMeters = 1)
# # head(d)
# 
# 
# nedsgulch <- SpatialPoints(cbind(-120.37673,37.99877), proj4string = CRS('+proj=longlat +datum=WGS84'))
# sierra <- SpatialPoints(cbind(-120.36651,37.99789), proj4string = CRS('+proj=longlat +datum=WGS84'))
# 
# p <- rbind(nedsgulch, sierra)
# 
# res <- pblapply(1:length(p), function(i) {
#   suppressMessages(dailyWB_SSURGO(x = p[i, ], modelDepth = 100, bufferRadiusMeters = 1))
# })
# 
# d <- do.call('rbind', res)
# 
# levels(d$series)
# 
