

library(daymetr)
library(Evapotranspiration)
library(elevatr)
library(hydromad)
library(zoo)

library(aqp)
library(soilDB)
library(sharpshootR)

library(sp)
library(rgeos)


library(pbapply)
library(reshape2)

library(latticeExtra)
library(tactile)
library(knitr)
library(RColorBrewer)
library(viridis)


## simulate NULL PET
# near Sonora, CA
p <- SpatialPoints(cbind(-120.37673,37.99877), proj4string = CRS('+proj=longlat +datum=WGS84'))
daily.data <- prepareDailyClimateData(p, start = 2013, end = 2014, onlyWB = TRUE)

# save PET
daily.data$PET_saved <- daily.data$PET

# example soil material
data("ROSETTA.centroids")
vars <- c('texture', 'sat', 'fc', 'pwp')

# required soil hydraulic parameters + ID
x <- ROSETTA.centroids[c(12, 1, 3, 5), vars]

# thickness and recession coef.
x$thickness <- 50
# recession coef.
x$a.ss <- c(0.2, 0.1, 0.3, 0.4)
x

# NO PET
daily.data$PET <- 0

z <- dailyWB(x, daily.data, id = 'texture', S_0 = 1)

xyplot(
  VWC ~ date, 
  groups = texture, 
  data = z, 
  type = 'l', 
  par.settings = tactile.theme(), 
  auto.key = list(lines = TRUE, points = FALSE, columns = 4), 
  panel = function(...) {
    panel.xyplot(...)
    panel.abline(h = x$fc, col = tactile.theme()$superpose.line$col, lty = 2)
  })


daily.data$PET <- daily.data$PET_saved

z <- dailyWB(x, daily.data, id = 'texture', S_0 = 0.75)

xyplot(
  VWC ~ date, 
  groups = texture, 
  data = z, 
  type = 'l', 
  par.settings = tactile.theme(), 
  auto.key = list(lines = TRUE, points = FALSE, columns = 4), 
  panel = function(...) {
    panel.xyplot(...)
    panel.abline(h = x$pwp, col = tactile.theme()$superpose.line$col, lty = 2)
  })

## what about M parameter?
## no effect

# z <- dailyWB(x, daily.data, id = 'texture', S_0 = 0.75, M = 0)
# 
# xyplot(
#   VWC ~ date, 
#   groups = texture, 
#   data = z, 
#   type = 'l', 
#   par.settings = tactile.theme(), 
#   auto.key = list(lines = TRUE, points = FALSE, columns = 4), 
#   panel = function(...) {
#     panel.xyplot(...)
#     panel.abline(h = x$fc, col = tactile.theme()$superpose.line$col, lty = 2)
#   })



## prepare some example data via DAYMET

# near Sonora, CA
p <- SpatialPoints(cbind(-120.37673,37.99877), proj4string = CRS('+proj=longlat +datum=WGS84'))
daily.data <- prepareDailyClimateData(p, start = 2000, end = 2019, onlyWB = TRUE)

head(daily.data)


# example soil material
data("ROSETTA.centroids")
vars <- c('texture', 'sat', 'fc', 'pwp')

# required soil hydraulic parameters + ID
x <- ROSETTA.centroids[c(12, 1, 3, 5), vars]

# thickness and recession coef.
x$thickness <- 50
# this should reflect drainage class: lower numbers = less internal drainage
x$a.ss <- c(0.2, 0.1, 0.3, 0.5)

# x$a.ss <- 0.3
x

z <- dailyWB(x, daily.data, id = 'texture', MS.style = 'newhall')

str(z)

ann.AET <- tapply(
  z$ET, INDEX = list(z$texture, z$year), FUN = sum
)

t(apply(ann.AET, 1, quantile))


kable(prop.table(table(z$texture, z$state), margin = 1), digits = 2)

# kable(prop.table(table(z$texture, z$state <= 'dry'), margin = 1), digits = 2)
# kable(prop.table(table(z$texture, z$state > 'very moist'), margin = 1), digits = 2)



xyplot(
  VWC ~ date, 
  groups = texture, 
  data = z, 
  type = 'l', 
  par.settings = tactile.theme(), 
  auto.key = list(lines = TRUE, points = FALSE, columns = 4), 
  subset = year %in% c('2013'), 
  panel = function(...) {
    panel.xyplot(...)
    panel.abline(h = x$fc, col = tactile.theme()$superpose.line$col, lty = 2)
  })

xyplot(S ~ date, groups = texture, data = z, type = c('l', 'g'), par.settings = tactile.theme(), auto.key = list(lines = TRUE, points = FALSE, columns = 4), subset = year %in% c('2013'))

xyplot(U ~ date | texture, data = z, type = c('l', 'g'), par.settings = tactile.theme(), auto.key = list(lines = TRUE, points = FALSE, columns = 3), subset = year %in% c('2013'))





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

levelplot(state ~ as.numeric(doy) * texture | year, data = z, 
          subset = year %in% c('2013'),
          col.regions = colorRampPalette(brewer.pal(n.states, 'Spectral'), interpolate = 'spline', space = 'Lab'),
          par.settings = tactile.theme(),
          as.table = TRUE,
          key = sK,
          colorkey = FALSE,
          xlab = 'Day of the Year',
          ylab = '',
          main='Soil Moisture State',
          scales=list(alternating = 1, x=list(tick.number=25))
)

levelplot(state ~ as.numeric(doy) * year | texture, data = z, 
          # subset = year %in% c('2010'),
          col.regions = colorRampPalette(brewer.pal(n.states, 'Spectral'), interpolate = 'spline', space = 'Lab'),
          par.settings = tactile.theme(),
          as.table = TRUE,
          key = sK,
          colorkey = FALSE,
          xlab = 'Day of the Year',
          ylab = '',
          main='Soil Moisture State',
          scales=list(alternating = 1, x=list(tick.number=25))
)

levelplot(S ~ as.numeric(doy) * year | texture, data = z, 
          # subset = year %in% c('2010'),
          col.regions = colorRampPalette(brewer.pal(n.states, 'Spectral'), interpolate = 'spline', space = 'Lab'),
          par.settings = tactile.theme(),
          as.table = TRUE,
          xlab = 'Day of the Year',
          ylab = '',
          main='Soil Moisture',
          scales=list(alternating = 1, x=list(tick.number=25))
)

levelplot(VWC ~ as.numeric(doy) * year | texture, data = z, 
          # subset = year %in% c('2010'),
          col.regions = colorRampPalette(brewer.pal(n.states, 'Spectral'), interpolate = 'spline', space = 'Lab'),
          par.settings = tactile.theme(),
          as.table = TRUE,
          xlab = 'Day of the Year',
          ylab = '',
          main='Soil Moisture',
          scales=list(alternating = 1, x=list(tick.number=25))
)

levelplot(ET ~ as.numeric(doy) * year | texture, data = z, 
          # subset = year %in% c('2010'),
          col.regions = colorRampPalette(brewer.pal(n.states, 'Spectral'), interpolate = 'spline', space = 'Lab'),
          par.settings = tactile.theme(),
          as.table = TRUE,
          xlab = 'Day of the Year',
          ylab = '',
          main='Soil Moisture',
          scales=list(alternating = 1, x=list(tick.number=25))
)

## 

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


mss <- moistureStateStats(msp, id = 'texture')

xyplot(H ~ as.numeric(interval), groups = texture, data = mss, type = c('l', 'g'), par.settings = tactile.theme(), auto.key = list(lines = TRUE, points = FALSE, columns = 4))

levelplot(state ~ as.numeric(interval) * texture, data = mss, 
          # subset = year %in% c('2010'),
          col.regions = colorRampPalette(brewer.pal(n.states, 'Spectral'), interpolate = 'spline', space = 'Lab'),
          par.settings = tactile.theme(),
          as.table = TRUE,
          key = sK,
          colorkey = FALSE,
          xlab = 'Week',
          ylab = '',
          main='Soil Moisture',
          scales=list(alternating = 1, x=list(tick.number=25))
)

##

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


## hmmm
tapply(mst$Pr, mst$texture, function(i) length(which(i > 0.8)))



##
## use SSURGO data
##


p <- SpatialPoints(cbind(-120.37673,37.99877), proj4string = CRS('+proj=longlat +datum=WGS84'))
cokeys <- c('19586277', '19586145', '19586459', '19586251')

# investigate source data
s <- prepare_SSURGO_hydro_data(cokeys = cokeys, max.depth = 100)

par(mar = c(0, 0, 3, 0))
plotSPC(s$SPC, color = 'sat', name.style = 'center-center', plot.depth.axis = FALSE, label = 'compname', hz.depths = TRUE, cex.names = 0.8)

## moisture state proportions still aren't quite right... need to compare with real data

# with a.ss = 1, xeric soils are "saturated" too often
d <- dailyWB_SSURGO(x = p, cokeys = cokeys, modelDepth = 50, bufferRadiusMeters = 1, a.ss = 0.6, MS.style = 'default')


xyplot(VWC ~ as.numeric(doy) | compname, groups = year, data = d, type = 'l', subset = year %in% c('1990', '1991', '1992'), par.settings = tactile.theme(), scales = list(y = list(rot = 0)))

xyplot(VWC ~ as.numeric(doy) | year, groups = compname, data = d, type = 'l', subset = year %in% c('1990', '1991', '1992'), par.settings = tactile.theme(), scales = list(y = list(rot = 0)), auto.key = list(lines = TRUE, points = FALSE, columns = 4))

xyplot(S ~ as.numeric(doy) | compname, groups = year, data = d, type = 'l', subset = year %in% c('1990', '1991', '1992'), par.settings = tactile.theme(), scales = list(y = list(rot = 0)))

xyplot(S ~ as.numeric(doy) | year, groups = compname, data = d, type = 'l', subset = year %in% c('1990', '1991', '1992'), par.settings = tactile.theme(), scales = list(y = list(rot = 0)), auto.key = list(lines = TRUE, points = FALSE, columns = 4))

xyplot(U ~ as.numeric(doy) | compname, groups = year, data = d, type = 'l', subset = year %in% c('1990', '1991', '1992'), par.settings = tactile.theme(), scales = list(y = list(rot = 0)))

xyplot(U ~ as.numeric(doy) | year, groups = compname, data = d, type = 'l', subset = year %in% c('1990', '1991', '1992'), par.settings = tactile.theme(), scales = list(y = list(rot = 0)), auto.key = list(lines = TRUE, points = FALSE, columns = 4))



## quick check on 30-yr proportions
kable(prop.table(table(d$compname, d$state), margin = 1), digits = 2)

# MS.style = 'newhall'
# mean "days dry|moist|saturated"
kable(table(d$compname, d$state) / length(unique(d$year)), digits = 2)

# kable(prop.table(table(d$compname, d$state < 'dry'), margin = 1), digits = 2)
# kable(prop.table(table(d$compname, d$state > 'very moist'), margin = 1), digits = 2)



levelplot(state ~ as.numeric(doy) * compname | year, data = d, 
          subset = year %in% c('2011', '2012', '2013', '2014'),
          col.regions = colorRampPalette(brewer.pal(n.states, 'Spectral'), interpolate = 'spline', space = 'Lab'),
          par.settings = tactile.theme(),
          as.table = TRUE,
          key = sK,
          colorkey = FALSE,
          xlab = 'Day of the Year',
          ylab = '',
          main='Soil Moisture State',
          scales=list(alternating = 1, x=list(tick.number=25))
)

levelplot(state ~ as.numeric(doy) * year | compname, data = d, 
          # subset = year %in% c('2010'),
          col.regions = colorRampPalette(brewer.pal(n.states, 'Spectral'), interpolate = 'spline', space = 'Lab'),
          par.settings = tactile.theme(),
          as.table = TRUE,
          key = sK,
          colorkey = FALSE,
          xlab = 'Day of the Year',
          ylab = '',
          main='Soil Moisture State',
          scales=list(alternating = 1, x=list(tick.number=25))
)

## sum over each year likely the best metric
levelplot(ET ~ as.numeric(doy) * year | compname, data = d, 
          # subset = year %in% c('2010'),
          col.regions = colorRampPalette(brewer.pal(n.states, 'Spectral'), interpolate = 'spline', space = 'Lab'),
          par.settings = tactile.theme(),
          as.table = TRUE,
          xlab = 'Day of the Year',
          ylab = '',
          main='AET (mm)\n1988-2018',
          scales=list(alternating = 1, x=list(tick.number=25))
)

##
msp <- moistureStateProportions(d, step = 'week')

# colors / style
ll <- levels(msp$state)
n.states <- length(ll)
ms.colors <- brewer.pal(n.states, 'Spectral')

suppressWarnings(
  trellis.par.set(list(superpose.polygon=list(col=ms.colors, border=ms.colors)))
)

sK <- simpleKey(text = ll, space='top', columns=n.states, rectangles = TRUE, points=FALSE, cex=1)



barchart(proportion ~ interval | compname, groups = state, 
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


# barchart(proportion ~ interval | compname, groups = state, 
#          main='Expected Weekly Soil Moisture State\n1988-2018',
#          subset=compname %in% c('Clarno', 'Diablo', 'Drummer', 'Frederick', 'Sierra'),
#          data = msp, horiz = FALSE, stack = TRUE, xlab = '', ylab='Proportion',
#          as.table=TRUE,
#          key=sK,
#          strip=strip.custom(bg=grey(0.9)),
#          par.strip.text=list(cex=1.25),
#          # layout=c(2,2),
#          scales=list(y=list(alternating=3, cex=1), x=list(draw=FALSE, relation='free', alternating=1, cex=0.75, rot=90)),
#          par.settings=list(superpose.polygon=list(col=ms.colors, lwd = 1, lend = 1)),
#          # this breaks auto.key, thus requires simpleKey()
#          panel=function(...) {
#            # panel.abline(h=seq(0, 1, by=0.1), v=1:12, col=grey(0.9))
#            panel.barchart(...)
#          }
# )





msp <- moistureStateProportions(d, step = 'month', id = 'compname')


# colors / style
ll <- levels(msp$state)
n.states <- length(ll)
ms.colors <- brewer.pal(n.states, 'Spectral')

suppressWarnings(
  trellis.par.set(list(superpose.polygon=list(col=ms.colors, border=ms.colors)))
)

sK <- simpleKey(text = ll, space='top', columns=n.states, rectangles = TRUE, points=FALSE, cex=1)


barchart(proportion ~ interval | compname, groups = state, 
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

barchart(proportion ~ compname | interval, groups = state, 
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
xyplot(proportion ~ interval | state, groups = compname,
       subset = state < 'moist',
       main='Expected Soil Moisture State\n1988-2018',
       data = msp, type=c('l', 'g'),
       xlab = '', ylab='Proportion',
       as.table=TRUE,
       auto.key=list(lines=TRUE, points=FALSE, columns=length(levels(msp$compname))),
       strip=strip.custom(bg=grey(0.9)),
       par.strip.text=list(cex=1.25),
       scales=list(y=list(alternating=3, cex=1), x=list(relation='free', alternating=1, cex=0.85, rot=90)),
       par.settings=list(superpose.line=list(col=c('firebrick', 'orange', 'darkgreen', 'royalblue'), lwd = 2, lend = 1))
)



levels(d$state)
mst <- moistureStateThreshold(d, threshold = 'dry', operator = '<=')

# colors / style
stripe.colors <- colorRampPalette(viridis(100), interpolate = 'spline', space = 'Lab')

levelplot(Pr ~ as.numeric(doy) * compname, data = mst, 
          col.regions = stripe.colors,
          par.settings = tactile.theme(),
          xlab = 'Day of the Year',
          ylab = '',
          main='Pr(<=Dry)\n1988-2018',
          scales=list(x=list(tick.number=25))
)



# a different take: "on average, when do the soils
xyplot(Pr ~ as.numeric(doy), groups = compname,
       main='Expected Soil Moisture State\n1988-2018',
       data = mst, type=c('l', 'g'),
       xlab = '', ylab='Proportion',
       as.table=TRUE,
       auto.key=list(lines=TRUE, points=FALSE, columns=length(levels(msp$compname))),
       strip=strip.custom(bg=grey(0.9)),
       par.strip.text=list(cex=1.25),
       scales=list(y=list(alternating=3, cex=1), x=list(relation='free', alternating=1, cex=0.85, rot=0)),
       par.settings = tactile.theme(superpose.line = list(lwd = 2))
)


tapply(mst$Pr, mst$compname, function(i) length(which(i > 0.8)))



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
