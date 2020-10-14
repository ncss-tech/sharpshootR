[![Travis-CI Build Status](https://travis-ci.org/ncss-tech/sharpshootR.svg?branch=master)](https://travis-ci.org/ncss-tech/sharpshootR)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/sharpshootR)](https://cran.r-project.org/package=sharpshootR)
[![Total_Downloads](http://cranlogs.r-pkg.org/badges/grand-total/sharpshootR)](https://cran.r-project.org/package=sharpshootR)

# sharpshootR

This package contains mish-mash of functionality and sample data related to the daily business of soil survey operations with the USDA-NRCS. Many of the functions are highly specialized and inherit default arguments from the names used by the various NCSS (National Cooperative Soil Survey) databases.

Install the stable version from CRAN:

`install.packages('sharpshootR', dep=TRUE)`

Install the development version from Github:

`remotes::install_github("ncss-tech/sharpshootR", dependencies=FALSE, upgrade=FALSE, build=FALSE)`

## Website
http://ncss-tech.github.io/AQP/


## Quick Reference

### Vizualization
   * [`aggregateColorPlot`](http://ncss-tech.github.io/sharpshootR/docs/reference/aggregateColorPlot.html)
   * [`aspect.plot`](http://ncss-tech.github.io/sharpshootR/docs/reference/aspect.plot.html)
   * [`diagnosticPropertyPlot`](http://ncss-tech.github.io/sharpshootR/docs/reference/diagnosticPropertyPlot.html)
   * [`dueling.dendrograms`](http://ncss-tech.github.io/sharpshootR/docs/reference/dueling.dendrograms.html)
   * [`plotAvailWater`](http://ncss-tech.github.io/sharpshootR/docs/reference/plotAvailWater.html)
   * [`plotProfileDendrogram`](http://ncss-tech.github.io/sharpshootR/docs/reference/plotProfileDendrogram.html)
   * [`plotSoilRelationGraph`](http://ncss-tech.github.io/sharpshootR/docs/reference/plotSoilRelationGraph.html)
   * [`plotSoilRelationChordGraph`](http://ncss-tech.github.io/sharpshootR/docs/reference/plotSoilRelationChordGraph.html)
   * [`plotTransect`](http://ncss-tech.github.io/sharpshootR/docs/reference/plotTransect.html)
   * [`SoilTaxonomyDendrogram`](http://ncss-tech.github.io/sharpshootR/docs/reference/SoilTaxonomyDendrogram.html)
   * [`vizGeomorphicComponent`](http://ncss-tech.github.io/sharpshootR/docs/reference/vizHillslopePosition.html)
   * [`vizHillslopePosition`](http://ncss-tech.github.io/sharpshootR/docs/reference/vizHillslopePosition.html)
   * [`vizFlatsPosition`](http://ncss-tech.github.io/sharpshootR/docs/reference/vizFlatsPosition.html)
   * [`vizTerracePosition`](http://ncss-tech.github.io/sharpshootR/docs/reference/vizTerracePosition.html)
   * [`vizMountainPosition`](http://ncss-tech.github.io/sharpshootR/docs/reference/vizMountainPosition.html)
   * [`vizAnnualClimate`](http://ncss-tech.github.io/sharpshootR/docs/reference/vizAnnualClimate.html)
   * [`plotWB`](http://ncss-tech.github.io/sharpshootR/docs/reference/plotWB.html)

   
### Climate
   * [`monthlyWB`](http://ncss-tech.github.io/sharpshootR/docs/reference/monthlyWB.html)
   * [`CDECquery`](http://ncss-tech.github.io/sharpshootR/docs/reference/CDECquery.html)
   * [`CDECsnowQuery`](http://ncss-tech.github.io/sharpshootR/docs/reference/CDECsnowQuery.html)
   * [`FFD`](http://ncss-tech.github.io/sharpshootR/docs/reference/FFD.html)
   * [`FFDplot`](http://ncss-tech.github.io/sharpshootR/docs/reference/FFD.html)
   * [`PCP_plot`](http://ncss-tech.github.io/sharpshootR/docs/reference/PCP_plot.html)
   * [`waterDayYear`](http://ncss-tech.github.io/sharpshootR/docs/reference/waterDayYear.html)


### Spatial Data
   * [`constantDensitySampling`](http://ncss-tech.github.io/sharpshootR/docs/reference/constantDensitySampling.html)
   * [`generateLineHash`](http://ncss-tech.github.io/sharpshootR/docs/reference/generateLineHash.html)
   * [`polygonAdjacency`](http://ncss-tech.github.io/sharpshootR/docs/reference/polygonAdjacency.html)
   * [`PLSS2LL`](http://ncss-tech.github.io/sharpshootR/docs/reference/PLSS2LL.html)
   * [`LL2PLSS`](http://ncss-tech.github.io/sharpshootR/docs/reference/LL2PLSS.html)
   * [`sample.by.poly`](http://ncss-tech.github.io/sharpshootR/docs/reference/sample.by.poly.html)
   * [`samplingStability`](http://ncss-tech.github.io/sharpshootR/docs/reference/samplingStability.html)
   * [`sampleRasterStackByMU`](http://ncss-tech.github.io/sharpshootR/docs/reference/sampleRasterStackByMU.html)


### Utility
   * [`component.adj.matrix`](http://ncss-tech.github.io/sharpshootR/docs/reference/component.adj.matrix.html)
   * [`dist.along.grad`](http://ncss-tech.github.io/sharpshootR/docs/reference/dist.along.grad.html)


### Misc.
   * [`percentileDemo`](http://ncss-tech.github.io/sharpshootR/docs/reference/percentileDemo.html)
   * [`multinominal2logical`](http://ncss-tech.github.io/sharpshootR/docs/reference/multinominal2logical.html)
   * [`site_photos_kml`](http://ncss-tech.github.io/sharpshootR/docs/reference/site_photos_kml.html)


## Related Packages
 * [`aqp`](https://github.com/ncss-tech/aqp)
 * [`soilDB`](https://github.com/ncss-tech/soilDB)
 

## Examples
```r
library(sharpshootR)
data(loafercreek, package = 'soilDB')

# generalize horizon names using REGEX rules
n <- c('Oi', 'A', 'BA','Bt1','Bt2','Bt3','Cr','R')
p <- c('O', '^A$|Ad|Ap|AB','BA$|Bw', 
       'Bt1$|^B$','^Bt$|^Bt2$','^Bt3|^Bt4|CBt$|BCt$|2Bt|2CB$|^C$','Cr','R')
loafercreek$genhz <- generalize.hz(loafercreek$hzname, n, p)

# remove non-matching generalized horizon names
loafercreek$genhz[loafercreek$genhz == 'not-used'] <- NA
loafercreek$genhz <- factor(loafercreek$genhz)

# aggregate color data, this function is from the `aqp` package
a <- aggregateColor(loafercreek, 'genhz', k = 8)

# plot
par(mar=c(3,4,4,1))
aggregateColorPlot(a, print.n.hz = TRUE)
```


## Dependency Graph
![](https://cran.microsoft.com/packagedata/graphs/sharpshootR.png)

