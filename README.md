
[![R build
status](https://github.com/ncss-tech/sharpshootR/workflows/R-CMD-check/badge.svg)](https://github.com/ncss-tech/sharpshootR/actions)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/sharpshootR)](http://cran.r-project.org/web/packages/sharpshootR)
[![Total_Downloads](http://cranlogs.r-pkg.org/badges/grand-total/sharpshootR)](https://cran.r-project.org/package=sharpshootR)
[![sharpshootR
Manual](https://img.shields.io/badge/docs-HTML-informational)](http://ncss-tech.github.io/sharpshootR/)

# sharpshootR

This package contains a mish-mash of functionality and sample data
related to the daily business of soil survey operations with the
USDA-NRCS. Many of the functions are highly specialized and inherit
default arguments from the names used by the various NCSS (National
Cooperative Soil Survey) databases.

## Installation

Get the stable version from CRAN:

``` r
install.packages('sharpshootR', dep = TRUE)
```

Get the development version from Github, after installing the CRAN
version + dependencies:

``` r
remotes::install_github("ncss-tech/sharpshootR", dependencies=FALSE, upgrade=FALSE, build=FALSE)
```

### Install Suggested Packages

``` r
p <- c("MASS", "spdep", "circlize", "rvest", "xml2", "terra", 
       "raster", "exactextractr", "httr", "jsonlite", "igraph", 
       "dendextend", "testthat", "hydromad", "latticeExtra", 
       "farver", "venn", "gower", "daymetr", "elevatr", 
       "Evapotranspiration", "zoo", "SoilTaxonomy", "sf", "Hmisc"
)

install.packages(p)
```

## Website

<http://ncss-tech.github.io/AQP/>

## Examples

``` r
library(sharpshootR)
library(aqp)

# some example soil series, from soilDB::fetchOSD()
data("OSDexamples")

# extract Soil Profile Collection
x <- OSDexamples$SPC

# use the first 10 profiles
x <- x[1:10, ]

# arrange according to subgroup classification
SoilTaxonomyDendrogram(
  x, 
  KST.order = TRUE,
  scaling.factor = 0.02,
  cex.taxon.labels = 0.75,
  width = 0.33, 
  max.depth = 150, 
  depth.axis = list(line = -3, cex = 0.8, style = 'compact'),
  hz.distinctness.offset = 'hzd'
  )
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="864" style="display: block; margin: auto;" />

## Citation

``` r
citation("sharpshootR")
#> To cite package 'sharpshootR' in publications use:
#> 
#>   Beaudette D, Skovlin J, Roecker S, Brown A (????). _sharpshootR: A
#>   Soil Survey Toolkit_. doi:10.32614/CRAN.package.sharpshootR
#>   <https://doi.org/10.32614/CRAN.package.sharpshootR>, R package
#>   version 2.4, <https://CRAN.R-project.org/package=sharpshootR>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {sharpshootR: A Soil Survey Toolkit},
#>     author = {Dylan Beaudette and Jay Skovlin and Stephen Roecker and Andrew Brown},
#>     note = {R package version 2.4},
#>     url = {https://CRAN.R-project.org/package=sharpshootR},
#>     doi = {10.32614/CRAN.package.sharpshootR},
#>   }
```

## Quick Reference

### Visualization

- [`aggregateColorPlot`](http://ncss-tech.github.io/sharpshootR/reference/aggregateColorPlot.html)
- [`aspect.plot`](http://ncss-tech.github.io/sharpshootR/reference/aspect.plot.html)
- [`diagnosticPropertyPlot`](http://ncss-tech.github.io/sharpshootR/reference/diagnosticPropertyPlot.html)
- [`plotAvailWater`](http://ncss-tech.github.io/sharpshootR/reference/plotAvailWater.html)
- [`plotProfileDendrogram`](http://ncss-tech.github.io/sharpshootR/reference/plotProfileDendrogram.html)
- [`plotSoilRelationGraph`](http://ncss-tech.github.io/sharpshootR/reference/plotSoilRelationGraph.html)
- [`plotSoilRelationChordGraph`](http://ncss-tech.github.io/sharpshootR/reference/plotSoilRelationChordGraph.html)
- [`plotTransect`](http://ncss-tech.github.io/sharpshootR/reference/plotTransect.html)
- [`SoilTaxonomyDendrogram`](http://ncss-tech.github.io/sharpshootR/reference/SoilTaxonomyDendrogram.html)
- [`vizGeomorphicComponent`](http://ncss-tech.github.io/sharpshootR/reference/vizHillslopePosition.html)
- [`vizHillslopePosition`](http://ncss-tech.github.io/sharpshootR/reference/vizHillslopePosition.html)
- [`vizFlatsPosition`](http://ncss-tech.github.io/sharpshootR/reference/vizFlatsPosition.html)
- [`vizTerracePosition`](http://ncss-tech.github.io/sharpshootR/reference/vizTerracePosition.html)
- [`vizMountainPosition`](http://ncss-tech.github.io/sharpshootR/reference/vizMountainPosition.html)
- [`vizAnnualClimate`](http://ncss-tech.github.io/sharpshootR/reference/vizAnnualClimate.html)
- [`plotWB`](http://ncss-tech.github.io/sharpshootR/reference/plotWB.html)
- [`plotWB_lines`](http://ncss-tech.github.io/sharpshootR/reference/plotWB_lines.html)

### Climate

- [`monthlyWB`](http://ncss-tech.github.io/sharpshootR/reference/monthlyWB.html)
- [`simpleWB`](http://ncss-tech.github.io/sharpshootR/reference/simpleWB.html)
- [`dailyWB`](http://ncss-tech.github.io/sharpshootR/reference/dailyWB.html)
- [`dailyWB_SSURGO`](http://ncss-tech.github.io/sharpshootR/reference/dailyWB_SSURGO.html)
- [`CDECquery`](http://ncss-tech.github.io/sharpshootR/reference/CDECquery.html)
- [`CDECsnowQuery`](http://ncss-tech.github.io/sharpshootR/reference/CDECsnowQuery.html)
- [`FFD`](http://ncss-tech.github.io/sharpshootR/reference/FFD.html)
- [`FFDplot`](http://ncss-tech.github.io/sharpshootR/reference/FFD.html)
- [`PCP_plot`](http://ncss-tech.github.io/sharpshootR/reference/PCP_plot.html)
- [`waterDayYear`](http://ncss-tech.github.io/sharpshootR/reference/waterDayYear.html)

### Spatial Data

- [`constantDensitySampling`](http://ncss-tech.github.io/sharpshootR/reference/constantDensitySampling.html)
- [`generateLineHash`](http://ncss-tech.github.io/sharpshootR/reference/generateLineHash.html)
- [`polygonAdjacency`](http://ncss-tech.github.io/sharpshootR/reference/polygonAdjacency.html)
- [`PLSS2LL`](http://ncss-tech.github.io/sharpshootR/reference/PLSS2LL.html)
- [`LL2PLSS`](http://ncss-tech.github.io/sharpshootR/reference/LL2PLSS.html)
- [`sample.by.poly`](http://ncss-tech.github.io/sharpshootR/reference/sample.by.poly.html)
- [`samplingStability`](http://ncss-tech.github.io/sharpshootR/reference/samplingStability.html)
- [`sampleRasterStackByMU`](http://ncss-tech.github.io/sharpshootR/reference/sampleRasterStackByMU.html)

### Utility

- [`component.adj.matrix`](http://ncss-tech.github.io/sharpshootR/reference/component.adj.matrix.html)
- [`dist.along.grad`](http://ncss-tech.github.io/sharpshootR/reference/dist.along.grad.html)

### Misc.

- [`percentileDemo`](http://ncss-tech.github.io/sharpshootR/reference/percentileDemo.html)
- [`multinominal2logical`](http://ncss-tech.github.io/sharpshootR/reference/multinominal2logical.html)
- [`site_photos_kml`](http://ncss-tech.github.io/sharpshootR/reference/site_photos_kml.html)

## Related Packages

- [`aqp`](https://github.com/ncss-tech/aqp)
- [`soilDB`](https://github.com/ncss-tech/soilDB)

## Related Presentations / Posters

- [Numerical Classification of Soil Profiles (2023 NCSS
  Meetings)](https://ncss-tech.github.io/AQP/presentations/2023-NCSS-NCSP-poster.pdf)
