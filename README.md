[![Travis-CI Build Status](https://travis-ci.org/ncss-tech/sharpshootR.svg?branch=master)](https://travis-ci.org/ncss-tech/sharpshootR)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/sharpshootR)](https://cran.r-project.org/package=sharpshootR)
[![Total_Downloads](http://cranlogs.r-pkg.org/badges/grand-total/sharpshootR)](https://cran.r-project.org/package=sharpshootR)

# sharpshootR

Install the stable version from CRAN:

`install.packages('sharpshootR', dep=TRUE)`

Install the development version from Github:

`remotes::install_github("ncss-tech/soilDB", dependencies=FALSE, upgrade=FALSE, build=FALSE)`


## Website
http://ncss-tech.github.io/AQP/


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
a <- aggregateColor(loafercreek, 'genhz')

# plot
par(mar=c(1,4,4,1))
aggregateColorPlot(a, print.n.hz = TRUE)
```

## Related Packages
 * [aqp](https://github.com/ncss-tech/aqp)
 * [soilDB](https://github.com/ncss-tech/soilDB)
 
## Dependency Graph
https://cran.microsoft.com/packagedata/graphs/sharpshootR.png

