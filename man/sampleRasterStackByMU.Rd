\name{sampleRasterStackByMU}
\alias{sampleRasterStackByMU}
\alias{ESS_by_Moran_I}
\alias{Moran_I_ByRaster}
\alias{samplingStability}

\title{Sample a Raster Stack}

\description{Sample a raster stack by map unit polygons, at a constant density.}

\usage{
sampleRasterStackByMU(mu, mu.set, mu.col, raster.list, pts.per.acre, 
p = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), progress = TRUE,
estimateEffectiveSampleSize=TRUE)
}

\arguments{
  \item{mu}{a \code{SpatialPolygonsDataFrame} object in a projected coordinate reference system (CRS)}
  \item{mu.set}{character vector of map unit labels to be sampled}
  \item{mu.col}{column name in attribute table containing map unit labels}
  \item{raster.list}{a \code{list} containing raster names and paths, see details below}
  \item{pts.per.acre}{target sampling density in `points per acre`}
  \item{p}{percentiles for polygon area stats, e.g. (0.05, 0.25, 0.5, 0.75, 0.95)}
  \item{progress}{logical, print a progress bar while sampling?}
  \item{estimateEffectiveSampleSize}{estimate an effective sample size via Moran's I?}
}

\details{This function is used by various NRCS reports that summarize or compare concepts defined by collections of polygons using raster data sampled from within each polygon, at a constant sampling density. Even though the function name includes "rasterSTack", this function doesn't actually operate on a `stack` object as defined in the raster package. The collection of raster data defined in \code{raster.list} do not have to share a common coordinate reference system, grid spacing, or extent. Point samples generated from \code{mu} are automatically converted to the CRS of each raster before extracting values. The extent of each raster in \code{raster.list} must completely contain the extent of \code{mu}.}

\value{A \code{list} containing:

\describe{
	\item{`raster.samples`}{a \code{data.frame} containing samples from all rasters in the stack}
	\item{`area.stats`}{a \code{data.frame} containing area statistics for all map units in the collection}
	\item{`unsampled.ids`}{an index to rows in the original SPDF associated with polygons not sampled}
	\item{`raster.summary`}{a \code{data.frame} containing information on sampled rasters}
	\item{`Moran_I`}{a \code{data.frame} containing estimates Moran's I (index of spatial autocorrelation)}
	} 

}

\author{D.E. Beaudette}

\seealso{\code{\link{constantDensitySampling}}, \code{\link{sample.by.poly}}}

\keyword{ manip }

