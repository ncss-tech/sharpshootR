% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampleRasterStackByMU.R
\name{sampleRasterStackByMU}
\alias{sampleRasterStackByMU}
\title{Sample a Raster Stack}
\usage{
sampleRasterStackByMU(
  mu,
  mu.set,
  mu.col,
  raster.list,
  pts.per.acre,
  p = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
  progress = TRUE,
  estimateEffectiveSampleSize = TRUE,
  polygon.id = "pID"
)
}
\arguments{
\item{mu}{a \code{SpatialPolygonsDataFrame} object in a projected coordinate reference system (CRS)}

\item{mu.set}{character vector of map unit labels to be sampled}

\item{mu.col}{column name in attribute table containing map unit labels}

\item{raster.list}{a \code{list} containing raster names and paths, see details below}

\item{pts.per.acre}{target sampling density in \verb{points per acre}}

\item{p}{percentiles for polygon area stats, e.g. \code{c(0.05, 0.25, 0.5, 0.75, 0.95)}}

\item{progress}{logical, print a progress bar while sampling?}

\item{estimateEffectiveSampleSize}{estimate an effective sample size via Moran's I?}

\item{polygon.id}{Column name containing unique polygon IDs; default: \code{"pID"}; calculated if missing}
}
\value{
A \code{list} containing:
\describe{
\item{\code{raster.samples}}{a \code{data.frame} containing samples from all rasters in the stack}
\item{\code{area.stats}}{a \code{data.frame} containing area statistics for all map units in the collection}
\item{\code{unsampled.ids}}{an index to rows in the original SPDF associated with polygons not sampled}
\item{\code{raster.summary}}{a \code{data.frame} containing information on sampled rasters}
\item{\code{Moran_I}}{a \code{data.frame} containing estimates Moran's I (index of spatial autocorrelation)}
}
}
\description{
Sample a raster stack by map unit polygons, at a constant density.
}
\details{
This function is used by various NRCS reports that summarize or compare concepts defined by collections of polygons using raster data sampled from within each polygon, at a constant sampling density. Even though the function name includes "RasterStack", this function doesn't actually operate on the "stack" object as defined in the raster package. The collection of raster data defined in \code{raster.list} do not have to share a common coordinate reference system, grid spacing, or extent. Point samples generated from \code{mu} are automatically converted to the CRS of each raster before extracting values. The extent of each raster in \code{raster.list} must completely contain the extent of \code{mu}.
}
\seealso{
\code{\link[=constantDensitySampling]{constantDensitySampling()}}, \code{\link[=sample.by.poly]{sample.by.poly()}}
}
\author{
D.E. Beaudette
}
\keyword{manip}
