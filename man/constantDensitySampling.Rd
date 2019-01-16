\name{constantDensitySampling}
\alias{constantDensitySampling}

\title{Constant Density Sampling}
\description{Perform sampling at a constant density over all polygons within a SpatialPolygonsDataFrame object.}
\usage{
constantDensitySampling(x, polygon.id='pID', parallel=FALSE, cores=NULL, 
n.pts.per.ac=1, min.samples=5, sampling.type='regular', iterations=10)
}

\arguments{
  \item{x}{a \code{SpatialPolygonsDataFrame} object in a projected CRS with units of meters}
  \item{polygon.id}{name of attribute in \code{x} that contains a unique ID for each polygon}
  \item{parallel}{invoke parallel back-end}
  \item{cores}{number of CPU cores to use for parallel operation}
  \item{n.pts.per.ac}{requested sampling density in points per acre (results will be close)}
  \item{min.samples}{minimum requested number of samples per polygon}
  \item{sampling.type}{sampling type, see \code{spsample}}
  \item{iterations}{number of tries that \code{spsample} will attempt}
}


\value{a \code{SpatialPointsDataFrame} object}


\author{D.E. Beaudette}
\note{This function expects that \code{x} has coordinates associated with a projected CRS and units of meters.}

\seealso{\code{\link{sample.by.poly}}}

\keyword{manip}

