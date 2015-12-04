\name{sample.by.poly}
\alias{sample.by.poly}

\title{Sample a Polygon at Fixed Density}
\description{Generate sampling points within a SpatialPolygon object, according to a specified sampling density.}
\usage{sample.by.poly(p, n.pts.per.ac=1, min.samples=5, 
sampling.type='hexagonal', iterations=10, p4s=NULL)}

\arguments{
  \item{p}{a Polygon object, with coordinates in a projected CRS}
  \item{n.pts.per.ac}{requested sampling density in points per acre (results will be close)}
  \item{min.samples}{minimum requested number of samples per polygon}
  \item{sampling.type}{sampling type, see \code{spsample}}
  \item{iterations}{number of tries that \code{spsample} will attempt}
  \item{p4s}{a qualified proj4string that will be assigned to sampling points}
}
\details{This function is typically accessed via some kind of helper function such as \code{\link{constantDensitySampling}}.}
\value{A \code{SpatialPoints} object.}
\author{D.E. Beaudette}
\note{This function expects that the \code{Polygon} object has coordinates associated with a projected CRS-- e.g. units of meters.}

\seealso{\code{\link{constantDensitySampling}}}

\keyword{manip}
