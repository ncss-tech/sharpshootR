\name{constantDensitySampling}
\alias{constantDensitySampling}

\title{Constant Density Sampling}
\description{Perform sampling at a constant density over all polygons within a SpatialPolygonsDataFrame object.}
\usage{
constantDensitySampling(x, polygon.id = "pID", ...)
}

\arguments{
  \item{x}{a \code{SpatialPolygonsDataFrame} object}
  \item{polygon.id}{name of attribute in \code{x} that contains a unique ID for each polygon}
  \item{\dots}{further arguments to \code{sample.by.poly}}
}


\value{a \code{SpatialPointsDataFrame} object}


\author{D.E. Beaudette}
\note{This function expects that \code{x} has coordinates associated with a projected CRS and units of meters.}

\seealso{\code{\link{sample.by.poly}}}

\keyword{manip}

