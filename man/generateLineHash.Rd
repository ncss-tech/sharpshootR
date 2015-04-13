\name{generateLineHash}
\alias{generateLineHash}

\title{Generate a unique ID for line segments}
\description{Generate a unique ID for a line segment, based on the MD5 hash method from the digest package.}
\usage{
generateLineHash(x, precision = -1, ...)
}

\arguments{
  \item{x}{a \code{SpatialLinesDataFrame} object, with 1 line segment per feature (e.g. simple features)}
  \item{precision}{number of places to the right of the decimal point at which to round coordinates}
  \item{\dots}{additional arguments to \code{digest()}}
}
\details{The input \code{SpatialLinesDataFrame} object must NOT contain multi-part features.}

\value{A vector of unique ideas based on an MD5 (or other type specified to \code{digest}) hash of line segment start and end vertex coordinates, after rounding. Unique IDs are returned in the order of records of \code{x} and can therefore be saved into a new column of the associated attribute table.}

\author{D.E. Beaudette}

\keyword{manip}

