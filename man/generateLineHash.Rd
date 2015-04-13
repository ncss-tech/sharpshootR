\name{generateLineHash}
\alias{generateLineHash}

\title{Generate a unique ID for line segments}
\description{Generate a unique ID for a line segment, based on the non-cryptographic murmur32 hash.}
\usage{
generateLineHash(x, precision=-1, algo='murmur32')
}

\arguments{
  \item{x}{a \code{SpatialLinesDataFrame} object, with 1 line segment per feature (e.g. simple features)}
  \item{precision}{digits are rounded to this many places to the right (negative) or left (positive) of the decimal place}
  \item{algo}{hash function algorithm}
}
\details{The input \code{SpatialLinesDataFrame} object must NOT contain multi-part features. The precision specified should be tailored to the coordinate system in use and the snapping tolerance used to create join decision line segments. A \code{precision} of 4 is reasonable for geographic coordinates (snapping tolerance of 0.0001 degrees or ~ 10 meters). A \code{precision} of -1 (snapping tolerance of 10 meters) is reasonable for projected coordinate systems with units in meters.}

\value{A vector of unique IDs created from the hash of line segment start and end vertex coordinates. Unique IDs are returned in the order of records of \code{x} and can therefore be saved into a new column of the associated attribute table.}

\note{An error is issued if any non-unique IDs are generated. This could be casued by using coordiantes that do not contain enough precision for unique hashing.}

\author{D.E. Beaudette}

\keyword{manip}

