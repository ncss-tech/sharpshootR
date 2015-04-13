\name{generateLineHash}
\alias{generateLineHash}

\title{Generate a unique ID for line segments}
\description{Generate a unique ID for a line segment, based on the non-cryptographic murmur32 hash.}
\usage{
generateLineHash(x, algo='murmur32')
}

\arguments{
  \item{x}{a \code{SpatialLinesDataFrame} object, with 1 line segment per feature (e.g. simple features)}
  \item{algo}{hash function algorithm}
}
\details{The input \code{SpatialLinesDataFrame} object must NOT contain multi-part features.}

\value{A vector of unique IDs created from the hash of line segment start and end vertex coordinates. Unique IDs are returned in the order of records of \code{x} and can therefore be saved into a new column of the associated attribute table.}

\note{An error is issued if any non-unique IDs are generated. This could be casued by using coordiantes that do not contain enough precision for unique hashing.}

\author{D.E. Beaudette}

\keyword{manip}

