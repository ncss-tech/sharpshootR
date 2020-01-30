\name{polygonAdjacency}
\alias{polygonAdjacency}
\title{Evaluate Spatial Adjacency of SpaitialPolygonsDataFrame Objects}
\description{This function utilizes the `spdep` and `igraph` packages to evaluate several measures of spatial connectivity.}
\usage{
polygonAdjacency(x, v='MUSYM', ...)
}

\arguments{
  \item{x}{a \code{SpatialPolygonsDataFrame} object}
  \item{v}{name of the field in the attribute table to use when searching for `common lines`, see details}
  \item{\dots}{additional arguments passed to \code{spdep::poly2nb}}
}

\details{Examples are presented in \href{http://ncss-tech.github.io/AQP/sharpshootR/common-soil-lines.html}{this tutorial}.}

\value{A \code{list} object containing:
\describe{
  \item{commonLines}{An integer vector of feature IDs, that share a common boundary and attribute \code{v.commonLines}. Sometimes referred to as "common soil lines".}
  \item{adjMat}{A weighted adjacency matrix}
  }
}

\author{D.E. Beaudette}



\keyword{manip}

