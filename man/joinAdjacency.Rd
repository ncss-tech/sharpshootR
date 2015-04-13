\name{joinAdjacency}
\alias{joinAdjacency}

\title{Join Document Adjacency}
\description{Convert a set of line segment "join decisions" into a weighted adjacency matrix describing which map unit symbols touch.}
\usage{
joinAdjacency(x, vars = c("l_musym", "r_musym"))
}

\arguments{
  \item{x}{a \code{SpatialLinesDataFrame} object, with 1 line segment per feature (e.g. simple features)}
  \item{vars}{a vector of two characters naming columns containing "left", and "right" map unit symbols}
}

\value{A weighted adjacency matrix is returned, suitable for plotting directly with \code{plotSoilRelationGraph}.}

\author{D.E. Beaudette}

\seealso{\code{\link{plotSoilRelationGraph}}}

\keyword{manip}

