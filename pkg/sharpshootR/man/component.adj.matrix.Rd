\name{component.adj.matrix}
\alias{component.adj.matrix}

\title{Create an adjacency matrix from a data.frame of component data}
\description{Create an adjacency matrix from SSURGO component data}
\usage{
component.adj.matrix(d, mu='mukey', co='compname', wt='comppct_r', 
method='community.matrix', standardization='max', metric='jaccard')
}

\arguments{
  \item{d}{a \code{data.frame}, typically of SSURGO data}
  \item{mu}{name of the column containing the map unit ID (typically 'mukey')}
  \item{co}{name of the column containing the component ID (typically 'compname')}
  \item{wt}{name of the column containing the component weight percent (typically 'comppct_r')}
  \item{method}{one of either: `community.matrix`, or `occurrence`; see details}
  \item{standardization}{community matrix standardization method, passed to \code{decostand}}
  \item{metric}{community matrix dissimilarity metric, passed to \code{vegdist}}
}
\value{a similatiy matrix / ajacency matrix suitable for use with igraph functions}
\author{D.E. Beaudette}

\details{Pending...}

\examples{
# load sample data set
data(amador)

# convert into adjacency matrix
m <- component.adj.matrix(amador)

# plot network diagram, with Amador soil highlighted
plotSoilRelationGraph(m, s='amador')
}

\keyword{manip}

