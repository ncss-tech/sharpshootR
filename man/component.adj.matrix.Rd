\name{component.adj.matrix}
\alias{component.adj.matrix}

\title{Create an adjacency matrix from a data.frame of component data}
\description{Create an adjacency matrix from a data.frame of component data}
\usage{
component.adj.matrix(d, mu = "mukey", co = "compname", wt=NULL)
}

\arguments{
  \item{d}{a data.frame}
  \item{mu}{name of the column containing the map unit ID (typically 'mukey')}
  \item{co}{name of the column containing the component ID (typically 'compname')}
  \item{wt}{name of the column containing the component weight percent (typically 'comppct_r')}
}
\details{}
\value{an ajacency matrix suitable for use with igraph functions}
\author{D.E. Beaudette}


\examples{
# load sample data set
data(amador)

# convert into unweighted adjacency matrix
m <- component.adj.matrix(amador)

# plot network diagram, with Amador soil highlighted
plotSoilRelationGraph(m, s='amador')
}

\keyword{manip}

