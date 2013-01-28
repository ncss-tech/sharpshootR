\name{plotSoilRelationGraph}
\alias{plotSoilRelationGraph}
\title{Plot a component relation graph}
\description{Plot a component relation graph based on an adjacency matrix.}
\usage{
plotSoilRelationGraph(m, s, type = "network")
}

\arguments{
  \item{m}{adjacency matrix}
  \item{s}{central component}
  \item{type}{plot style ('network', or 'dendrogram')}
}

\author{D.E. Beaudette}


\examples{
# load sample data set
data(amador)

# convert into unweighted adjacency matrix
m <- component.adj.matrix(amador)

# plot network diagram, with Amador soil highlighted
plotSoilRelationGraph(m, s='amador')

# dendrogram representation
plotSoilRelationGraph(m, s='amador', type='dendrogram')

# compare weighted with unweighted adjacancy matrices
m.wt <- component.adj.matrix(amador, wt='comppct_r')

par(mfcol=c(1,2))
plotSoilRelationGraph(m, s='amador', type='dendrogram')
title('unweighted')
plotSoilRelationGraph(m.wt, s='amador', type='dendrogram')
title('weighted')
}

\keyword{hplot}

