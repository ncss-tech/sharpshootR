\name{plotSoilRelationGraph}
\alias{plotSoilRelationGraph}
\title{Plot a component relation graph}
\description{Plot a component relation graph based on an adjacency or similarity matrix.}
\usage{
plotSoilRelationGraph(m, s='', plot.style = "network", ...)
}

\arguments{
  \item{m}{adjacency matrix}
  \item{s}{central component; an empty character string is interpreted as no central component}
  \item{plot.style}{plot style ('network', or 'dendrogram')}
  \item{\dots}{further arguments passed to plotting function}
}

\author{D.E. Beaudette}


\examples{
# load sample data set
data(amador)

# create weighted adjacency matrix (see ?component.adj.matrix for details)
m <- component.adj.matrix(amador)

# plot network diagram, with Amador soil highlighted
plotSoilRelationGraph(m, s='amador')

# dendrogram representation
plotSoilRelationGraph(m, s='amador', plot.style='dendrogram')

# compare methods
m.o <- component.adj.matrix(amador, method='occurrence')

par(mfcol=c(1,2))
plotSoilRelationGraph(m, s='amador', plot.style='dendrogram')
title('community matrix')
plotSoilRelationGraph(m.o, s='amador', plot.style='dendrogram')
title('occurence')

\dontrun{
# get similar data from soilweb, for the Pardee series
s <- 'pardee'
u <- url(URLencode(paste(
'http://casoilresource.lawr.ucdavis.edu/soil_web/reflector_api/soils.php?',
'what=soil_series_component_query&q_string=', s, sep='')))

# fetch data
d <- read.table(u, sep='|', header=TRUE, stringsAsFactors=FALSE)

# normalize component names
d$compname <- tolower(d$compname)

# keep only major components
d <- subset(d, subset=compkind == 'Series')

# build adj. matrix and plot
m <- component.adj.matrix(d)
plotSoilRelationGraph(m, s=s, plot.style='dendrogram')

# alter plotting style, see ?plot.phylo
plotSoilRelationGraph(m, s=s, plot.style='dendrogram', type='fan')
plotSoilRelationGraph(m, s=s, plot.style='dendrogram', type='unrooted', use.edge.length=FALSE)
}
}

\keyword{hplot}

