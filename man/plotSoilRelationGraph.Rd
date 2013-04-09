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


## get similar data from soilweb, for the Pardee series
if(require(plyr)) {
	s <- 'pardee'
	u <- url(URLencode(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/reflector_api/soils.php?what=soil_series_component_query&q_string=', s, sep='')))

	# fetch data
	d <- read.table(u, sep='|', header=TRUE, stringsAsFactors=FALSE)

	# normalize component names
	d$compname <- tolower(d$compname)

	# aggregate component percentages when multiple components of the same name are present
	d <- ddply(d, c('mukey','compname'), summarise, comppct_r=sum(comppct_r))
	d <- d[order(d$mukey, d$comppct_r), ]

	# build weighted-adj. matrix and plot
	m <- component.adj.matrix(d, wt='comppct_r')
	plotSoilRelationGraph(m, s=s, type='dendrogram')
}
}

\keyword{hplot}

