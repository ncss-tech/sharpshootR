\name{vizHillslopePosition}
\alias{vizHillslopePosition}
\alias{vizGeomorphicComponent}

\title{Hillslope / Geomorphic Component Vizualization}

\description{A unique display of hillslope or geomorhphic component probability.}

\usage{
vizHillslopePosition(x, s = NULL)
vizGeomorphicComponent(x, s = NULL)
}


\arguments{
  \item{x}{\code{data.frame} as created by \code{soilDB::fetchOSD(..., extended=TRUE)}, see details}
  \item{s}{an optional soil series name, highlighted in the figure}
}

\details{\href{http://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.html}{Example usage}}

\value{a \code{list} with the following elements:

\item{fig}{lattice object (the figure)}
\item{order}{ordering of soil series}

}

\references{http://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.html}


\author{D.E. Beaudette}
\note{This is a work in progress.}

\examples{
\dontrun{
library(soilDB)

# soils of interest
s.list <- c('musick', 'cecil', 'drummer', 'amador', 'pentz', 'reiff', 
'san joaquin','montpellier','grangeville','pollasky','ramona')

# fetch and convert data into an SPC
s <- fetchOSD(s.list, extended=TRUE)

res <- vizHillslopePosition(s$hillpos)
print(res$fig)

}
}
