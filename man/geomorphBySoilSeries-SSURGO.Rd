\name{geomorphBySoilSeries-SSURGO}
\alias{hillslope.probability}
\alias{hillslopeProbability}
\alias{surfaceShapeProbability}
\title{Geomorphic Position Probability via SDA}
\description{Hillslope position probability estimates from the SDA query service (SSURGO)}
\usage{
hillslopeProbability(s, replaceNA=TRUE)
surfaceShapeProbability(s, replaceNA=TRUE)
}
\arguments{
  \item{s}{a character vector of soil series names, automatically normalized to upper case}
  \item{replaceNA}{boolean: should missing classes be converted to probabilities of 0?}
}

\details{These functions send a query to the \href{SDA}{http://sdmdataaccess.nrcs.usda.gov} webservice. Further information on the SDA webservice and query examples can be found at \url{http://sdmdataaccess.nrcs.usda.gov/QueryHelp.aspx}}

\value{A \code{data.frame} object with rows representing soil series, and columns representing probability estimates of that series occuring at specificed geomorphic positions or associated with a surface shape.}

\note{Probability values are computed from SSURGO data.}

\author{D.E. Beaudette}

\examples{
\dontrun{
library(soilDB)

# soil series of interest
s <- c('amador', 'peters', 'pentz', 'inks', 'auburn', 'dunstone', 'argonaut')

# generate hillslope probability table
hillslopeProbability(s)

# generate surface 2D shape probability table
surfaceShapeProbability(s)
}
}

\keyword{manip}

