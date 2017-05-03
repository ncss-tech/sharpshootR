\name{LL2PLSS}
\alias{LL2PLSS}

\title{PLSS2LL}
\description{Uses lattitude and longitude coordinates to return the PLSS section geometry from the BLM PLSS web service.}
\usage{LL2PLSS(x, y)}

\arguments{
  \item{x}{longitude coordinates}
  \item{y}{lattitude coordinates}		
}
\details{This function takes xy coordinates and returns the PLSS section geometry to the quarter-quarter section.}
\value{x dataframe of PLSS codes and coordinates.}
\author{D.E. Beaudette, Jay Skovlin}
\note{This function requires the following packages: \code{httr}, \code{jsonlite}, and \code{sp}.}

\seealso{\code{\link{PLSS2LL}}, \code{\link{formatPLSS}}}

\examples{
library(sp)

# create coordinates
x=-115.3823 
y=48.88228

# fetch PLSS geometry for these coordinates
p.plss <- LL2PLSS(x,y)

# plot geometry
plot(p.plss$geom)
}

\keyword{manip}