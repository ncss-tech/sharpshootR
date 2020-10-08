\name{LL2PLSS}
\alias{LL2PLSS}

\alias{plssMeridians}
\alias{d4}

\title{PLSS2LL}
\description{Uses lattitude and longitude coordinates to return the PLSS section geometry from the BLM PLSS web service.}

\usage{LL2PLSS(x, y, returnlevel)}

\arguments{
  \item{x}{longitude coordinates}
  \item{y}{lattitude coordinates}		
  \item{returnlevel}{'S' for "Section" or 'I' for "Intersection" (subsections)}
} 
\details{This function takes xy coordinates and returns the PLSS section geometry to the quarter-quarter section. \code{returnlevel} options are defaulted to 'I' which returns smallest intersected sectional aliquot geometry, 'S' will return the section geometry of the coordinates. See https://gis.blm.gov/arcgis/rest/services/Cadastral/BLM_Natl_PLSS_CadNSDI/MapServer for details.}
\value{\code{list} of of PLSS codes and coordinates.}
\author{D.E. Beaudette, Jay Skovlin}
\note{This function requires the following packages: \code{httr}, \code{jsonlite}, and \code{sp}.}

\seealso{\code{\link{PLSS2LL}}, \code{\link{formatPLSS}}}

\examples{
\donttest{
if(requireNamespace("curl") &
   curl::has_internet() &
   require(sp)) {
  
  # create coordinates
x <- -115.3823 
y <- 48.88228

# fetch PLSS geometry for these coordinates
p.plss <- LL2PLSS(x, y)

# plot geometry
plot(p.plss$geom)
  
}

}
}

\keyword{manip}
