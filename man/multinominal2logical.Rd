% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multinominal2logical.R
\name{multinominal2logical}
\alias{multinominal2logical}
\title{Convert Multinominal to Logical Matrix}
\usage{
multinominal2logical(x, v)
}
\arguments{
\item{x}{a \code{SoilProfileCollection} object}

\item{v}{the name of a site-level attribute that is a factor, or can be coerced to a factor, with more than 2 levels}
}
\value{
A \code{data.frame} with IDs in the first column, and as many columns of logical vectors as there were levels in \code{v}. See examples.
}
\description{
Convert a single multinominal, site-level attribute from a \code{SoilProfileCollection} into a matrix of corresponding logical values. The result contains IDs from the \code{SoilProfileCollection} and can easily be joined to the original site-level data.
}
\examples{

\donttest{

if(require(soilDB) &
   require(aqp) &
   require(latticeExtra)) {
  
  
  # sample data, an SPC
  data(loafercreek, package='soilDB')
  
  # convert to logical matrix
  hp <- multinominal2logical(loafercreek, 'hillslopeprof')
  
  # join-in to site data
  site(loafercreek) <- hp
  
  # variable names
  v <- c('lithic.contact', 'paralithic.contact', 
         'argillic.horizon', 'toeslope', 'footslope', 
         'backslope', 'shoulder', 'summit')
  
  # visualize with some other diagnostic features
  x <- diagnosticPropertyPlot(loafercreek, v, k = 5, 
                              grid.label = 'bedrckkind', dend.label = 'upedonid')  
}

}

}
\seealso{
\code{\link[=diagnosticPropertyPlot]{diagnosticPropertyPlot()}}
}
\author{
D.E. Beaudette
}
\keyword{manip}
