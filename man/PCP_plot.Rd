\name{PCP_plot}
\alias{PCP_plot}

\title{Percentiles of Cumulative Precipitation}
\description{Generate a plot representing percentiles of cumulative precipitation, given a historic record, and criteria for selecting a year of data for comparison.}

\usage{
PCP_plot(x, this.year, method = "exemplar", 
q.color = "RoyalBlue", c.color = "firebrick", ...)
}

\arguments{
  \item{x}{result from \code{CDECquery()} for now, will need to generalize to other sources}
  \item{this.year}{a single water year, e.g. 2020}
  \item{method}{'exemplar' or 'daily', currently 'exemplar' is the only method available}
  \item{q.color}{color of percentiles cumulative precipitation}
  \item{c.color}{color of selected year}
  \item{\dots}{addtional arguments to \code{plot()}}
}

\details{This is very much a work in progress. Further examples at \url{http://ncss-tech.github.io/AQP/sharpshootR/CDEC.html}}

\value{Currently nothing is returned.}

\author{D.E. Beaudette}


\seealso{
\code{\link{waterDayYear}}
}

\examples{
\donttest{

if(requireNamespace("curl") &
   curl::has_internet()
   ) {
  
  s <- 'SPW'
# get metadata
s.info <- CDEC_StationInfo(s)
# format title for cumulative PPT
title.text <- sprintf("\%s [\%s]", s.info$site.meta$Name, s)

# get data
x <- CDECquery(id=s, sensor=45, interval='D', start='2000-01-01', end='2030-01-01')

## NOTE: requires sharpshootR >= 1.4.02
# plot
par(mar=c(4.5, 4.5, 2.5, 1.5))
PCP_plot(x[1:(nrow(x)-60), ], ylab='Cumulative PPT (inches)', main=title.text, this.year = 2020)
  
}

}
}

\keyword{ hplots }

