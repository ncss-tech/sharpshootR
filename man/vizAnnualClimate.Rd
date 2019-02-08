\name{vizAnnualClimate}
\alias{vizAnnualClimate}

\title{Annual Climate Summaries for Soil Series Data}

\description{Annual climate summaries for soil series, based on \code{latticeExtra::segplot}, based on 5th, 25th, 50th, 75th, and 95th percentiles. Input data should be from \code{soilDB::fetchOSD}.}

\usage{
vizAnnualClimate(climate.data, IQR.cex=1, s=NULL, s.col='firebrick', ...)
}

\arguments{
  \item{climate.data}{Annual climate summaries, as returned from \code{soilDB::fetchOSD(..., extended=TRUE)}}
  
  \item{IQR.cex}{scaling factor for bar representing interquartile range}
  
  \item{s}{a soil series name, e.g. "AMADOR", to highlight}
  
  \item{s.col}{color for highlighted soil series}
  
  \item{\dots}{further arguments passed to \code{latticeExtra::segplot}}
}


\details{This function was designed for use with \code{soilDB::fetchOSD}. It might be possible to use with other sources of data but your mileage may vary.}

\value{a \code{list} with the following elements:

\item{fig}{lattice object (the figure)}
\item{clust}{clustering object returned by \code{cluster::diana}}

}


\author{D.E. Beaudette}


\seealso{
\code{\link{vizHillslopePosition}}
}


\examples{
\dontrun{
library(soilDB)

soil <- 'ARBUCKLE'

s <- fetchOSD(soil, extended = TRUE)
spc <- fetchOSD(c(soil, s$competing$competing))
s.names <- unique(site(spc)$id)
s.competing.data <- fetchOSD(s.names, extended = TRUE)

res <- vizAnnualClimate(s.competing.data$climate.annual, s=soil, col='RoyalBlue')
print(res$fig)

x.d <- res$clust

par(mar=c(0,0,1,1))
plotProfileDendrogram(s.competing.data$SPC, clust = x.d, scaling.factor = 0.075, width = 0.2, y.offset = 0.5)
mtext(fm.name, side = 1, at = 0.5, adj = 0, line = -1.5, font=4)
mtext('sorted by annual climate summaries', side = 3, at = 0.5, adj = 0, line = -1.5, font=3)

}
}

\keyword{ hplot }

