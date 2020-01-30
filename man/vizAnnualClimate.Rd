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
\donttest{

if(requireNamespace("curl") &
   curl::has_internet() &
   require(soilDB) & 
   require(latticeExtra)
) {
  
  # soil series of interest
soil <- 'ARBUCKLE'

# get competing series
sdata <- fetchOSD(soil, extended = TRUE)

# get competing series' data
sdata.competing <- fetchOSD(c(soil, sdata$competing$competing))

# only use established series
idx <- which(sdata.competing$series_status == 'established')

# subset as needed
if(length(idx) < length(sdata.competing)) {
  sdata.competing <- sdata.competing[idx, ]
  
}

# now get the extended data
sdata.competing.full <- fetchOSD(site(sdata.competing)$id, extended = TRUE)

# extract SPC
spc <- sdata.competing.full$SPC

# full set of series names
s.names <- unique(site(spc)$id)

# todo: probably better ways to do this...
# note: need to load lattice for this to work
trellis.par.set(plot.line=list(col='RoyalBlue'))

# control center symbol and size here
res <- vizAnnualClimate(sdata.competing.full$climate.annual, s=soil, IQR.cex = 1.1, cex=1.1, pch=18)

# plot figure
print(res$fig)

# check clustering
str(res$clust)

# do something with clustering
par(mar=c(0,0,1,1))
plotProfileDendrogram(spc, clust = res$clust, scaling.factor = 0.075, width = 0.2, y.offset = 0.5)
mtext('sorted by annual climate summaries', side = 3, at = 0.5, adj = 0, line = -1.5, font=3)

}


}
}

\keyword{ hplot }

