\name{SoilTaxonomyDendrogram}
\alias{SoilTaxonomyDendrogram}

\title{SoilTaxonomyDendrogram}
\description{SoilTaxonomyDendrogram}
\usage{
SoilTaxonomyDendrogram(spc, name = "hzname", max.depth = 150, n.depth.ticks = 6, scaling.factor = 0.015, cex.names = 0.75, cex.id = 0.75, axis.line.offset = -4, width = 0.1, y.offset = 0.5, cex.taxon.labels = 0.66)
}

\arguments{
  \item{spc}{a \code{SoilProfileCollection} object}
  \item{name}{column name containing horizon names}
  \item{max.depth}{depth at which profiles are truncated for plotting}
  \item{n.depth.ticks}{suggested number of ticks on the depth axis}
  \item{scaling.factor}{scaling factor used to convert depth units into plotting units}
  \item{cex.names}{character scaling for horizon names}
  \item{cex.id}{character scaling for profile IDs}
  \item{axis.line.offset}{horizontal offset for depth axis}
  \item{width}{width of profiles}
  \item{y.offset}{vertical offset between dendrogram and profiles}
  \item{cex.taxon.labels}{character scaling for taxonomic information}
}

\details{}

\references{}
\author{D.E. Beaudette}

\examples{
library(soilDB)

# soils of interest
s.list <- c('musick', 'cecil', 'drummer', 'amador', 'pentz', 'reiff', 'san joaquin','montpellier','grangeville','pollasky','ramona')

# fetch and convert data into an SPC
h <- fetchOSD(s.list)

# plot dendrogram + profiles
SoilTaxonomyDendrogram(h)
}

\keyword{hplots}

