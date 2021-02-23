\name{plotTransect}
\alias{plotTransect}

\title{Plot a collection of Soil Profiles linked to their position along some gradient (e.g. transect).}
\description{Plot a collection of Soil Profiles linked to their position along some gradient (e.g. transect).}
\usage{
plotTransect(s, grad.var.name, 
grad.var.order = order(site(s)[[grad.var.name]]),
transect.col = "RoyalBlue", 
tick.number=7, y.offset = 100, 
scaling.factor = 0.5, 
distance.axis.title = "Distance Along Transect (km)", 
crs = NULL, grad.axis.title = NULL, dist.scaling.factor=1000,
spacing='regular',
fix.relative.pos=list(thresh = 0.6, maxIter = 5000),
...)
}

\arguments{
  \item{s}{a \code{SoilProfileCollection} object}
  \item{grad.var.name}{the name of a site-level attribute containing gradient values}
  \item{grad.var.order}{optional vector of indices used to override sorting along \code{grad.var.name}}
  \item{transect.col}{color used to plot gradient (transect) values}
  \item{tick.number}{number of desired ticks and labels on the gradient axis}
  \item{y.offset}{vertical offset used to position profile sketches}
  \item{scaling.factor}{scaling factor used to adjust profile sketches}
  \item{distance.axis.title}{a tital for the along-transect distances}
  \item{crs}{an optional \code{CRS} object used to convert coordinates into a planar system}
  \item{grad.axis.title}{a title for the gradient axis}
  \item{dist.scaling.factor}{scaling factor applied to linear distance units, default is conversion from m to km}
  \item{spacing}{regular (profiles aligned to an integer grid) or relative (relative distance along transect) spacing}
  \item{fix.relative.pos}{FALSE to suppress, otherwise list of arguments to \code{aqp::fixOverlap}}
  \item{\dots}{further arguments passed to \code{plotSPC}}
}

\details{Depending on the nature of your SoilProfileCollection and associated gradient values, it may be necessary to tinker with figure margins, \code{y.offset} and \code{scaling.factor}.}
\note{This function is very much a work in progress, ideas welcome!}

\value{An invisibly-returned \code{data.frame} object:
\describe{
  \item{scaled.grad}{scaled gradient values}
  \item{scaled.distance}{cumulative distance, scaled to the interval of \code{0.5, nrow(coords) + 0.5}}
  \item{distance}{cumulative distance computed along gradient, e.g. transect distance}
  \item{variable}{sorted gradient values}
  \item{x}{x coordinates, ordered by gradient values}
  \item{y}{y coordinate, ordered by gradient values}
  \item{grad.order}{a vector index describing the sort order defined by gradient values}
  }}

\author{D.E. Beaudette}


\examples{
\donttest{
if(require(aqp) & 
require(sp) &
require(soilDB)
) {

# sample data
data("mineralKing", package = "soilDB")

# quick overview
par(mar=c(1,1,2,1))
groupedProfilePlot(mineralKing, groups='taxonname', print.id=FALSE)

# init coords and CRS
coordinates(mineralKing) <- ~ x_std + y_std
proj4string(mineralKing) <- '+proj=longlat +datum=NAD83'

# projected CRS, units of meters
crs.utm <- CRS('+proj=utm +zone=11 +datum=NAD83')

# adjust margins
par(mar=c(4.5,4,4,1))

# standard transect plot, profile sketches arranged along integer sequence
plotTransect(mineralKing, grad.var.name='elev_field', crs=crs.utm, 
             grad.axis.title='Elevation (m)', label='pedon_id', name='hzname')

# default behavior, attempt adjustments to prevent over-plot and preserve relative spacing
# use set.seed() to fix outcome
plotTransect(mineralKing, grad.var.name='elev_field', crs=crs.utm, 
             grad.axis.title='Elevation (m)', label='pedon_id', 
             name='hzname', width=0.15, spacing = 'relative')

# attempt relative positioning based on scaled distances, no corrections for overlap
# profiles are clustered in space and therefore over-plot
plotTransect(mineralKing, grad.var.name='elev_field', crs=crs.utm, 
             grad.axis.title='Elevation (m)', label='pedon_id', name='hzname', 
             width=0.15, spacing = 'relative', fix.relative.pos = FALSE)

# customize arguments to aqp::fixOverlap()
plotTransect(mineralKing, grad.var.name='elev_field', crs=crs.utm, 
             grad.axis.title='Elevation (m)', label='pedon_id', name='hzname', 
             width=0.15, spacing = 'relative', 
             fix.relative.pos = list(maxIter=6000, adj=0.2, thresh=0.7))

plotTransect(mineralKing, grad.var.name='elev_field', crs=crs.utm, 
             grad.axis.title='Elevation (m)', label='pedon_id', name='hzname', 
             width=0.15, spacing = 'relative', 
             fix.relative.pos = list(maxIter=6000, adj=0.2, thresh=0.5))


}
}
}

\keyword{hplots}

