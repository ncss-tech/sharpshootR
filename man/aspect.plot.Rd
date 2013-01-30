\name{aspect.plot}
\alias{aspect.plot}

\title{Plot Aspect Data}
\description{Plot a graphical summary of multiple aspect measurements on a circular diagram.}
\usage{
aspect.plot(p, p.bins = 60, p.bw = 30, p.axis = seq(0, 350, by = 10), plot.title = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{a vector of aspect angles in degrees, measured clock-wise from North}
  \item{p.bins}{number of bins to use for circular histogram}
  \item{p.bw}{bandwidth used for circular density estimation}
  \item{p.axis}{a sequence of integers (degrees) describing the circular axis}
  \item{plot.title}{an informative title}
}
\details{Spread and central tendency are depicted with a combination of circular histogram and kernel density estimate. The circular mean, and relative confidence in that mean are depicted with an arrow: longer arrow lengths correspond to greater confidence in the mean.}

\author{D.E. Beaudette}

\note{Manual adjustment of \code{p.bw} may be required in order to get an optimal circular density plot.}

\examples{
# simulate some data
p.narrow <- runif(n=25, min=215, max=280)
p.wide <- runif(n=25, min=0, max=270)

# set figure margins to 0, 2-column plot
par(mar=c(0,0,0,0), mfcol=c(1,2))

# plot 
aspect.plot(p.narrow, p.bw=10, plot.title='Soil A')
aspect.plot(p.wide, p.bw=10, plot.title='Soil B')
}

\keyword{hplots}
