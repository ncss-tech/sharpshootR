\name{CDECquery}
\alias{CDECquery}

\title{Get water-related data (California only) from the CDEC website.}
\description{Get water-related data (California only) from the CDEC website.}

\usage{CDECquery(id, sensor, interval = "D", start, end)}

\arguments{
  \item{id}{station ID (e.g. 'spw'), see details}
  \item{sensor}{the sensor ID (e.g. 45), see details}
  \item{interval}{character, 'D' for daily, 'H' for hourly, 'M' for monthly}
  \item{start}{starting date, in the format 'YYYY-MM-DD'}
  \item{end}{ending date, in the format 'YYYY-MM-DD'}
}

\value{a \code{data.frame} object with the following fields: `datetime`, `year`, `month`, `value`.}
\references{\url{http://cdec.water.ca.gov/queryCSV.html}}
\author{D.E. Beaudette}

\details{
\describe{
  \item{Station IDs can be found here: \url{http://cdec.water.ca.gov/staInfo.html}}
  \item{Sensor IDs can be found using this URL \url{cdec.water.ca.gov/cgi-progs/queryCSV?station_id=}, followed by the station ID.}
}
}


\seealso{
\code{\link{CDECsnowQuery}}
}

\examples{
\dontrun{
library(RColorBrewer)
library(plyr)
library(e1071)

# get daily precip totals from Stan Powerhouse
x <- CDECquery(id='spw', sensor=45, interval='D', start='1900-01-01', end='2015-01-01')

# compute total precip by year/month
a <- ddply(x, c('year', 'month'), summarize, s=sum(value, na.rm=TRUE))

# make a nice color ramp function, scaled by the skewness of the underlying distribution
cols <- colorRampPalette(brewer.pal(9, 'Spectral'), space='Lab', interpolate='spline', bias=skewness(a$s, na.rm=TRUE))

# plot, each pixel is colored by the total precip by year/month
levelplot(s ~ year * month, data=a, col.regions=cols, xlab='', ylab='', scales=list(x=list(tick.number=10)), main='Monthly Total Precipitation (inches) SPW')


# get pre-aggregated monthly data from Sonora RS
x <- CDECquery(id='sor', sensor=2, interval='M', start='1900-01-01', end='2015-01-01')

# make a nice color ramp function, scaled by the skewness of the underlying distribution
cols <- colorRampPalette(brewer.pal(9, 'Spectral'), space='Lab', interpolate='spline', bias=skewness(x$value, na.rm=TRUE))

# plot
levelplot(value ~ year * month, data=x, col.regions=cols, xlab='', ylab='', scales=list(x=list(tick.number=20)), main='Monthly Total Precipitation (inches) SOR')
}
}

