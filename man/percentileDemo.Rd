\name{percentileDemo}
\alias{percentileDemo}

\title{Demonstration of Percentiles vs. Mean / SD}
\description{This function can be used to graphically demonstrate the relationship between distribution shape, an idealized normal distribution (based on sample mean and sd) shape, and measures of central tendency / spread.}
\usage{
percentileDemo(x, labels.signif = 3, pctile.color = "RoyalBlue", 
mean.color = "Orange", range.color = "DarkRed", 
hist.breaks = 30, boxp = FALSE, ...)
}

\arguments{
  \item{x}{vector of values to summarize}
  \item{labels.signif}{integer, number of significant digits to be used in figure annotation}
  \item{pctile.color}{color used to demonstrate range from 10th to 90th percentiles}
  \item{mean.color}{color used to specify mean +/- 2SD}
  \item{range.color}{color used to specify data range}
  \item{hist.breaks}{integer, number of suggested breaks to \code{hist}}
  \item{boxp}{logical, add a box and whisker plot?}
  \item{\dots}{further arguments to \code{plot}}
}


\value{A 1-row matrix of summary stats is invisibly returned.}

\references{
\url{https://ncss-tech.github.io/soil-range-in-characteristics/why-percentiles.html}
}

\author{D.E. Beaudette}

\note{
This function is mainly for educational purposes.
}

\examples{
if (requireNamespace("Hmisc")) {
  x <- rnorm(100)
  percentileDemo(x)
  
  x <- rlnorm(100)
  percentileDemo(x)
}
}

\keyword{hplots}

