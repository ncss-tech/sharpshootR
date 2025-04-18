% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PLSS2LL.R
\name{PLSS2LL}
\alias{PLSS2LL}
\title{PLSS2LL}
\usage{
PLSS2LL(p, plssid = "plssid")
}
\arguments{
\item{p}{\code{data.frame} with chunks of PLSS definition}

\item{plssid}{column name containing PLSS ID}
}
\value{
A \code{data.frame} of PLSS codes and coordinates.
}
\description{
Fetch latitude and longitude (centroid) coordinates for coded PLSS information from the BLM PLSS web service.
}
\note{
This function expects that the input \code{p} will have a 'plssid' column generated by the \code{\link[=formatPLSS]{formatPLSS()}} function. Requires the following packages: httr, and jsonlite.
}
\seealso{
\code{\link[=LL2PLSS]{LL2PLSS()}}, \code{\link[=formatPLSS]{formatPLSS()}}
}
\author{
D.E. Beaudette, Jay Skovlin, A.G. Brown
}
