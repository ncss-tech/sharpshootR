\name{formatPLSS}
\alias{formatPLSS}

\title{formatPLSS}
\description{Format PLSS information into a coded format that can be digested by PLSS web service.}
\usage{formatPLSS(p, type='SN')}

\arguments{
  \item{p}{data.frame with chunks of PLSS coordinates}
  \item{type}{an option to format protracted blocks 'PB', unprotracted blocks 'UP', or standard section number 'SN' (default).}
}
\details{This function is typically accessed as a helper function to prepare data for use within \code{\link{PLSS2LL}} function.}
\value{A vector of PLSS codes.}
\author{D.E. Beaudette, Jay Skovlin}
\note{This function expects that the \code{Polygon} object has coordinates associated with a projected CRS-- e.g. units of meters.}

\seealso{\code{\link{PLSS2LL}}}

\keyword{manip}
