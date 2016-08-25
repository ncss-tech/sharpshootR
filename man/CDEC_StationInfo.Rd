\name{CDEC_StationInfo}
\alias{CDEC_StationInfo}

\title{CDEC Sensor Details (by Station)}
\description{Query CDEC Website for Sensor Details}
\usage{
CDEC_StationInfo(s)
}

\arguments{
  \item{s}{a CDEC station ID (e.g. 'HHM')}
}
\details{This function requires the `rvest` package}
\value{a `data.frame` object}

\author{D.E. Beaudette}
\note{Use with caution, this is still experimental.}


\seealso{\code{\link{CDECquery}}}
\examples{
\dontrun{
CDEC_StationInfo('HHM')
}
}

\keyword{ manip }% use one of  RShowDoc("KEYWORDS")

