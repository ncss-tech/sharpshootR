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
\value{a `list` object containing site metadata, sensor metadata, and possibly comments about the site}

\author{D.E. Beaudette}

\seealso{\code{\link{CDECquery}}}
\examples{
\dontrun{
CDEC_StationInfo('HHM')
}
}

\keyword{ manip }

