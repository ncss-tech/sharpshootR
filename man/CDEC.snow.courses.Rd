\name{CDEC.snow.courses}
\alias{CDEC.snow.courses}
\docType{data}
\title{CDEC Snow Course List}
\description{The CDEC snow course list, updated September 2019}
\usage{data(CDEC.snow.courses)}
\format{
  A data frame with 259 observations on the following 9 variables.
  \describe{
    \item{\code{course_number}}{course number}
    \item{\code{name}}{connotative course label}
    \item{\code{id}}{course ID}
    \item{\code{elev_feet}}{course elevation in feet}
    \item{\code{latitude}}{latitude}
    \item{\code{longitude}}{longitude}
    \item{\code{april.1.Avg.inches}}{average inches of snow as of April 1st}
    \item{\code{agency}}{responsible agency}
    \item{\code{watershed}}{watershed label}
  }
}

\source{Data were scraped from \url{http://cdec.water.ca.gov/misc/SnowCourses.html}, 2019.}

\examples{
data(CDEC.snow.courses)
head(CDEC.snow.courses)
}
\keyword{datasets}
