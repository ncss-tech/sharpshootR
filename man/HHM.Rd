\name{HHM}
\alias{HHM}
\docType{data}

\title{Highland Meadows}

\description{11 years of climate data from the Highland Meadows weather station, as maintained by CA DWR.}

\usage{data("HHM")}

\format{
  A data frame with 3469 observations on the following 12 variables.
  \describe{
    \item{\code{station_id}}{a character vector}
    \item{\code{dur_code}}{a character vector}
    \item{\code{sensor_num}}{a numeric vector}
    \item{\code{sensor_type}}{a character vector}
    \item{\code{value}}{a numeric vector}
    \item{\code{flag}}{a character vector}
    \item{\code{units}}{a character vector}
    \item{\code{datetime}}{a POSIXct}
    \item{\code{year}}{a numeric vector}
    \item{\code{month}}{a factor with levels \code{January} \code{February} \code{March} \code{April} \code{May} \code{June} \code{July} \code{August} \code{September} \code{October} \code{November} \code{December}}
    \item{\code{water_year}}{a numeric vector}
    \item{\code{water_day}}{a numeric vector}
  }
}


\keyword{datasets}
