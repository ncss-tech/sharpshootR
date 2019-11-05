\name{monthlyWB}
\alias{monthlyWB}
\alias{plotWB}


\title{Monthly Water Balances}

\description{Monthly water balances and visualization by "leaky bucket" model, provided by the hydromad package.}

\usage{
monthlyWB(AWC, PPT, PET, S_init=AWC, starting_month=1, rep=1, keep_last=FALSE)

plotWB(AWC, WB, fig.title='', sw.col='#377EB8', 
surplus.col='#4DAF4A', et.col='#E41A1C', deficit.col='#FF7F00')
}

\arguments{
  \item{AWC}{available water-holding capacity (mm)}
  \item{PPT}{time-series of monthly PPT (mm), calendar year ordering}
  \item{PET}{time-series of monthly PET (mm), calendar year ordering}
  \item{S_init}{intitial soil water storage (mm)}
  \item{starting_month}{starting month index, 1=January, 9=September}
  \item{rep}{number of cycles to run water balance}
  \item{keep_last}{keep only the last iteration of the water balance}
  
  \item{WB}{output from \code{monthlyWB}}
  \item{fig.title}{a title}
  \item{sw.col}{color for soil water}
  \item{surplus.col}{color for surplus water}
  \item{et.col}{color for ET}
  \item{deficit.col}{color for deficit}
}

\details{This function depends on the \href{http://hydromad.catchment.org/}{hydromad package}.}

\value{a \code{data.frame} with the following elements:

\item{PPT}{monthly PPT values}
\item{PPT}{monthly PET values}
\item{U}{monthly U values}
\item{S}{monthly S values}
\item{ET}{monthly ET values}
\item{D}{monthly D values}
\item{month}{month number}
\item{mo}{month label}
}


\author{D.E. Beaudette}

\note{This is a work in progress.}
