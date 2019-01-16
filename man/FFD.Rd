\name{FFD}
\alias{FFD}
\alias{FFDplot}
\alias{alignDOY}
\alias{findFirstLastFrostDOY}
\alias{frostFreePeriod}
\alias{makeFrostMatrix}

\title{Frost-Free Day Evaluation}
\description{Evaluation frost-free days and related metrics from daily climate records.}
\usage{
FFD(d, returnDailyPr = TRUE, minDays = 165, ...)
FFDplot(s, sub.title = NULL)
}

\arguments{
  \item{d}{\code{data.frame} with columns 'datetime' 'year', and 'value'}
  \item{returnDailyPr}{optionally return \code{list} with daily summaries}
  \item{minDays}{min number of days / spring|fall required for a reasonable estimate of FFD}
  \item{\dots}{further arguments passed to \code{frostFreePeriod}}
  
  \item{s}{object returned by \code{FFD}}
  \item{sub.title}{override default subtitle}
}

\details{\href{http://ncss-tech.github.io/AQP/sharpshootR/FFD-estimates.html}{FFD tutorial}}

\value{a \code{list} with the following elements:

\item{summary}{FFD summary statistics as a \code{data.frame}}
\item{fm}{frost matrix}
\item{Pr.frost}{Pr(frost|day): daily probability of frost}
}


\author{D.E. Beaudette}

\note{This is a work in progress.}
