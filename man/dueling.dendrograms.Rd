\name{dueling.dendrograms}
\alias{dueling.dendrograms}
\title{Dueling Dendrograms}
\description{Graphically compare two related dendrograms}
\usage{dueling.dendrograms(d.1, d.2, lab.1 = "D1", lab.2 = "D2")}

\arguments{
  \item{d.1}{left-hand dissimilarity matrix, rescaled to [0,1]}
  \item{d.2}{right-hand dissimilarity matrix, rescaled to [0,1]}
  \item{lab.1}{left-hand title}
  \item{lab.2}{right-hand title}
}

\author{D.E. Beaudette}

\examples{
library(aqp)
library(cluster)
library(ape)

# load sample dataset from aqp package
data(sp3)

# promote to SoilProfileCollection
depths(sp3) <- id ~ top + bottom

# compute dissimilarity using different sets of variables
d.1 <- profile_compare(sp3, vars=c('clay', 'cec'), k=0, max_d=100, rescale.result=TRUE)
d.2 <- profile_compare(sp3, vars=c('clay', 'L'), k=0, max_d=100, rescale.result=TRUE)

# graphically compare
dueling.dendrograms(d.1, d.2)
}

\keyword{hplots}

