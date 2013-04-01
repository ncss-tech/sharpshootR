\name{dueling.dendrograms}
\alias{dueling.dendrograms}
\title{Dueling Dendrograms}
\description{Graphically compare two related dendrograms}
\usage{dueling.dendrograms(d.1, d.2, lab.1 = "D1", lab.2 = "D2", cex.nodelabels=0.75, arrow.length=0.05)}

\arguments{
  \item{d.1}{left-hand dissimilarity matrix, re-scaled to [0,1]}
  \item{d.2}{right-hand dissimilarity matrix, re-scaled to [0,1]}
  \item{lab.1}{left-hand title}
  \item{lab.2}{right-hand title}
  \item{cex.nodelabels}{character expansion size for node labels}
  \item{arrow.length}{arrow head size}
}

\author{D. E. Beaudette}

\details{Dissimilarity matrices are converted into dendrograms via divisive hierarchical clustering via \code{diana()}. Connector arrows are used to link nodes from the left-hand dendrogram to the right-hand dendrogram.

}

\examples{
library(aqp)
library(cluster)
library(ape)

# load sample dataset from aqp package
data(sp3)

# promote to SoilProfileCollection
depths(sp3) <- id ~ top + bottom

# compute dissimilarity using different sets of variables
# note that these are rescaled to the interval [0,1]
d.1 <- profile_compare(sp3, vars=c('clay', 'cec'), k=0, max_d=100, rescale.result=TRUE)
d.2 <- profile_compare(sp3, vars=c('clay', 'L'), k=0, max_d=100, rescale.result=TRUE)

# graphically compare
dueling.dendrograms(d.1, d.2, lab.1='clay and CEC', lab.2='clay and L')
}

\keyword{hplots}

