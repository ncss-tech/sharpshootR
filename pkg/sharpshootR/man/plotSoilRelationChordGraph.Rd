\name{plotSoilRelationChordGraph}
\alias{plotSoilRelationChordGraph}

\title{Vizualize Soil Relationships via Chord Diagram.}
\description{Plot a soil relationship diagram using a chord diagram.}
\usage{
plotSoilRelationChordGraph(m, s, mult = 2, base.color = "grey", 
highlight.colors = c("RoyalBlue", "DarkOrange", "DarkGreen"), 
add.legend = TRUE, ...)
}

\arguments{
  \item{m}{an adjacency matrix, no NA allowed}
  \item{s}{soil of interest, must exist in the column or row names of \code{m}}
  \item{mult}{multiplier used to re-scale data in \code{m} associated with \code{s}}
  \item{base.color}{color for all soils other than \code{s} and 1st and 2nd most commonly co-occuring}
  \item{highlight.colors}{vector of 3 colors: soil of interest, 1st most common, 2nd most common}
  \item{add.legend}{logical, add a legend}
  \item{\dots}{additional arguments passed to \code{circlize::chordDiagramFromMatrix}}
}
\details{This function is experimental. Documentation pending. See \url{http://jokergoo.github.io/circlize/} for ideas.}

\references{\url{https://github.com/jokergoo/circlize}}
\author{D.E. Beaudette}


\seealso{\code{\link{plotSoilRelationGraph}}}
\examples{
\dontrun{
data(amador)
m <- component.adj.matrix(amador)
plotSoilRelationChordGraph(m, 'amador')
}
}

\keyword{hplots}

