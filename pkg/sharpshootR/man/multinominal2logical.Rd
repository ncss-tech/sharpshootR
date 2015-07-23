\name{multinominal2logical}
\alias{multinominal2logical}
\title{Convert multinominal to logical matrix}
\description{Convert a single multinominal, site-level attribute from a SoilProfileCollection into a matrix of corresponding logical values. The result contains IDs from the SoilProfileCollection and can easily be joined to the original site-level data.}
\usage{
multinominal2logical(x, v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a \code{SoilProfileCollection} object}
  \item{v}{the name of a site-level attribute that is a factor with more than 2 levels}
}

\value{A \code{data.frame} with IDs in the first column, and as many columns of logical vectors as there were levels in \code{v}. See examples.}

\author{D.E. Beaudette}

\seealso{\code{\link{diagnosticPropertyPlot}}}

\examples{
\dontrun{
# sample data, an SPC
data(loafercreek, package='soilDB')

# convert to factor
loafercreek$hillslope_pos <- factor(loafercreek$hillslope_pos, 
levels=c('Toeslope', 'Footslope', 'Backslope', 'Shoulder', 'Summit'))

# convert to logical matrix
hp <- multinominal2logical(loafercreek, 'hillslope_pos')

# join-in to site data
site(loafercreek) <- hp

# variable names
v <- c('lithic.contact', 'paralithic.contact', 
'argillic.horizon', 'Toeslope', 'Footslope', 
'Backslope', 'Shoulder', 'Summit')

# visualize with some other diagnostic features
x <- diagnosticPropertyPlot(loafercreek, v, k=5, 
grid.label='bedrock_kind', dend.label='pedon_id')
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

