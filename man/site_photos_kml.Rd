\name{site_photos_kml}
\alias{site_photos_kml}

\title{site_photos_kml}
\description{Generates a KML file of site locations with associated site photos and a link to a pedon description report.}

\usage{site_photos_kml(data, 
filename='photos.kml', make.image.grid=FALSE, 
file.source = c('local', 'relative')
)
}

\arguments{
  \item{data}{a dataframe}
  \item{filename}{full file path and name with .kml extension}
  \item{make.image.grid}{logical, include linked site images, default is FALSE}
  \item{file.source}{'local' sources the image files to a specific system path, 'relative' sources the image files to files folder that can be included and referenced within a .kmz file}			
}

\details{This function simplifies writing a kml file of site and/or sites with linked photos. Further documentation is provided in \href{https://ncss-tech.github.io/AQP/sharpshootR/site-photo-kml.html}{this tutorial}.}

\value{A KML file of of sites with embedded associated site photos.}
\author{Jay Skovlin, D.E. Beaudette}


\keyword{manip}
