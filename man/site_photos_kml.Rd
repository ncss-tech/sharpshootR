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

\details{This function simplifies writing a kml file of site and/or sites with linked photos.}
\value{A KML file of of sites with embedded associated site photos.}
\author{Jay Skovlin, D.E. Beaudette}
\note{This function requires the following packages: \code{plyr}.}


\examples{
\donttest{
if(require(soilDB) &
  require(plyr)
) {
  
    if(soilDB:::.local_NASIS_defined()) {
    
    ### Load data #####
# fetch data from NASIS
f <- fetchNASIS()

# trim down to the essentials:
vars <- c("site_id","pedon_id", "siteiid", "describer","obs_date",
"pedonpurpose","taxpartsize","taxsubgrp", 'x_std', 'y_std')
f1 <- site(f)[ , vars]

# get photo links
e <- get_extended_data_from_NASIS_db()$photo

# join photos to site data
d1 <- join(f1, e, by='siteiid')

# write kml file
site_photos_kml(data = d1, filename = 'C:/Temp/site_photos.kml', 
make.image.grid=TRUE, file.source = 'relative') 
    
    }
  
}

}
}

\keyword{manip}
