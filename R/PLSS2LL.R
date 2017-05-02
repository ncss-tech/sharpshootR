library(httr)
library(jsonlite)
library(stringi)

# data for meridians - not sure how to use this
# add support function to search for meridians?
plssMeridians <- read.csv(textConnection("state, meridian, meridian_name
AL,16,HUNTSVILLE_MER
AL,25,ST_STEPHENS
AL,29,TALLAHASEE
AR,05,5TH_PM
AZ,14,GILA-SALT_RIVER
AZ,22,NAVAJO
AZ,27,SAN_BERNARDINO
CA,14,GILA-SALT_RIVER
CA,15,HUMBOLDT
CA,21,MOUNT_DIABLO
CA,27,SAN_BERNARDINO
CO,06,6TH_PM
CO,23,NEW_MEX_PM
CO,31,UTE
CT,CT,CONNECTICUT
DE,DE,DELAWARE
FL,29,TALLAHASEE
GA,GA,GEORGIA
IA,05,5TH_PM
ID,08,BOISE
IL,02,2ND_PM
IL,03,3RD_PM
IL,04,4TH_PM_ILLINOIS
IN,01,1ST_PM
IN,02,2ND_PM
KS,06,6TH_PM
KY,KY,KENTUCKY
LA,18,LOUISIANA
LA,24,ST_HELENA
MA,MA,MASSACHUSETS
MD,MD,MARYLAND
ME,ME,MAINE
MI,19,MICHIGAN
MN,05,5TH_PM
MN,46,4TH_PM_MN-WI
MO,05,5TH_PM
MS,09,CHICKASAW
MS,10,CHOCTAW
MS,16,HUNTSVILLE
MS,25,ST. STEPHENS
MS,32,WASHINGTON
MT,20,MONTANA
NC,NC,NO_CAROLINA
ND,05,5TH_PM
NE,06,6TH_PM
NH,NH,NEW_HAMPSHIRE
NJ,NJ,NEW_JERSEY
NM,23,NEW_MEX_PM
NV,21,MOUNT_DIABLO
NV,27,SAN_BERNARDINO
NY,NY,NEW YORK
OH,01,1ST_PM
OH,19,MICHIGAN
OH,35,OHIO_RIVER_SURVEY
OH,36,BETWEEN_THE_MIAMIS
OH,37,MUSKINGUM_RIVER_BASIN
OH,38,OHIO_RIVER_BASIN
OH,39,1ST_SCIOTO_RIVER_BASE
OH,40,2ND_SCIOTO_RIVER_BASE
OH,41,3RD_SCIOTO_RIVER_BASE
OH,43,TWELVE_MILE_SQUARE
OH,47,WEST_OF_GREAT_MIAMI
OH,48,US_MILITARY_SURVEY
OH,91,CT_WEST_RES-OHIO
OH,92,OHIO_CO_PUR-OHIO
OH,93,VA_MILITARY_SURVEY-OHIO
OH,OH,OHIO
OK,11,CIMARRON
OK,17,INDIAN
OR,33,WILLAMETTE
PA,PA,PENNSYLVANIA
RI,RI,RHODE_ISLAND
SC,SC,SO_CAROLINA
SD,05,5TH_PM
SD,06,6TH_PM
SD,07,BLACK_HILLS
TN,TN,TENNESSEE
TX,TX,TEXAS
UT,26,SALT_LAKE
UT,30,UINTAH_SPEC
VA,VA,VIRGINIA
VT,VT,VERMONT
WA,33,WILLAMETTE
WI,46,4TH_PM_MN-WI
WV,WV,WEST_VIRGINIA
WY,06,6TH_PM
WY,34,WIND_RIVER"), stringsAsFactors = FALSE)


# p: data.frame with chunks of PLSS coordinates
formatPLSS <- function(p, type='SN') {
  
  # pre-allocate char vector for results
  f <- vector(mode='character', length = nrow(p))
  for(i in 1:nrow(p)) {
    # split Township / Range into elements, case sensitive
    p.t <- stri_match_first_regex(p$t[i], pattern='([0-9]+)([N|S])')[2:3]
    p.r <- stri_match_first_regex(p$r[i], pattern='([0-9]+)([E|W])')[2:3]
    
    # pad T/R codes with 0
    p.t[1] <- stri_pad(p.t[1], width=2, pad = '0')
    p.r[1] <- stri_pad(p.r[1], width=2, pad = '0')
    
    # add extra '0' between T/R code and direction
    p.t <- paste0(p.t, collapse = '0')
    p.r <- paste0(p.r, collapse = '0')
    
    # add 'SN to section number and pad single digit section numbers
    p.s <- ifelse((p$s[i] > 9), paste0('SN', p$s[i]), paste0('SN', 0, p$s[i]))
    #p.s <- paste0('SN', p$s[i])
    
    # replace NA -> '' IN Q and QQ sections
    p.q <- ifelse(is.na(p$q[i]), '', p$q[i])
    p.qq <- ifelse(is.na(p$qq[i]), '', p$qq[i])
    
    # format the first chunk
    f.1 <- paste0(c(p$m[i], p.t, p.r, p.s, 'A'), collapse = '0')
    # format the (optional) Q and QQ sections
    f.2 <- paste0(p.q, p.qq)
    f[i] <- paste0(f.1, f.2)
    
    # handle if sections are protracted and unprotracted blocks
    if(type=='PB') {
    f[i] <- stri_replace_all_fixed(f[i], 'SN', 'PB')
    f[i] <- stri_replace_all_fixed(f[i], 'A', 'U')
    }
    if(type=='UP') {
      f[i] <- stri_replace_all_fixed(f[i], 'SN', 'UP')
      f[i] <- stri_replace_all_fixed(f[i], 'A', 'U')
    }
      # truncate UP and PB cases to section
      if(!is.na(p$qq[i])) {
      f[i] <- stri_sub(f[i], 0, stri_length(f[i])-4)
      }
      if(is.na(p$qq[i]) & !is.na(p$q[i])) {
      f[i] <- stri_sub(f[i], 1, stri_length(f[i])-2)
      }
      if(is.na(p$qq[i]) & is.na(p$q[i])) {
      f[i] <- f[i]
      }
  }
  
  return(f)
}



## TODO: trap errors
# not vectorized
LL2PLSS <- function(x, y) {
  
  # ensure that x/y have at least 8 decimal places
  x <- sprintf("%.08f", as.numeric(x))
  y <- sprintf("%.08f", as.numeric(y))
  
  # composite URL for GET request, result is JSON
  u <- paste0("https://gis.blm.gov/arcgis/rest/services/Cadastral/BLM_Natl_PLSS_CadNSDI/MapServer/exts/CadastralSpecialServices/GetTRS?lat=", y, "&lon=", x, "&units=DD&f=pjson")
  
  # process GET request
  res <- httr::GET(u)
  httr::stop_for_status(res)
  
  # convert JSON -> list
  res <- jsonlite::fromJSON(httr::content(res, as = 'text'), flatten = TRUE)
  
  # attempt to extract PLSS geometry
  geom <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(res$features$geometry.rings[[1]][1,, ])), ID = 1)))
  srid <- res$features$geometry.spatialReference.wkid
  sp::proj4string(geom) <- paste0('+init=epsg:', srid)
  
  # attempt to extract PLSS coordinates
  plss.coords <- res$features$attributes.landdescription
  
  # consider returning both geom + PLSS code
  return(list(geom=geom, plss=plss.coords))
}

## TODO: what should be returned @ reduced precision?
## TODO: trap errors associated with bogus PLSS coords
# vectorized
# p: data.frame with chunks of PLSS coordinates
# formatted.plss: character vector of pre-formatted PLSS codes
# returns: data.frame, 1 row / formatted.plss
## references
# http://nationalcad.org/download/PLSS_CadNSDI_Standard_Domains_of_Values.pdf
# http://nationalcad.org/download/PublicationHandbookOct2016.pdf
# http://nationalcad.blogspot.com/2015/03/plss-cadnsdi-plss-first-division.html

PLSS2LL <- function(p) {
  
  # pre-allocate char vector for results
  res <- list()
  
  # create vector of formatted plss codes
  formatted.plss <- paste(p$plssid, sep=" ")
  
  for(i in 1:length(formatted.plss)) { 
    # composite URL for GET request, result is JSON
    u <- paste0("https://gis.blm.gov/arcgis/rest/services/Cadastral/BLM_Natl_PLSS_CadNSDI/MapServer/exts/CadastralSpecialServices/GetLatLon?trs=", formatted.plss[i], "&f=pjson")
    
    # process GET request
    r <- httr::GET(u)
    httr::stop_for_status(r)
    
    # convert JSON -> list
    r <- jsonlite::fromJSON(content(r, as = 'text'), flatten = TRUE)
    #print(r$coordinates)
    
    # handling for if no coords returned
      if(class(r$coordinates) == 'list' & length(r$coordinates) == 0) {
      r <- data.frame(id=p$id[i], plssid=formatted.plss[i]) 
      res[[i]] <- r
      } else {
      # keep only coordinates
      r <- r$coordinates
      # print(formatted.plss[i])
      # print(r$coordinates)
      
      # request that are less than QQ precision will return multiple QQ centers
      # keep the mean coordinates - get to one set of lat/lon coords
      if(nrow(r) >= 0) {
        r <- data.frame(id=p$id[i], plssid=formatted.plss[i], t(colMeans(r[ ,2:3], na.rm = TRUE))) 
      }
      res[[i]] <- r
      
    }
    
  }
  
  res <- plyr::ldply(res)
  print(res)
  return(res)
}
