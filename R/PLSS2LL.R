
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
    f.2 <- paste0(p.qq, p.q)
    f[i] <- paste0(f.1, f.2)
    
    # handle if sections are protracted and unprotracted blocks
    if(type=='PB') {
    f[i] <- stri_replace_all_fixed(f[i], 'SN', 'PB')
    f[i] <- stri_replace_all_fixed(f[i], 'A', '')
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
    if(type=='UP') {
      f[i] <- stri_replace_all_fixed(f[i], 'SN', 'UP')
      f[i] <- stri_replace_all_fixed(f[i], 'A', '')
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
  }
  
  return(f)
}



## TODO: trap errors
# not vectorized
LL2PLSS <- function(x, y) {
  
  # check for required packages
  if(!requireNamespace('httr', quietly = TRUE) | !requireNamespace('jsonlite', quietly = TRUE))
    stop('please install the `httr` and `jsonlite` packages', call.=FALSE)
  
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
  geom <- SpatialPolygons(list(Polygons(list(Polygon(res$features$geometry.rings[[1]][1,, ])), ID = 1)))
  srid <- res$features$geometry.spatialReference.wkid
  proj4string(geom) <- paste0('+init=epsg:', srid)
  
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
  
  # check for required packages
  if(!requireNamespace('httr', quietly = TRUE) | !requireNamespace('jsonlite', quietly = TRUE))
    stop('please install the `httr` and `jsonlite` packages', call.=FALSE)
  
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
    r <- jsonlite::fromJSON(httr::content(r, as = 'text'), flatten = TRUE)
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
  
  res <- ldply(res)
  #print(res)
  return(res)
}

PLSS2LL_1 <- function(formatted.plss) {
  
  # pre-allocate char vector for results
  res <- list()
  
  for(i in 1:length(formatted.plss)) { 
    # composite URL for GET request, result is JSON
    u <- paste0("https://gis.blm.gov/arcgis/rest/services/Cadastral/BLM_Natl_PLSS_CadNSDI/MapServer/exts/CadastralSpecialServices/GetLatLon?trs=", formatted.plss[i], "&f=pjson")
    
    # process GET request
    r <- httr::GET(u)
    httr::stop_for_status(r)
    
    # convert JSON -> list
    r <- jsonlite::fromJSON(content(r, as = 'text'), flatten = TRUE)
    
    # keep only coordinates
    # r <- r$coordinates[, c('lon', 'lat')]
    r <- r$coordinates
    
    # request that are less than QQ precision will return multiple QQ centers
    # keep the mean coordinates
    if(nrow(r) >= 1) {
      r <- data.frame(t(colMeans(r[ ,2:3], na.rm = TRUE)))
    }
       #if(nrow(r) == 0) {
       #    r <- data.frame(plssid=r$plssid[1], lat='NA', lon='NA')
       #}	
    res[[i]] <- r
  }
  
  res <- plyr::ldply(res)
  return(res)
}
