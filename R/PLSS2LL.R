#' @title formatPLSS
#' @description Format PLSS information into a coded format that can be digested by PLSS web service.
#'
#' @param p data.frame with chunks of PLSS coordinates
#' @param type an option to format protracted blocks 'PB', unprotracted blocks 'UP', or standard section number 'SN' (default).
#' @details This function is typically accessed as a helper function to prepare data for use within \code{\link{PLSS2LL}} function.
#'
#' @note This function expects that the \code{Polygon} object has coordinates associated with a projected CRS-- e.g. units of meters.
#' @author D.E. Beaudette, Jay Skovlin, A.G. Brown
#' @seealso \code{\link{PLSS2LL}}
#' @note This function requires the following packages: \code{stringi}.
#' @return A vector of PLSS codes.
#' @export
#'
#' @examples
#' # create some data
#' d <- data.frame(
#'   id = 1:3,
#'   qq = c('SW', 'SW', 'SE'),
#'   q = c('NE', 'NW', 'SE'),
#'   s = c(17, 32, 30),
#'   t = c('T36N', 'T35N', 'T35N'),
#'   r = c('R29W', 'R28W', 'R28W'),
#'   type = 'SN',
#'   m = 'MT20',
#'   stringsAsFactors = FALSE
#' )
#' # add column names
#'
#' names(d) <- c('id', 'qq', 'q', 's', 't', 'r', 'type', 'm')
#' # generate formatted PLSS codes
#' formatPLSS(d, type='SN')
#' 
formatPLSS <- function(p, type = 'SN') {
  # check for required packages
  if(!requireNamespace('stringi', quietly = TRUE))
    stop('please install the `stringi` package', call.=FALSE)

  # specify columns
  required_chr <- c("id", "t", "r", "type", "m")
  optional_chr <- c("qq", "q")
  optional_int <- c("s")

  if (!inherits(p, 'data.frame') || !all(required_chr %in% colnames(p)))
    stop('p must be a data.frame containing columns: id, t, r, type, m; and optionally: s, qq, q')

  if (any(optional_chr %in% colnames(p))) {
    if (!"q" %in% optional_chr)
      stop('p must contain q (quarter section) if it contains qq (quarter-quarter section)')
  }

  # handle subclasses of data.frame (e.g. tibble, data.table)
  p <- as.data.frame(p)

  # force conversions to appropriate data type
  p[,required_chr] <- lapply(p[,required_chr, drop = FALSE], as.character)

  if (sum(optional_chr %in% colnames(p)) > 0) {
    p[,optional_chr] <- lapply(p[,optional_chr, drop = FALSE], as.character)
  }

  if (optional_int %in% colnames(p)) {
    p[,optional_int] <- lapply(p[,optional_int, drop = FALSE], as.integer)
  }

  # pre-allocate char vector for results
  f <- vector(mode = 'character', length = nrow(p))

  # required names and optional section, quarter and quarter-quarter section
  required_names <- required_chr
  optional <- c(optional_int, optional_chr)
  optional_names <- optional[optional %in% colnames(p)]

  # identify those that can produce valid PLSS string (assuming need everything down to section number)
  p.good <- complete.cases(p[,required_names])
  p.bad.idx <- which(!p.good)

  # calculate expected number of characters
  p.expected.nchar <- .expectedPLSSnchar(p, type = type, optional_names = optional_names)

  # expected length is internal, not really necessary to create below warning
  # if (length(p.bad.idx) > 0)
  #   p.expected.nchar[p.bad.idx] <- NA

  for (i in 1:nrow(p)) {
    # skip incomplete (NA-containing) rows
    if (!i %in% p.bad.idx) {
      # split Township / Range into elements, case sensitive
      p.t <- stri_match_first_regex(p$t[i], pattern = '([0-9]+)([N|S])')[2:3]
      p.r <- stri_match_first_regex(p$r[i], pattern = '([0-9]+)([E|W])')[2:3]

      # pad T/R codes with 0
      p.t[1] <- stri_pad(p.t[1], width = 2, pad = '0')
      p.r[1] <- stri_pad(p.r[1], width = 2, pad = '0')

      # add extra '0' between T/R code and direction
      p.t <- paste0(p.t, collapse = '0')
      p.r <- paste0(p.r, collapse = '0')

      # add 'SN to section number and pad single digit section numbers
      p.s <- ifelse(is.na(p$s[i]), '', ifelse(p$s[i] > 9, paste0('SN', p$s[i]), paste0('SN', 0, p$s[i])))

      # replace NA -> '' IN S, Q and QQ sections
      p.q <- ifelse(is.na(p$q[i]), '', p$q[i])
      p.qq <- ifelse(is.na(p$qq[i]), '', p$qq[i])

      # format the first chunk
      f.1 <- ifelse(nchar(p.s) == 0, 
                    paste0(paste0(c(p$m[i], p.t, p.r), collapse="0"), "0"), # no section
                    paste0(c(p$m[i], p.t, p.r, p.s, 'A'), collapse = '0'))       # with section
      
      # format the (optional) Q and QQ sections
      f.2 <- paste0(p.qq, p.q)
      f[i] <- paste0(f.1, f.2)

      # handle if sections are protracted and unprotracted blocks
      if (type == 'PB') {
        f[i] <- stringi::stri_replace_all_fixed(f[i], 'SN', 'PB')
        f[i] <- stringi::stri_replace_all_fixed(f[i], 'A', '')
        # truncate UP and PB cases to section
        if (!is.na(p$qq[i])) {
          f[i] <- stringi::stri_sub(f[i], 0, stringi::stri_length(f[i]) - 4)
        }
        if (is.na(p$qq[i]) & !is.na(p$q[i])) {
          f[i] <- stringi::stri_sub(f[i], 1, stringi::stri_length(f[i]) - 2)
        }
        if (is.na(p$qq[i]) & is.na(p$q[i])) {
          f[i] <- f[i]
        }
      }
      if (type == 'UP') {
        f[i] <- stringi::stri_replace_all_fixed(f[i], 'SN', 'UP')
        f[i] <- stringi::stri_replace_all_fixed(f[i], 'A', 'U')
        # truncate UP and PB cases to section
        if (!is.na(p$qq[i])) {
          f[i] <- stringi::stri_sub(f[i], 0, stringi::stri_length(f[i]) - 4)
        }
        if (is.na(p$qq[i]) & !is.na(p$q[i])) {
          f[i] <- stringi::stri_sub(f[i], 1, stringi::stri_length(f[i]) - 2)
        }
        if (is.na(p$qq[i]) & is.na(p$q[i])) {
          f[i] <- f[i]
        }
      }
    } else {
      f[i] <- NA
    }
  }
  if (any(sapply(f[p.good], nchar) != p.expected.nchar[p.good]))
    warning("one or more formatted PLSS strings does not match expected length")
  if (any(is.na(p.expected.nchar)))
    warning("one or more expected lengths is NA") # generally shouldnt happen?
  if (any(is.na(f)))
    warning("one or more results is NA; check with `attr(,'na.action')`") # this most common user-level errors
  return(f)
}

.expectedPLSSnchar <- function(p, type = 'SN', optional_names = c("s","qq","q")) {
  # calculate expected number of characters
  .hasZeroLen <- function(x) return(is.na(x) | x == "")
  p.expected.nchar <- 25 - ((rowSums(do.call('cbind', lapply(p[, optional_names, drop = FALSE], .hasZeroLen)[optional_names])))*2)
  # note that while it is possible to specify only one of q/qq -- 
  # it does not appear that the API accepts, so we warn accordingly in formatPLSS expected length wont be right

  naopt <- which(apply(is.na(p[,optional_names]) | p$type == "PB", 1, function(b) any(b)))
  if (type == 'PB') { # type=PB -5
    p.expected.nchar <- p.expected.nchar - 5
  } else {
    if(any(is.na(p[,'s']))) { # section missing -4
      p.expected.nchar[is.na(p[,'s'])] <- p.expected.nchar[is.na(p[,'s'])] - 4
    }
  }
  return(p.expected.nchar)
}

#' @title LL2PLSS
#' @aliases plssMeridians
#' @description Uses lattitude and longitude coordinates to return the PLSS section geometry from the BLM PLSS web service.
#' @param x longitude coordinates
#' @param y lattitude coordinates
#' @param returnlevel 'S' for "Section" or 'I' for "Intersection" (subsections)
#' @details This function takes xy coordinates and returns the PLSS section geometry to the quarter-quarter section. \code{returnlevel} options are defaulted to 'I' which returns smallest intersected sectional aliquot geometry, 'S' will return the section geometry of the coordinates. See https://gis.blm.gov/arcgis/rest/services/Cadastral/BLM_Natl_PLSS_CadNSDI/MapServer for details.
#' @return \code{list} of of PLSS codes and coordinates.
#' @author D.E. Beaudette, Jay Skovlin, A.G. Brown
#' @seealso \code{\link{PLSS2LL}}, \code{\link{formatPLSS}}
#' @note This function requires the following packages: \code{httr}, \code{jsonlite}, and \code{sp}.
#' @export
#'
#' 
LL2PLSS <- function(x, y, returnlevel= 'I') {

  if (length(x) > 1 && length(y) > 1 && length(x) == length(y)) {
    # vectorization
    itrres <- lapply(seq_along(x), function(i) .LL2PLSS(x[i], y[i], returnlevel = returnlevel, .polyID = i))
    out <- list()
    out$plss <- rep(NA, length(x))
    geo <- lapply(itrres, function(x) x$geom)
    geoNA <- !sapply(geo, inherits, what = 'Spatial')
    if (sum(geoNA) > 0)
      warning("Dropping NA coordinates from SpatialPolygons result!")
    out$geom <- do.call('rbind', geo[which(!geoNA)])
    out$plss <- na.omit(sapply(itrres, function(x) x$plss))
              # na.omit() gives parity with Spatial result
              #   attr(foo$plss,"na.action") to get NA's
    return(out)
  }

  .LL2PLSS(x, y, returnlevel)
}

.LL2PLSS <- function(x, y, returnlevel = "I", .polyID = 1) {
  # use LL2PLSS() for vectorization
  stopifnot(length(x) == 1)
  stopifnot(length(x) == length(y))

  # check for required packages
  if(!requireNamespace('httr', quietly = TRUE) | !requireNamespace('jsonlite', quietly = TRUE))
    stop('please install the `httr` and `jsonlite` packages', call.=FALSE)

  if(!is.na(x) && !is.na(y)) {

    # ensure that x/y have at least 8 decimal places
    x <- sprintf("%.08f", as.numeric(x))
    y <- sprintf("%.08f", as.numeric(y))

    # composite URL for GET request, result is JSON
    if(returnlevel == 'S') {
      u <- paste0("https://gis.blm.gov/arcgis/rest/services/Cadastral/BLM_Natl_PLSS_CadNSDI/MapServer/exts/CadastralSpecialServices/GetTRS?lat=",
                  y, "&lon=", x, "&units=DD&returnlevel=S&f=pjson")
    }
    if (returnlevel == 'I') {
      u <- paste0("https://gis.blm.gov/arcgis/rest/services/Cadastral/BLM_Natl_PLSS_CadNSDI/MapServer/exts/CadastralSpecialServices/GetTRS?lat=",
                  y, "&lon=", x, "&units=DD&returnlevel=I&f=pjson")
    }

    # process GET request
    res <- httr::GET(u)
    httr::stop_for_status(res)

    # convert JSON -> list
    res <- jsonlite::fromJSON(httr::content(res, as = 'text'), flatten = TRUE)
    
    ## TODO: check for a valid result from API
    
    # check for invalid result:
    # * reversed coordinates in x,y
    # * nothing returned by the API
    if(is.null(res$features$geometry.rings)) {
      message("invalid geometry specification, check coordinate XY order (longitude: X, latitude: Y)")
      # return "NULL" result
      return(list(geom=SpatialPolygons(list()), plss=NULL))
    }
    
  # attempt to extract PLSS geometry
    geom <- SpatialPolygons(list(Polygons(list(Polygon(res$features$geometry.rings[[1]][1,, ])), ID = .polyID)))
    srid <- res$features$geometry.spatialReference.wkid
    proj4string(geom) <- paste0('+init=epsg:', srid)

    # attempt to extract PLSS coordinates
    plss.coords <- res$features$attributes.landdescription
  } else {
    # return NA for NA X/Y input
    geom <- NA
    plss.coords <- NA
  }

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

# This function retrieves one coordinate. To be used by PLSS2LL wrapper function.
# consider not exporting

.PLSS2LL <- function(p) {
  
  # p in a vectorized function is passed as named vector
  if (is.na(p['plssid'])) {
    return(NA)
  }
  
  formatted.plss <- p['plssid']


   # composite URL for GET request, result is JSON
  u <-
    paste0(
      'https://gis.blm.gov/arcgis/rest/services/Cadastral/',
      'BLM_Natl_PLSS_CadNSDI/MapServer/exts/CadastralSpecialServices/',
      'GetLatLon?trs=',
      formatted.plss,
      '&f=pjson'
    )

  # process GET request
  r <- httr::GET(u)
  httr::stop_for_status(r)

  # convert JSON -> list
  r <- jsonlite::fromJSON(httr::content(r, as = 'text'), flatten = TRUE)

  # handling for if no coords returned
  if (inherits(r$coordinates, 'list') &
      length(r$coordinates) == 0) {
    r <- data.frame(id = p['id'], plssid = formatted.plss, lat = NA, lon = NA)
    res <- r
  } else {
    # keep only coordinates
    r <- r$coordinates

    if (is.null(r))
      return(NULL)
    
    # request that are less than QQ precision will return multiple QQ centers
    # keep the mean coordinates - get to one set of lat/lon coords
    if (nrow(r) >= 0) {
      r <- data.frame(
        id = p['id'], 
        plssid = formatted.plss, 
        t(colMeans(r[, 2:3], na.rm = TRUE))
      )
    }
    res <- r
  }
  # reset rownames
  row.names(res) <- NULL
  return(res)
}


#' @title PLSS2LL
#' @description Fetch latitude and longitude centroid coordinates for coded PLSS information from the BLM PLSS web service.
#' @param p data.frame with chunks of PLSS coordinates
#' @param plssid Column name containing PLSS ID (default: \code{"plssid"})
#'
#' @return A \code{data.frame} of PLSS codes and coordinates.
#' @author D.E. Beaudette, Jay Skovlin, A.G. Brown
#' @note This function expects that the dataframe will have a 'plssid' column generated by the \code{formatPLSS} function. Requires the following packages: \code{httr}, and \code{jsonlite}.
#' @seealso \code{\link{LL2PLSS}}, \code{\link{formatPLSS}}
#' @export
#'
#' 
PLSS2LL <- function(p, plssid = "plssid") {
  # check for required packages
  if(!requireNamespace('httr', quietly = TRUE) | !requireNamespace('jsonlite', quietly = TRUE))
    stop('please install the `httr` and `jsonlite` packages', call.=FALSE)

  # check that p is a data frame
  if (!inherits(p, 'data.frame')) {
    stop('p must be a data frame')
  } else {
    # add data.table or other support (by casting all data.frame subclasses to data.frame)
    p <- as.data.frame(p)
  }

  if (nrow(p) == 0) {
    stop('p must have more than 0 rows')
  }

  # check that p has a plssid column
  if (!(plssid %in% colnames(p))) {
    stop(sprintf("Column %s not found in `p`. Consider using the `formatPLSS` function to generate `p`.", plssid))
  }

  # apply over data frame
  pres <- apply(p, 1, .PLSS2LL)
  
  if (length(pres) == 0)
    return(NULL)
  
  res <-  do.call("rbind", pres)
  return(res)
}

