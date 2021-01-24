#' @title formatPLSS
#' @description Format PLSS information into a coded format that can be digested by PLSS web service.
#'
#' @param p data.frame with chunks of PLSS coordinates
#' @param type an option to format protracted blocks 'PB', unprotracted blocks 'UP', or standard section number 'SN' (default).
#' @details This function is typically accessed as a helper function to prepare data for use within \code{\link{PLSS2LL}} function.
#'
#' @note This function expects that the \code{Polygon} object has coordinates associated with a projected CRS-- e.g. units of meters.
#' @author D.E. Beaudette, Jay Skovlin
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
# p: data.frame with chunks of PLSS coordinates
formatPLSS <- function(p, type='SN') {

  # check for required packages
  if(!requireNamespace('stringi', quietly = TRUE))
    stop('please install the `stringi` package', call.=FALSE)

  # specify columns
  required_chr <- c("id", "qq", "q", "t", "r", "type", "m")
  required_int <- c("s")

  if (!inherits(p, 'data.frame') && !all(c(required_chr, required_int) %in% colnames(p)))
    stop('p must be a data.frame containing columns: id, qq, q, s, t, r, type, m')

  # handle subclasses of data.frame (e.g. tibble, data.table)
  p <- as.data.frame(p)

  # force conversions
  p[required_chr] <- lapply(p[,required_chr, drop = FALSE], as.character)
  p[required_int] <- lapply(p[,required_int, drop = FALSE], as.integer)

  # pre-allocate char vector for results
  f <- vector(mode = 'character', length = nrow(p))

  # identify those that can produce valid PLSS string
  p.good <- which(complete.cases(p))

  for(i in 1:nrow(p)) {

    # skip incomplete (NA-containing) rows
    if(i %in% p.good) {

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
      p.s <- ifelse(p$s[i] > 9, paste0('SN', p$s[i]), paste0('SN', 0, p$s[i]))

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
        f[i] <- stri_replace_all_fixed(f[i], 'A', 'U')
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
    } else {
      f[i] <- NA
    }
  }

  return(f)
}

#' @title LL2PLSS
#' @aliases plssMeridians
#' @description Uses lattitude and longitude coordinates to return the PLSS section geometry from the BLM PLSS web service.
#' @param x longitude coordinates
#' @param y lattitude coordinates
#' @param returnlevel 'S' for "Section" or 'I' for "Intersection" (subsections)
#' @details This function takes xy coordinates and returns the PLSS section geometry to the quarter-quarter section. \code{returnlevel} options are defaulted to 'I' which returns smallest intersected sectional aliquot geometry, 'S' will return the section geometry of the coordinates. See https://gis.blm.gov/arcgis/rest/services/Cadastral/BLM_Natl_PLSS_CadNSDI/MapServer for details.
#' @return \code{list} of of PLSS codes and coordinates.
#' @author D.E. Beaudette, Jay Skovlin
#' @seealso \code{\link{PLSS2LL}}, \code{\link{formatPLSS}}
#' @note This function requires the following packages: \code{httr}, \code{jsonlite}, and \code{sp}.
#' @export
#'
#' @examples
#'
#' if(requireNamespace("curl") &
#'
#'    curl::has_internet() &
#'     require(sp)) {
#'
#'     # create coordinates
#'     x <- -115.3823
#'     y <- 48.88228
#'
#'     # fetch PLSS geometry for these coordinates
#'     p.plss <- LL2PLSS(x, y)
#'
#'     # plot geometry
#'     plot(p.plss$geom)
#' }
LL2PLSS <- function(x, y, returnlevel= 'I') {

  if (length(x) > 1 && length(y) > 1 && length(x) == length(y)) {
    # vectorization
    itrres <- lapply(seq_along(x), function(i) .LL2PLSS(x[i], y[i], returnlevel = returnlevel, .polyID = i))
    out <- list()
    out$geom <- do.call('rbind', lapply(itrres, function(x) x$geom))
    out$plss <- do.call('c', lapply(itrres, function(x) x$plss))
    return(out)
  }
  .LL2PLSS(x, y, returnlevel)
}

.LL2PLSS <- function(x, y, returnlevel = "I", .polyID = 1) {
  # check for required packages
  if(!requireNamespace('httr', quietly = TRUE) | !requireNamespace('jsonlite', quietly = TRUE))
    stop('please install the `httr` and `jsonlite` packages', call.=FALSE)

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

  # check for invalid result (e.g. reversed coordinates)
  if(is.null(res$features$geometry.rings))
    stop("invalid geometry specification, check coordinate XY order (longitude: X, latitude: Y)")

  # attempt to extract PLSS geometry
  geom <- SpatialPolygons(list(Polygons(list(Polygon(res$features$geometry.rings[[1]][1,, ])), ID = .polyID)))
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

# This function retrieves one coordinate. To be used by LSS2LL wrapper function.
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

    # request that are less than QQ precision will return multiple QQ centers
    # keep the mean coordinates - get to one set of lat/lon coords
    if (nrow(r) >= 0) {
      r <-
        data.frame(id = p['id'], plssid = formatted.plss, t(colMeans(r[, 2:3], na.rm = TRUE)))
    }
    res <- r
  }
  row.names(res) <- NULL
  #return(as.vector(res))
  return(res)
}


#' @title PLSS2LL
#' @description Fetch latitude and longitude centroid coordinates for coded PLSS information from the BLM PLSS web service.
#' @param p data.frame with chunks of PLSS coordinates
#' @param plssid Column name containing PLSS ID (default: \code{"plssid"})
#'
#' @return A \code{data.frame} of PLSS codes and coordinates.
#' @author D.E. Beaudette, Jay Skovlin
#' @note This function expects that the dataframe will have a 'plssid' column generated by the \code{formatPLSS} function. Requires the following packages: \code{httr}, and \code{jsonlite}.
#' @seealso \code{\link{LL2PLSS}}, \code{\link{formatPLSS}}
#' @export
#'
#' @examples
#' if(requireNamespace("curl") &
#'    curl::has_internet()) {
#'
#'   # create some data
#'   d <- data.frame(
#'     id = 1:3,
#'     qq = c('SW', 'SW', 'SE'),
#'     q = c('NE', 'NW', 'SE'),
#'     s = c(17, 32, 30),
#'     t = c('T36N', 'T35N', 'T35N'),
#'     r = c('R29W', 'R28W', 'R28W'),
#'     type = 'SN',
#'     m = 'MT20',
#'     stringsAsFactors = FALSE
#'   )
#'
#'   # generate formatted PLSS codes
#'   d$plssid <- formatPLSS(d)
#'
#'   # fetch lat/long coordinates
#'   PLSS2LL(d)
#' }
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

  if(!nrow(p) > 0) {
    stop('p must have more than 0 rows')
  }

  # check that p has a plssid column
  if (!(plssid %in% colnames(p))) {
    stop(sprintf("Column %s not found in `p`. Consider using the `formatPLSS` function to generate `p`.", plssid))
  }

  # apply over data frame
  res <-  do.call("rbind", apply(p, 1, .PLSS2LL))
  return(res)
}

.PLSS2LL_1 <- function(formatted.plss) {

  # check for required packages
  if(!requireNamespace('httr', quietly = TRUE) | !requireNamespace('jsonlite', quietly = TRUE))
    stop('please install the `httr` and `jsonlite` packages', call.=FALSE)

  # pre-allocate char vector for results
  res <- list()

  for(i in 1:length(formatted.plss)) {
    # composite URL for GET request, result is JSON
    u <- paste0("https://gis.blm.gov/arcgis/rest/services/Cadastral/BLM_Natl_PLSS_CadNSDI/MapServer/exts/CadastralSpecialServices/GetLatLon?trs=", formatted.plss[i], "&f=pjson")

    # process GET request
    r <- httr::GET(u)
    httr::stop_for_status(r)

    # convert JSON -> list
    r <- jsonlite::fromJSON(httr::content(r, as = 'text'), flatten = TRUE)

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
