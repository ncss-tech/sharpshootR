

#' @title Format Public Land Survey System (PLSS) Components
#' @description Format Public Land Survey System (PLSS) components into a string that can be interpreted by the US Bureau of Land Management (BLM) PLSS encoding and decoding web service.
#'
#' @param p `data.frame` with components of a PLSS description, see details.
#' @details This function is typically accessed as a helper function to prepare data for use by the `PLSS2LL()` function. The `data.frame` 'p' must contain:
#'  * id: a unique ID over rows
#'  * t: township number and direction
#'  * r: range number and direction
#'  * m: base meridian code
#'  * type: one of 'SN', 'PB', or 'UN'
#'
#'and can optioninally contain:
#'  * s: section number
#'  * q: quarter section specification
#'  * qq: quarter-quarter section specification
#' 
#'
#' @author D.E. Beaudette, Jay Skovlin, A.G. Brown
#' @seealso [PLSS2LL()]
#' @note This function requires the following packages: stringi.
#' @return A vector of PLSS codes.
#' @export
#'
#' @examples
#' # create PLSS description components
#' d <- data.frame(
#'   id = 1:3,
#'   qq = c('SW', 'SW', 'SE'),
#'   q = c('NE', 'NW', 'SE'),
#'   s = c(17, 32, 30),
#'   t = c('T36N', 'T35N', 'T35N'),
#'   r = c('R29W', 'R28W', 'R28W'),
#'   type = 'SN',
#'   m = 'MT20'
#' )

#' # generate formatted PLSS codes
#' # and save back to original data.frame
#' d$plssid <- formatPLSS(d)
#' 
#' # convert to geographic coordinates
#' # PLSS2LL(d)
#' 
formatPLSS <- function(p) {
  # check for required packages
  if(!requireNamespace('stringi', quietly = TRUE))
    stop('please install the `stringi` package', call.=FALSE)
  
  # specify columns
  required_chr <- c("id", "t", "r", "type", "m")
  optional_chr <- c("qq", "q")
  optional_int <- c("s")
  
  if (!inherits(p, 'data.frame') || !all(required_chr %in% colnames(p)))
    stop('p must be a data.frame containing columns: id, t, r, type, m; and optionally: s, q, qq')
  
  if (any(optional_chr %in% colnames(p))) {
    if (!"q" %in% optional_chr)
      stop('p must contain q (quarter section) if it contains qq (quarter-quarter section)')
  }
  
  # handle subclasses of data.frame (e.g. tibble, data.table)
  p <- as.data.frame(p)
  
  # force conversions to appropriate data type
  p[ ,required_chr] <- lapply(p[ ,required_chr, drop = FALSE], as.character)
  
  if (sum(optional_chr %in% colnames(p)) > 0) {
    p[ ,optional_chr] <- lapply(p[ ,optional_chr, drop = FALSE], as.character)
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
  p.expected.nchar <- .expectedPLSSnchar(p, optional_names = optional_names)
  
  # expected length is internal, not really necessary to create below warning
  # if (length(p.bad.idx) > 0)
  #   p.expected.nchar[p.bad.idx] <- NA
  
  for (i in 1:nrow(p)) {
    # skip incomplete (NA-containing) rows
    if (!i %in% p.bad.idx) {
      # request type
      type <- p$type[i]
      
      # split Township / Range into elements, case sensitive
      p.t <- stri_match_first_regex(p$t[i], pattern = '([0-9]+)([N|S])')[2:3]
      p.r <- stri_match_first_regex(p$r[i], pattern = '([0-9]+)([E|W])')[2:3]
      
      ## TODO: accommodate T and R values > 99
      ##       -> no 0-padding with 3-digit T/R
      ## https://github.com/ncss-tech/sharpshootR/issues/62
      
      # pad T/R codes with 0
      p.t[1] <- stri_pad_left(p.t[1], width = 3, pad = '0')
      p.r[1] <- stri_pad_left(p.r[1], width = 3, pad = '0')
      
      # add extra '0' between T/R code and direction
      p.t <- paste0(p.t, collapse = '0')
      p.r <- paste0(p.r, collapse = '0')
      
      # add 'SN to section number and pad single digit section numbers
      p.s <- ifelse(is.na(p$s[i]), '', ifelse(p$s[i] > 9, p$s[i], paste0("0", p$s[i])))
      
      # replace NA -> '' IN S, Q and QQ sections
      p.q <- ifelse(is.na(p$q[i]), '', p$q[i])
      p.qq <- ifelse(is.na(p$qq[i]), '', p$qq[i])
      
      
      ## TODO: accommodate T and R values > 99
      ##       -> no 0-padding with 3-digit T/R
      ## this line is breaking e.g. T149N
      ## https://github.com/ncss-tech/sharpshootR/issues/62
      # 
      # # format the first chunk
      # f.1 <- ifelse(nchar(p.s) == 0, 
      #               paste0(paste0(c(p$m[i], p.t, p.r), collapse="0"), "0"), # no section
      #               paste0(c(p$m[i], p.t, p.r, paste0("SN", p.s, "0", 'A')), collapse = '0'))       # with section
      
      # format first chunk
      if(nchar(p.s) == 0) {
        # description missing section
        f.1 <- paste0(paste0(c(p$m[i], p.t, p.r), collapse = ""), "0")
      } else {
        # description includes section
        f.1 <- paste0(c(p$m[i], p.t, p.r, '0', paste0("SN", p.s, "0", 'A')), collapse = '')
      }
      
      
      
      # format the (optional) Q and QQ sections
      f.2 <- paste0(p.qq, p.q)
      if (nchar(f.2) != 4) {
        if (!is.na(f.2) && nchar(f.2) != 0)
          message("Dropping quarter and quarter-quarter sections due to bad formatting: ", f.2)
        f.2 <- ""
      }
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
    warning("one or more expected lengths is NA") # generally shouldn't happen?
  if (any(is.na(f)))
    warning("one or more results is NA; check with `attr(,'na.action')`") # this most common user-level errors
  return(f)
}

.expectedPLSSnchar <- function(p, optional_names = c("s", "qq", "q")) {
  # calculate expected number of characters
  .hasZeroLen <- function(x) return(is.na(x) | x == "")
  
  p.expected.nchar <- 25 - ((rowSums(do.call('cbind', lapply(p[, optional_names, drop = FALSE], .hasZeroLen)[optional_names])))*2)
  
  # note that while it is possible to specify only one of q/qq -- 
  # it does not appear that the API accepts, so we warn accordingly in formatPLSS() expected length wont be right
  naopt <- which(apply(is.na(p[ ,optional_names]) | p$type == "PB", 1, function(b) any(b)))
  
  # account for type = PB: -5
  .idx <- which(p$type == 'PB')
  p.expected.nchar[.idx] <- p.expected.nchar[.idx] - 5
  
  # account for missing section: -4
  .idx <- which(any(is.na(p[ ,'s'])))
  p.expected.nchar[.idx] <- p.expected.nchar[.idx] - 4
  
  return(p.expected.nchar)
}

#' @title LL2PLSS
#' @aliases plssMeridians
#' @description Uses latitude and longitude coordinates to return the PLSS section geometry from the BLM PLSS web service.
#' 
#' @param x longitude coordinates (WGS84)
#' @param y latitude coordinates (WGS84)
#' @param returnlevel 'S' for "Section" or 'I' for "Intersection" (subsections)
#' 
#' @details This function accepts geographic coordinates and returns the PLSS fabric geometry to the quarter-quarter section. `returnlevel` defaults to 'I' which returns smallest intersected sectional aliquot geometry, 'S' will return the section geometry of the coordinates. See https://gis.blm.gov/arcgis/rest/services/Cadastral/BLM_Natl_PLSS_CadNSDI/MapServer for details.
#' 
#' @return `sf` object with geometry and PLSS definition.
#' @author D.E. Beaudette, Jay Skovlin, A.G. Brown
#' @seealso [PLSS2LL()], [formatPLSS()]
#' @note This function requires the following packages: httr, jsonlite, and sf.
#' @export
#'
#' 
LL2PLSS <- function(x, y, returnlevel = c('I', 'S')) {
  
  # sanity check
  returnlevel <- match.arg(returnlevel)
  
  # vectorization
  if (length(x) > 1 && length(y) > 1 && length(x) == length(y)) {
    
    .res <- lapply(
      seq_along(x), 
      function(i) 
        .LL2PLSS(x[i], y[i], returnlevel = returnlevel)
    )
    
    # flatten list of sf object -> single sf object / multiple features
    .res <- do.call('rbind', .res)
  } else {
    # single point
    .res <- .LL2PLSS(x, y, returnlevel)  
  }
  
  return(.res)  
}


.LL2PLSS <- function(x, y, returnlevel = "I") {
  
  # empty result for error condition / no results from API
  .empty <- sf::st_sfc(sf::st_polygon(list()))
  .empty <- sf::st_sf(.empty)
  .empty$plss <- NA
  
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
    
    .baseURL <- "https://gis.blm.gov/arcgis/rest/services/Cadastral/BLM_Natl_PLSS_CadNSDI/MapServer/exts/CadastralSpecialServices/GetTRS?lat="
    
    # composite URL for GET request, result is JSON
    u <- switch(returnlevel,
                'S' = {
                  paste0(
                    .baseURL,
                    y, "&lon=", 
                    x, 
                    "&units=DD&returnlevel=S&f=pjson"
                  )
                },
                'I' = {
                  paste0(
                    .baseURL,
                    y, "&lon=", 
                    x, 
                    "&units=DD&returnlevel=I&f=pjson"
                  )
                })
    
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
      message("no resulting PLSS spec returned, check coordinate XY order (longitude: X, latitude: Y)")
      # return "NULL" result
      return(.empty)
    }
    
    # SRID
    srid <- res$features$geometry.spatialReference.wkid
    
    # extract PLSS geometry and convert to sf object
    geom <- sf::st_polygon(x = list(res$features$geometry.rings[[1]][1,, ]))
    geom <- sf::st_sfc(geom, crs = srid)
    geom <- sf::st_sf(geom)
    
    # store PLSS description in sf object
    geom$plss <- res$features$attributes.landdescription
    
  } else {
    # return empy geometry + attributes
    geom <- .empty
  }
  
  # consider returning both geom + PLSS code
  return(geom)
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
  if (is.na(p)) {
    return(NA)
  }
  
  # composite URL for GET request, result is JSON
  u <-
    paste0(
      'https://gis.blm.gov/arcgis/rest/services/Cadastral/',
      'BLM_Natl_PLSS_CadNSDI/MapServer/exts/CadastralSpecialServices/',
      'GetLatLon?trs=',
      p,
      '&f=pjson'
    )
  
  # process GET request
  r <- httr::GET(u)
  httr::stop_for_status(r)
  
  # convert JSON -> list
  r <- jsonlite::fromJSON(httr::content(r, as = 'text'), flatten = TRUE)
  
  # test for an error condition
  # usually poorly-formatted PLSS
  if(!is.null(r$error)) {
    .msg <- sprintf("%s: %s", p, r$error$message)
    message(.msg)
    # use NA coordinates
    res <- data.frame(plssid = p, lat = NA, lon = NA, .note = 'invalid PLSS specification')
    
  } else {
    # successful request
    if(r$status == 'success') {
      # NOTE: r$coordinates$plssid is laundered
      # copy original from r$trs
      r$coordinates$plssid <- r$trs
      
      # keep only 'coordinates' data.frame
      r <- r$coordinates
      
      # request that are less than QQ precision will return multiple QQ centers
      # flatten via mean coordinates
      if (nrow(r) > 1) {
        r <- data.frame(
          plssid = p, 
          t(colMeans(r[, 2:3], na.rm = TRUE))
        )
      }
      
      r[['.note']] <- 'success'
      res <- r
    } else{
      # NOTE: could be related to aberrations in the PLSS fabric
      # stats == 'fail'
      .msg <- sprintf("%s: %s", p, r$statusmsg)
      message(.msg)
      # return NA coordinates
      res <- data.frame(plssid = p, lat = NA, lon = NA, .note = 'no results')
    }
    
  }
  
  row.names(res) <- NULL
  return(res)
}


#' @title Lookup Geographic Coordinates for Public Land Survey System Descriptions
#' @description Fetch geographic coordinates by Public Land Survey System (PLSS)  description from the BLM PLSS web service. Coordinates represent the centroid of each PLSS aliquot defined in `p`.
#' @param p `data.frame` containing (at least) PLSS aliquot part identifiers. These can be generated by [formatPLSS()].
#' @param plssid column name containing PLSS aliquot part identifiers.
#'
#' @return A `data.frame` of PLSS codes and coordinates.
#' @author D.E. Beaudette, Jay Skovlin, A.G. Brown
#' @note Requires the following packages: httr and jsonlite.
#' @seealso [LL2PLSS()], [formatPLSS()]
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
  
  # perform lookup one at a time
  res <- lapply(p[[plssid]], .PLSS2LL)
  
  if (length(res) == 0)
    return(NULL)
  
  res <-  do.call("rbind", res)
  return(res)
}

