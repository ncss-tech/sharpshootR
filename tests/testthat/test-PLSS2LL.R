


# init empty objects, in case we are on CRAN or offline
d <- NULL
nd <- NULL

test_that("large township and range", {
  
  # ND has some 3 digit township values
  nd <- data.frame(
    id = 1,
    q = NA,
    qq = NA,
    s = 27,
    t = 'T149N',
    r = 'R92W',
    type = 'SN',
    m = 'ND05'
  )
  
  expect_equal(formatPLSS(nd), 'ND051490N0920W0SN270A')
  
  nd$plssid <- formatPLSS(nd)
  
  # update global var, in case not CRAN and online
  nd <<- nd
})

test_that("PLSS2LL() with large township", {
  
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_on_cran()
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  
  # center of township, center of section
  # center of NE,NW,SE,SW corner of northeast corner
  p <- PLSS2LL(nd)
  
  # expected results
  .res <- data.frame(
    plssid = c("ND051490N0920W0SN270A"),
    lat = c(47.6957),
    lon = c(-102.4394),
    .note = 'success'
  )
  
  # original PLSS codes, same order
  expect_equal(p$plssid, .res$plssid)
  
  # coordinates
  expect_equal(p$lon, .res$lon, tolerance = 1e-4)
  expect_equal(p$lat, .res$lat, tolerance = 1e-4)
  
})


test_that("formatPLSS() variations", {
  
  d <- data.frame(id = 1:6, plssid = formatPLSS(data.frame(
    id = 1:6,
    qq = c("", "", "NE", "NW", "SE", "SW"),
    q = c("", "", "NE", "NE", "NE", "NE"),
    s = c("", 11, 11, 11, 11, 11), 
    t = "T25S",
    r = "R38E",
    type = "SN",
    m = "CA21",
    stringsAsFactors = FALSE
  )))
  
  expect_equal(d, data.frame(
    id = 1:6,
    plssid = c(
      "CA210250S0380E0",
      "CA210250S0380E0SN110A",
      "CA210250S0380E0SN110ANENE",
      "CA210250S0380E0SN110ANWNE",
      "CA210250S0380E0SN110ASENE",
      "CA210250S0380E0SN110ASWNE"
    )
  ))
  
  # if qq is supplied without q, expect that the qq is dropped and warning about length
  expect_message(expect_warning({
    data.frame(id = 1, plssid = formatPLSS(data.frame(
      id = 1,
      qq = "SW",
      q = "",
      s = 11, 
      t = "T25S",
      r = "R38E",
      type = "SN",
      m = "CA21",
      stringsAsFactors = FALSE
    )
  ))}))
  
  # update global var, in case not CRAN and online
  d <<- d
})

test_that("PLSS2LL() works", {
  
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_on_cran()
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  
  # center of township, center of section
  # center of NE,NW,SE,SW corner of northeast corner
  p <- PLSS2LL(d)
  
  # expected results
  .res <- data.frame(
    plssid = c(
      "CA210250S0380E0", 
      "CA210250S0380E0SN110A",
      # "CA210250S0380E0SN110ANE", # doesn't work
      "CA210250S0380E0SN110ANENE",
      "CA210250S0380E0SN110ANWNE",
      "CA210250S0380E0SN110ASENE",
      "CA210250S0380E0SN110ASWNE"),
    lat = c(35.7536314968672, 35.7763284345201, 35.7818162070088, 
            35.7818389778208, 35.7781355594337, 35.7781460719862),
    lon = c(-117.883828112581, -117.857196264745, -117.85049298053, 
            -117.854957052084, -117.850511838394, -117.854968737876),
    .note = 'success'
  )
  
  # original PLSS codes, same order
  expect_equal(p$plssid, .res$plssid)
  
  # coordinates
  expect_equal(p$lon, .res$lon, tolerance = 1e-6)
  expect_equal(p$lat, .res$lat, tolerance = 1e-6)
  
})

