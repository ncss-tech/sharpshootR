context("PLSS Conversion")



test_that("formatPLSS, PLSS2LL, LL2PLSS works", {

  # test takes ~30seconds to run, and probably need to bump up timeout for stability against BLM server

    skip_on_cran()

    options(timeout = 60000)

    d <- data.frame(
      id = c(1:2, NA, 3),
      qq = c('SW', NA, NA, NA), # note: deliberately drop section in 2, quarter quarter and quarter in 4
      q = c('NE', NA, NA, NA),  # because qq comes before q, the case where q is defined and q is not is not valid.
      s = c(17, NA, NA, 30),
      t = c('T36N', 'T35N', NA, 'T35N'),
      r = c('R29W', 'R28W', NA, 'R28W'),
      type = 'SN',
      m = 'MT20',
      stringsAsFactors = FALSE
    )

    # generate formatted PLSS codes
    expect_warning(d$plssid <- formatPLSS(d))
    # one or more results is NA; check with `attr(,'na.action')`
    # it is index #3

    d2 <- data.frame(
      id = 1,
      qq = 'SW',
      q = 'NE',
      s = 'foobar',
      t = 'T36N',
      r = 'R29W',
      type = 'SN',
      m = 'MT20',
      stringsAsFactors = FALSE
    )
    expect_warning(formatPLSS(d2)) # cannot convert "foobar"->integer

    # fetch lat/long coordinates (warnings because of NA)
    expect_silent({res <- PLSS2LL(d)})

    # check type and length
    expect_true(inherits(res, 'data.frame'), TRUE)
    expect_equal(nrow(res), 4)

    # position of NA inputs is preserved
    expect_true(is.na(res$plssid[3]))

    # expect error: lng/lat reversed
    expect_error(LL2PLSS(res$lat[1], res$lon[1]))

    # expect silent: LL2PLSS now vectorized (warnings because of NA DROPPED from SpatialPolygons)
    expect_warning({res2 <- LL2PLSS(res$lon, res$lat)})

    # check value for back-transforming first result MT20 T36N R29W Sec. 17 SW NE
    expect_equal(length(res2), 2)
    expect_equal(res2$plss[1], "MT200360N0290W0SN170ASWNE")

    # na.omit
    plssna <- attr(res2$plss, "na.action")
    expect_true(attr(plssna,"class") == 'omit')
    expect_equal(as.numeric(plssna), 3)
})


test_that("LL2PLSS details", {
  
  # constraints
  skip_on_cran()
  options(timeout = 60000)
  
  # north of Clovis, CA
  x <- -119.711302
  y <- 36.928449
  
  # do they work?
  x.I <- LL2PLSS(x = x, y = y, returnlevel = 'I')
  x.S <- LL2PLSS(x = x, y = y, returnlevel = 'S')
  
  # structure
  expect_true(length(x.I) == 2)
  expect_true(length(x.S) == 2)
  
  expect_true(inherits(x.I, 'list'))
  expect_true(inherits(x.S, 'list'))
  
  expect_true(inherits(x.I$geom, 'SpatialPolygons'))
  expect_true(inherits(x.S$geom, 'SpatialPolygons'))
  
  ## graphical check: verified
  plot(x.S$geom)
  plot(x.I$geom, add = TRUE)
  points(x = x, y = y)
  
  
  # try a location without section fabric
  # near Knight's Landing, CA
  x <- -120.73314
  y <- 37.847644
  
  x.I <- LL2PLSS(x = x, y = y, returnlevel = 'I')
  x.S <- LL2PLSS(x = x, y = y, returnlevel = 'S')
  
  expect_true(length(x.I) == 2)
  expect_true(length(x.S) == 2)
  
  expect_true(inherits(x.I, 'list'))
  expect_true(inherits(x.S, 'list'))
  
  expect_true(inherits(x.I$geom, 'SpatialPolygons'))
  expect_true(inherits(x.S$geom, 'SpatialPolygons'))
  
  # no section
  expect_equal(x.S$plss, 'CA210010S0110E0SN000')
  expect_equal(x.I$plss, 'CA210010S0110E0SN000L37')
  
  # attempt inverse
  d <- data.frame(
    id = 1,
    plssid = x.I$plss
  )
  
  # resulting coordinates will not match query coords
  ll <- PLSS2LL(d)
  
  expect_true(inherits(ll, 'data.frame'))
  
  # PLSS -> LL conversion (centroid) will not match query coords
  expect_false(x == ll$lon)
  expect_false(y == ll$lat)
  
  
  ## graphical check: verified
  plot(x.S$geom)
  plot(x.I$geom, add = TRUE)
  # query point
  points(x = x, y = y)
  # PLSS -> LL conversion (centroid)
  points(ll$lon, ll$lat, col = 'red')
  
})


