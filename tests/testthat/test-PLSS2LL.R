test_that("formatPLSS, PLSS2LL, LL2PLSS works", {

  # test takes ~30seconds to run, and probably need to bump up timeout for stability against BLM server

    skip_on_cran()

    options(timeout = 60000)

    d <- data.frame(
      id = c(1:2, NA, 3),
      qq = c('SW', 'SW', NA, 'SE'),
      q = c('NE', 'NW', NA, 'SE'),
      s = c(17, 32, NA, 30),
      t = c('T36N', 'T35N', NA, 'T35N'),
      r = c('R29W', 'R28W', NA, 'R28W'),
      type = 'SN',
      m = 'MT20',
      stringsAsFactors = FALSE
    )

    # generate formatted PLSS codes
    d$plssid <- formatPLSS(d)

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
    expect_true(attr(plssna,"class") == 'omit' && as.integer(plssna) == 3)
})
