test_that("formatPLSS, PLSS2LL, LL2PLSS works", {
  
  # test takes ~30seconds to run, and probably need to bump up timeout for stability against BLM server
    
    skip_on_cran()
  
    options(timeout = 60000)
  
    d <- data.frame(
      id = 1:3,
      qq = c('SW', 'SW', 'SE'),
      q = c('NE', 'NW', 'SE'),
      s = c(17, 32, 30),
      t = c('T36N', 'T35N', 'T35N'),
      r = c('R29W', 'R28W', 'R28W'),
      type = 'SN',
      m = 'MT20',
      stringsAsFactors = FALSE
    )

    # generate formatted PLSS codes
    d$plssid <- formatPLSS(d)

    # fetch lat/long coordinates
    expect_silent({res <- PLSS2LL(d)})
    
    # check type and length
    expect_true(inherits(res, 'data.frame'), TRUE)
    expect_equal(nrow(res), 3)
    
    # expect error: lng/lat reversed
    expect_error(LL2PLSS(res$lat[1], res$lon[1]))
    
    # expect silent: LL2PLSS now vectorized
    expect_silent({res2 <- LL2PLSS(res$lon, res$lat)})
    
    # check value for back-transforming first result MT20 T36N R29W Sec. 17 SW NE
    expect_equal(length(res2), 2)
    expect_equal(res$plss[1], "MT200360N0290W0SN170ASWNE")
})
