test_that("formatPLSS, PLSS2LL, LL2PLSS works", {
  
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
    
    expect_true(inherits(res, 'data.frame'), TRUE)
    expect_equal(nrow(res), 3)
    
    # expect error: LL2PLSS not vectorized
    expect_error(LL2PLSS(res$lat, res$lon))
    
    # expect error: lat/lng reversed
    expect_error(LL2PLSS(res$lat[1], res$lon[1]))
    
    # make sure value for back-transforming first result: MT20 T36N R29W Sec. 17 SW NE
    expect_silent({res <- LL2PLSS(res$lon[1], res$lat[1])})
    expect_equal(length(res), 2)
    expect_equal(res$plss, "MT200360N0290W0SN170ASWNE")
})
