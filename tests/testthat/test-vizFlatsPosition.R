context("vizFlatsPosition")

## tests
library(aqp, quietly = TRUE)
suppressWarnings(library(cluster, quietly = TRUE))

test_that("vizFlatsPosition works as expected", {
  
  skip_if_not_installed("dendextend")
  skip_if_not_installed("latticeExtra")
  
  # use local data
  data("OSDexamples")
  a <- OSDexamples$flats
  
  # with / without highlight
  res <- vizFlatsPosition(a)
  res <- vizFlatsPosition(a, s = 'ZOOK')
  
  # print(res$fig)
  
  # list of lattice + clustering order
  expect_true(inherits(res, 'list'))
  expect_true(inherits(res$fig, 'trellis'))
  expect_true(inherits(res$order, 'integer'))
  expect_true(inherits(res$match.rate, 'numeric'))
  
  
  # try a singleton
  res <- vizFlatsPosition(a[1, ])
  expect_true(inherits(res, 'list'))
  expect_true(inherits(res$fig, 'trellis'))
  expect_true(inherits(res$order, 'integer'))
  expect_true(is.na(res$match.rate))
  
})

