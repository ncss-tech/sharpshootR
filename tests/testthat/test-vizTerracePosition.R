context("vizTerracePosition")

## tests
library(aqp, quietly = TRUE)
suppressWarnings(library(cluster, quietly = TRUE))

test_that("vizTerracePosition works as expected", {
  skip_if_not_installed("dendextend")
  skip_if_not_installed("latticeExtra")
  
  # use local data
  data("OSDexamples")
  a <- OSDexamples$terrace
  
  # with / without highlight
  res <- vizTerracePosition(a)
  res <- vizTerracePosition(a, s = 'SIERRA')
  
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

