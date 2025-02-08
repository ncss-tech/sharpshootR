context("vizAnnualClimate")

## tests
library(aqp, quietly = TRUE)
suppressWarnings(library(cluster, quietly = TRUE))

test_that("vizAnnualClimate works as expected", {
  skip_if_not_installed("dendextend")
  skip_if_not_installed("latticeExtra")
  # use local data
  data("OSDexamples")
  a <- OSDexamples$climate.annual
  
  # with / without highlight
  res <- vizAnnualClimate(a)
  res <- vizAnnualClimate(a, s = 'ZOOK')
  
  # print(res$fig)
  
  # list of lattice + clustering
  expect_true(inherits(res, 'list'))
  expect_true(inherits(res$fig, 'trellis'))
  expect_true(inherits(res$clust, 'diana'))
  
  # try a singleton
  res <- vizAnnualClimate(a[1:8, ])
  expect_true(inherits(res, 'list'))
  expect_true(inherits(res$fig, 'trellis'))
  # no clustering possible, result is NULL
  expect_false(inherits(res$clust, 'diana'))
  
})

