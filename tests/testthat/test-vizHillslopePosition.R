context("vizHillslopePosition")

## tests
library(aqp, quietly = TRUE)
suppressWarnings(library(cluster, quietly = TRUE))
suppressWarnings(library(dendextend, quietly = TRUE))
suppressWarnings(library(latticeExtra, quietly = TRUE))

test_that("vizHillslopePosition works as expected", {
  
  # use local data
  data("OSDexamples")
  a <- OSDexamples$hillpos
  
  # with / without highlight
  res <- vizHillslopePosition(a)
  res <- vizHillslopePosition(a, s = 'ZOOK')
  
  # print(res$fig)
  
  # list of lattice + clustering order
  expect_true(inherits(res, 'list'))
  expect_true(inherits(res$fig, 'trellis'))
  expect_true(inherits(res$order, 'integer'))
  expect_true(inherits(res$score, 'numeric'))
  
  
  # try a singleton
  res <- vizFlatsPosition(a[1, ])
  expect_true(inherits(res, 'list'))
  expect_true(inherits(res$fig, 'trellis'))
  expect_true(inherits(res$order, 'integer'))
  expect_true(is.na(res$score))
  
})

