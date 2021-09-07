context("vizMountainPosition")

## tests
library(aqp, quietly = TRUE)
suppressWarnings(library(cluster, quietly = TRUE))
suppressWarnings(library(dendextend, quietly = TRUE))
suppressWarnings(library(latticeExtra, quietly = TRUE))

test_that("vizMountainPosition works as expected", {
  
  # use local data
  data("OSDexamples")
  a <- OSDexamples$mtnpos
  
  # with / without highlight
  res <- vizMountainPosition(a)
  res <- vizMountainPosition(a, s = 'MUSICK')
  
  # print(res$fig)
  
  # list of lattice + clustering order
  expect_true(inherits(res, 'list'))
  expect_true(inherits(res$fig, 'trellis'))
  expect_true(inherits(res$order, 'integer'))
  
  # try a singleton
  res <- vizMountainPosition(a[3, ])
  expect_true(inherits(res, 'list'))
  expect_true(inherits(res$fig, 'trellis'))
  expect_true(inherits(res$order, 'integer'))
  
})

