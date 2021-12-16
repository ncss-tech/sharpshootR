context("vizSurfaceShape")

## tests
library(aqp, quietly = TRUE)
suppressWarnings(library(cluster, quietly = TRUE))
suppressWarnings(library(dendextend, quietly = TRUE))
suppressWarnings(library(latticeExtra, quietly = TRUE))

test_that("vizSurfaceShape works as expected", {
  
  # use local data
  data("OSDexamples")
  a <- OSDexamples$shape_across
  
  # with / without highlight
  res <- vizSurfaceShape(a)
  res <- vizSurfaceShape(a, s = 'ZOOK')
  
  # print(res$fig)
  
  # list of lattice + clustering order
  expect_true(inherits(res, 'list'))
  expect_true(inherits(res$fig, 'trellis'))
  expect_true(inherits(res$order, 'integer'))
  
  # try a singleton
  res <- vizSurfaceShape(a[2, ])
  expect_true(inherits(res, 'list'))
  expect_true(inherits(res$fig, 'trellis'))
  expect_true(inherits(res$order, 'integer'))
  
})

