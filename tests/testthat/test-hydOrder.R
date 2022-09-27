context("hydOrder")

## tests
library(aqp, quietly = TRUE)
suppressWarnings(library(cluster, quietly = TRUE))
suppressWarnings(library(dendextend, quietly = TRUE))
suppressWarnings(library(latticeExtra, quietly = TRUE))

test_that("hydOrder works as expected", {
  
  # use local data
  data("OSDexamples")
 
  # ensure that SPC and geomorph prop mat share a common set of IDs
  o <- reconcileOSDGeomorph(OSDexamples, 'hillpos')
  
  # no clustering
  res <- hydOrder(o$geom, g = 'hillpos', clust = FALSE)
  
  # result should be a character vector
  expect_true(inherits(res, 'character'))
  
  # should be same length of input SPC
  expect_true(
    length(o$SPC) == length(res)
  )
    
  # with clustering
  res <- hydOrder(o$geom, g = 'hillpos', clust = TRUE)
  
  # result should be a list
  expect_true(inherits(res, 'list'))
  
  # clustering object
  expect_true(inherits(res$clust, 'hclust'))
  
  # should be same length of input SPC
  expect_true(
    length(o$SPC) == length(res$hyd.order)
  )
  
  expect_true(
    length(o$SPC) == length(res$clust.hyd.order)
  )
  
})
