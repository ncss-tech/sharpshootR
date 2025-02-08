context("iterateHydOrder")

## tests
library(aqp, quietly = TRUE)
suppressWarnings(library(cluster, quietly = TRUE))

test_that("iterateHydOrder works as expected", {
  skip_if_not_installed("dendextend")
  
  
  # use local data
  data("OSDexamples")
  
  # ensure that SPC and geomorph prop mat share a common set of IDs
  o <- reconcileOSDGeomorph(OSDexamples, 'hillpos')
  
  # defaults
  res <- iterateHydOrder(o$geom, g = 'hillpos')
  
  # result should be a list
  expect_true(inherits(res, 'list'))
  
  # ordering vectors should be same length as SPC
  expect_true(
    length(o$SPC) == length(res$hyd.order)
  )
  expect_true(
    length(o$SPC) == length(res$clust.hyd.order)
  )
  
  
  
})
