context("reconcileOSDGeomorph")

## tests
library(aqp, quietly = TRUE)


test_that("reconcileOSDGeomorph works as expected", {
  
  # use local data
  data("OSDexamples")
  
  # test entire suite of geomorphic proportion matrices
  for(i in c('hillpos', 'geomcomp', 'flats', 'mtnpos', 'terrace', 'shape_across', 'shape_down')) {
    o <- reconcileOSDGeomorph(OSDexamples, selection = i)
  
    # number of profiles in SPC should equal number of rows in geom prop mat  
    expect_true(
      length(o$SPC) == nrow(o$geom)
    )
  }
  
})



