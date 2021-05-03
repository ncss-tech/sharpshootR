context("leaky bucket models")

## TODO: verify on paper


test_that("thermic / xeric WB is reasonable", {
  
  # this is only possible with required packages
  if(!requireNamespace('hydromad'))
    skip(message = 'this test requires the hydomad package')
  
  # AMADOR soil series data
  AWC <- 47
  PPT <- c(65, 59, 57, 28, 13, 3, 0, 1, 4, 20, 33, 53)
  PET <- c(14, 22, 38, 54, 92, 125, 154, 140, 106, 66, 29, 14)
  
  # no model spin-up
  wb <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 1, rep = 1, keep_last = TRUE)
  
  # output structure
  expect_true(inherits(wb, 'data.frame'))
  expect_true(nrow(wb) == 12)
  expect_true(all(wb$month == 1:12))
  
  # AET == PET in Jan
  expect_equal(wb$ET[1], wb$PET[1], tolerance = 0.001)
  
  # AET in July should be 0
  expect_equal(wb$ET[7], 0, tolerance = 0.001)
  
  # total AET
  expect_equal(round(sum(wb$ET)), 224, tolerance = 0.01)
  
})


test_that("thermic / udic WB is reasonable", {
  
  # this is only possible with required packages
  if(!requireNamespace('hydromad'))
    skip(message = 'this test requires the hydomad package')
  
  # LUCY soil series data
  AWC <- 207
  PPT <- c(98, 88, 99, 72, 65, 99, 107, 97, 85, 66, 70, 82)
  PET <- c(12, 18, 40, 65, 113, 151, 171, 157, 115, 66, 33, 15)
  
  # no model spin-up
  wb <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 1, rep = 1, keep_last = TRUE)
  
  # output structure
  expect_true(inherits(wb, 'data.frame'))
  expect_true(nrow(wb) == 12)
  expect_true(all(wb$month == 1:12))
  
  # S is never 0
  expect_equal(length(which(wb$S == 0)), 0)
  
  # total AET
  expect_equal(round(sum(wb$ET)), 810, tolerance = 0.01)
  
})

