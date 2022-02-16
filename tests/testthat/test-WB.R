context("leaky bucket models")

## TODO: verify on paper


test_that("Arkley and Ulrich 1962, Table 1", {
  

  skip(message = "unresolved issues with original method")
  
  # requires a non-CRAN package to function
  skip_on_cran()
  
  # 4" AWC (~ 100mm)
  # inches -> mm
  AWC <- 4 * 25.4 
  
  # monthly values from Table 1
  # inches -> mm
  PPT <- c(2.66, 2.76, 2.09, 1.38, 0.54, 0.11 , 0, 0, 0.06, 0.91, 1.50, 3.01) * 25.4
  PET <- c(0.53, 0.88, 1.55, 2.14, 3.45, 4.58, 5.62, 5.00, 3.86, 2.53, 1.20, 0.53) * 25.4
  
  # storage from Table 1 (4" AWC)
  # inches -> mm
  S <- c(4.0, 4.0, 4.0, 3.24, 0.33, 0, 0, 0, 0, 0, 0.30, 2.78) * 25.4
  
  # AET from Table 1 (4" AWC)
  # inches -> mm
  AET <- c(0.53, 0.88, 1.55, 2.14, 3.45, 0.44, 0, 0, 0.06, 0.91, 1.20, 0.53) * 25.4
  
  
  # no model spin-up
  # start with soil "full"
  wb <- monthlyWB(AWC, PPT, PET, S_init = AWC, starting_month = 1, rep = 1, keep_last = TRUE)
  
  ## TODO: why the deviation?
  ##       must have something to do with PPT at end of month vs. middle?
  data.frame(Table1 = S, model = wb$S)
  
  ## PPT - PET matches values in Table 1 
  # (PPT - PET) / 25.4
  
  ## 11.69" in Table 1 (4" AWC)
  ## vs. 10.7" here
  # sum(wb$ET) / 25.4
  
  
  ## TODO: why the deviation?
  ##       must have something to do with PPT at end of month vs. middle?
  data.frame(Table1 = AET, model = wb$ET)
  
  
})


test_that("thermic / xeric WB is reasonable", {
  
  # requires a non-CRAN package to function
  skip_on_cran()
  
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
  
  # requires a non-CRAN package to function
  skip_on_cran()
  
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


test_that("zero-PET, UDIC", {
  
  # requires a non-CRAN package to function
  skip_on_cran()
  
  # UDIC
  AWC <- 100
  PPT <- c(98, 88, 99, 72, 65, 99, 107, 97, 85, 66, 70, 82)
  PET <- rep(0, times = length(PPT))
  
  # no model spin-up
  wb <- monthlyWB(AWC, PPT, PET, S_init = 1, starting_month = 1, rep = 1, keep_last = TRUE)
  
  # output structure
  expect_true(inherits(wb, 'data.frame'))
  expect_true(nrow(wb) == 12)
  expect_true(all(wb$month == 1:12))
  
  # S always "full"
  expect_true(all(wb$S == AWC))
  
  # U == PPT
  expect_true(all(wb$PPT == wb$U))
  
})


