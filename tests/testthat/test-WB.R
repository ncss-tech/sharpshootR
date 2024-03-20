context("leaky bucket models")

# example WB from tropical climate
wb.tropical <- structure(
  list(PPT = c(56.41, 35.04, 49.97, 78, 266.66, 390.97, 304.66, 339.95, 371.03, 239.23, 99.31, 71.14), PET = c(65.7381009906128, 66.7953817708974, 92.0306540584507, 105.299973551709, 111.653291690732, 104.055517872244, 103.722049943755, 102.16665065397, 92.337490005685, 84.7099923120343, 68.9292596916165, 66.7089212706967), U = c(0, 0, 0, 0, 47.27098529304, 286.914482127756, 200.937950056245, 237.78334934603, 278.692509994315, 154.520007687966, 30.3807403083835, 4.43107872930327), S = c(190.671899009387, 158.91651723849, 116.855863180039, 92.2642769837715, 200, 200, 200, 200, 200, 200, 200, 200), ET = c(65.7381009906128, 66.7953817708974, 92.0306540584507, 102.591586196268, 111.653291690732, 104.055517872244, 103.722049943755, 102.16665065397, 92.337490005685, 84.7099923120343, 68.9292596916165, 66.7089212706967), D = c(0, 0, 0, -2.70838735544127, 0, 0, 0, 0, 0, 0, 0, 0), month = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), mo = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")), row.names = c(NA, -12L), class = "data.frame", AWC = 200
)


## TODO: verify on paper

## TODO: discrepancy likely has something to do with a declining availability function

test_that("Arkley and Ulrich 1962, Table 1", {
  

  skip(message = "unresolved issues with original method")
  
  # requires a non-CRAN package to function
  skip_on_cran()
  
  skip_if_not_installed("hydromad")
  
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
  wb <- monthlyWB(AWC, PPT, PET, S_init = 1, starting_month = 1, rep = 1, distribute = FALSE)
  
  # attr(wb, 'mass.balance')
  # plotWB(wb)
  
  ## TODO: why the deviation?
  ##       must have something to do with PPT at end of month vs. middle?
  data.frame(Table1 = S, model = wb$S)
  
  sum((S - wb$S)^2)
  
  ## PPT - PET matches values in Table 1 
  # (PPT - PET) / 25.4
  
  ## 11.69" in Table 1 (4" AWC)
  ## vs. 10.7" here
  # sum(wb$ET) / 25.4
  
  
  ## TODO: why the deviation?
  ##       must have something to do with PPT at end of month vs. middle?
  data.frame(Table1 = AET, model = wb$ET)
  
  sum((AET - wb$ET)^2)
  
})


test_that("thermic / xeric WB is reasonable", {
  
  # requires a non-CRAN package to function
  skip_on_cran()
  
  skip_if_not_installed("hydromad")
  
  # AMADOR soil series data
  AWC <- 47
  PPT <- c(65, 59, 57, 28, 13, 3, 0, 1, 4, 20, 33, 53)
  PET <- c(14, 22, 38, 54, 92, 125, 154, 140, 106, 66, 29, 14)
  
  # no model spin-up
  wb <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 1, rep = 1, keep_last = TRUE)
  
  # check mass balance
  expect_true(attr(wb, 'mass.balance') == 0)
  
  # output structure
  expect_true(inherits(wb, 'data.frame'))
  expect_true(nrow(wb) == 12)
  expect_true(all(wb$month == 1:12))
  
  # AET == PET in Jan
  expect_equal(wb$ET[1], wb$PET[1], tolerance = 0.001)
  
  # AET in July should be 0
  expect_equal(wb$ET[7], 0, tolerance = 0.001)
  
  # total AET
  skip_if_not_installed("hydromad", "0.9-27")
  expect_equal(round(sum(wb$ET)), 224, tolerance = 0.01)
  
})


test_that("thermic / udic WB is reasonable", {
  
  # requires a non-CRAN package to function
  skip_on_cran()
  
  skip_if_not_installed("hydromad")
  
  # LUCY soil series data
  AWC <- 207
  PPT <- c(98, 88, 99, 72, 65, 99, 107, 97, 85, 66, 70, 82)
  PET <- c(12, 18, 40, 65, 113, 151, 171, 157, 115, 66, 33, 15)
  
  # no model spin-up
  wb <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 1, rep = 1, keep_last = TRUE, distribute = FALSE)
  
  # check mass balance
  expect_true(attr(wb, 'mass.balance') == 0)
  
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
  
  skip_if_not_installed("hydromad")
  
  # UDIC
  AWC <- 100
  PPT <- c(98, 88, 99, 72, 65, 99, 107, 97, 85, 66, 70, 82)
  PET <- rep(0, times = length(PPT))
  
  # no model spin-up
  wb <- monthlyWB(AWC, PPT, PET, S_init = 1, starting_month = 1, rep = 1, keep_last = TRUE)
  
  # check mass balance
  expect_true(attr(wb, 'mass.balance') == 0)
  
  # output structure
  expect_true(inherits(wb, 'data.frame'))
  expect_true(nrow(wb) == 12)
  expect_true(all(wb$month == 1:12))
  
  # S always "full"
  expect_true(all(wb$S == AWC))
  
  # U == PPT
  expect_true(all(wb$PPT == wb$U))
  
})



## TODO: update this
test_that("water balance summary: zero dry days", {
  
  # requires a non-CRAN package to function
  skip_on_cran()
  
  skip_if_not_installed("hydromad")
  
  # using example data from tropical environment
  wbs <- monthlyWB_summary(wb.tropical)
  
  # there should be no dry days
  expect_true(wbs$dry == 0)
  expect_true(wbs$dry_con == 0)
  
  # well defined wet season, state == consecutive state
  expect_true(wbs$wet == wbs$wet_con)
  expect_true(wbs$moist == wbs$moist_con)
  
})





