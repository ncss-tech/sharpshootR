context("FFD")

## sample data

# 11 years of data from highland meadows
data('HHM', package = 'sharpshootR')
x.ffd <- FFD(HHM, returnDailyPr = TRUE, frostTemp=32)

## tests

test_that("FFD works as expected", {

  # basic structure of a result
  expect_equal(names(x.ffd), c('summary', 'fm', 'Pr.frost'))
  
  expect_true(inherits(x.ffd, 'list'))
  expect_true(inherits(x.ffd$summary, 'data.frame'))
  
  expect_true(inherits(x.ffd$fm, 'matrix'))
  expect_true(length(dim(x.ffd$fm)) == 2)
  
  expect_true(inherits(x.ffd$Pr.frost, 'numeric'))
  expect_true(length(x.ffd$Pr.frost) == 366)
  
})


test_that("FFD results are correct", {
  
  ## TODO: test on paper
  ## note: different OS will give +/- 1 day, expand fuzz factor via tolerance
  # assuming that calculation is correct, looks right
  expect_equal(x.ffd$summary$ffd.50, 80, tolerance=1)
  expect_equal(x.ffd$summary$ffd.80, 70, tolerance=1)
  expect_equal(x.ffd$summary$ffd.90, 60, tolerance=1)
  
  expect_equal(x.ffd$summary$spring.50, 165, tolerance=1)
  expect_equal(x.ffd$summary$spring.80, 168, tolerance=1)
  expect_equal(x.ffd$summary$spring.90, 169, tolerance=1)
  
  expect_equal(x.ffd$summary$fall.50, 245, tolerance=1)
  expect_equal(x.ffd$summary$fall.80, 238, tolerance=1)
  expect_equal(x.ffd$summary$fall.90, 228, tolerance=1)
  
  expect_equal(x.ffd$summary$n.yrs, 6)
  
})


# TODO: test related functions
# findFirstLastFrostDOY
# makeFrostMatrix
# frostFreePeriod
# alignDOY


