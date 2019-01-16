context("FFD: requires internet connection")

## sample data

# 11 years of data from highland meadows
x <- CDECquery(id='HHM', sensor=32, interval='D', start='2000-01-01', end='2010-12-31')

x.ffd <- FFD(x, returnDailyPr = TRUE, frostTemp=32)

## tests

test_that("FFD works as expected", {

  # basic structure of a result
  expect_equal(names(x.ffd), c('summary', 'fm', 'Pr.frost'))
  
  expect_match(class(x.ffd), 'list')
  expect_match(class(x.ffd$summary), 'data.frame')
  
  expect_match(class(x.ffd$fm), 'matrix')
  expect_true(length(dim(x.ffd$fm)) == 2)
  
  expect_match(class(x.ffd$Pr.frost), 'numeric')
  expect_true(length(x.ffd$Pr.frost) == 366)
  
})


test_that("FFD results are correct", {
  
  ## TODO: test on paper
  # assuming that calculation is correct, looks right
  expect_equal(x.ffd$summary$ffd.50, 70)
  expect_equal(x.ffd$summary$ffd.80, 44)
  expect_equal(x.ffd$summary$ffd.90, 40)
  
  expect_equal(x.ffd$summary$spring.50, 168)
  expect_equal(x.ffd$summary$spring.80, 175)
  expect_equal(x.ffd$summary$spring.90, 178)
  
  expect_equal(x.ffd$summary$fall.50, 238)
  expect_equal(x.ffd$summary$fall.80, 219)
  expect_equal(x.ffd$summary$fall.90, 218)
  
  expect_equal(x.ffd$summary$n.yrs, 11)
  
})


# TODO: test related functions
# findFirstLastFrostDOY
# makeFrostMatrix
# frostFreePeriod
# alignDOY


