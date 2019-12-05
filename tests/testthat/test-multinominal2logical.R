context("multinominal2logical")

## sample data
data(loafercreek, package='soilDB')

# convert to logical matrix
hp <- multinominal2logical(loafercreek, 'hillslopeprof')

## tests

test_that("multinominal2logical works as expected", {
  
  # basic structure of a result
  expect_equal(names(hp), c('peiid', 'summit', 'shoulder', 'backslope', 'footslope', 'toeslope'))
  expect_true(inherits(hp, 'data.frame'))
  
  # length is preserved, ordering is not
  expect_true(nrow(hp) == length(loafercreek))
  
  # test a single case
  # note that this will break if loafercreek is re-generated
  # "backslope"
  ex.hp <- hp[which(hp$peiid == 207255), ]
  ex.pedon <- loafercreek[which(profile_id(loafercreek) == 207255), ]
  
  expect_true(ex.hp[[as.character(ex.pedon$hillslopeprof)]])
  
})




