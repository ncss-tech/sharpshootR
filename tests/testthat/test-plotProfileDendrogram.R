context("plotProfileDendrogram")

## sample data

library(aqp)
library(cluster)

# example data that will have IDs re-shuffled by profile_compare (https://github.com/ncss-tech/aqp/issues/7)
set.seed(10101)
s <<- union(lapply(c('Z', LETTERS[1:5]), random_profile, SPC=TRUE))

## tests

test_that("plotProfileDendrogram works as expected", {
  
  # attempt profile comparison: this won't work, throws an error
  d <- profile_compare(s, vars=c('p1','p2'), max_d=100, k=0)
  dd <- diana(d)
  
  # par(mfrow = c(2,1), mar = c(0,0,0,1))
  # plotSPC(s, color='p1')
  res <- plotProfileDendrogram(s, dd, scaling.factor = 0.8, y.offset = 10, color='p1', debug = TRUE, width = 0.3)
  
  # debug info
  expect_true(inherits(res, 'data.frame'))
  
  # profile ID ordering preserved
  expect_equal(res$profileID, c('A', 'B', 'C', 'D', 'E', 'Z'))
  
  # clustering ID should be the same
  expect_equal(res$clustID, c('A', 'B', 'C', 'D', 'E', 'Z'))
  
  # left -> right dendrogram node labels
  expect_equal(res$clustID.ordered, c('A', 'Z', 'B', 'E', 'D', 'C'))
  
  # profile plotting order
  expect_equal(res$profile.plot.order, c(1, 6, 2, 5, 4, 3))
  
})
