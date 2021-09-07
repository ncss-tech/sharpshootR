context("SoilTaxonomyDendrogram")

## tests
library(aqp, quietly = TRUE)
suppressWarnings(library(cluster, quietly = TRUE))
suppressWarnings(library(dendextend, quietly = TRUE))

test_that("SoilTaxonomyDendrogram works as expected", {
  
  # use local data
  data("OSDexamples")
  s <- OSDexamples$SPC[1:8, ]
  s.list <- site(s)$id
  
  # this should run without errors / messages / warnings
  d <- SoilTaxonomyDendrogram(s, cex.taxon.labels=0.8, width=0.25, y.offset = 0.4)
  
  # profile IDs and Labels in dist matrix should match
  expect_equal(attr(d$dist, 'Labels'), profile_id(s))
  
  # hard-coded check on ordering of series, based on clustering of taxa
  expect_equal(d$order, c(1, 2, 3, 5, 8, 7, 6, 4))
  
  # attempt re-ordering, isn't possible to perfectly align
  d <- SoilTaxonomyDendrogram(s, cex.taxon.labels=0.8, width=0.25, y.offset = 0.4, rotationOrder = toupper(s.list))
  expect_true(inherits(d, 'list'))
})

