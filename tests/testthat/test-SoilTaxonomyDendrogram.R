context("SoilTaxonomyDendrogram -- requires internet connection")

## tests
library(soilDB, quietly = TRUE)
library(cluster, quietly = TRUE)
library(dendextend, quietly = TRUE)

test_that("SoilTaxonomyDendrogram works as expected", {
  
  skip_if_offline()
  
  # soils of interest
  s.list <- c('hornitos', 'auburn', 'pentz', 'pardee', 'peters', 'amador', 'palau')
  
  # fetch and convert data into an SPC
  s <- fetchOSD(s.list)
  
  # this should run without errors / messages / warnings
  d <- SoilTaxonomyDendrogram(s, cex.taxon.labels=0.8, width=0.25, y.offset = 0.4)
  
  # profile IDs and Labels in dist matrix should match
  expect_equal(attr(d$dist, 'Labels'), profile_id(s))
  
  # hard-coded check on ordering of series, based on clustering of taxa
  expect_equal(d$order, c(1, 2, 3, 6, 7, 5, 4))
  
  # attempt re-ordering, isn't possibly to perfectly align
  d <- SoilTaxonomyDendrogram(s, cex.taxon.labels=0.8, width=0.25, y.offset = 0.4, rotationOrder = toupper(s.list))
  expect_true(inherits(d, 'list'))
})

