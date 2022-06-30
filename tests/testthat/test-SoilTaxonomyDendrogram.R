context("SoilTaxonomyDendrogram")

## tests
library(aqp, quietly = TRUE)
suppressWarnings(library(cluster, quietly = TRUE))
suppressWarnings(library(ape, quietly = TRUE))

# use local data
data("OSDexamples")
s <- OSDexamples$SPC[1:8, ]
s.list <- site(s)$id


test_that("KST ordering", {
  
  # this should run without errors / messages / warnings
  d <- SoilTaxonomyDendrogram(s, cex.taxon.labels=0.8, width=0.25, y.offset = 0.4, KST.order = TRUE)
  
  # profile IDs and Labels in dist matrix should match
  expect_equal(attr(d$dist, 'Labels'), profile_id(s))
  
  # hard-coded check on ordering of series, based on clustering of taxa
  expect_equal(d$order, c(7, 4, 8, 5, 3, 2, 1, 6))
  
})

test_that("KST ordering, obsolete taxa fallback", {
  
  s$subgroup[1] <- 'funky chunkycrapepts'
  
  # message printed
  expect_message(
    d <- SoilTaxonomyDendrogram(s, cex.taxon.labels=0.8, width=0.25, y.offset = 0.4, KST.order = TRUE)
  )
  
  # profile IDs and Labels in dist matrix should match
  expect_equal(attr(d$dist, 'Labels'), profile_id(s))
  
  # hard-coded check on ordering of series, based on clustering of taxa
  expect_equal(d$order, c(8, 5, 4, 7, 1, 6, 2, 3))
})

test_that("regular factors", {
  
  # this should run without errors / messages / warnings
  d <- SoilTaxonomyDendrogram(s, cex.taxon.labels=0.8, width=0.25, y.offset = 0.4, KST.order = FALSE)
  
  # profile IDs and Labels in dist matrix should match
  expect_equal(attr(d$dist, 'Labels'), profile_id(s))
  
  # hard-coded check on ordering of series, based on clustering of taxa
  expect_equal(d$order, c(1, 2, 3, 5, 8, 7, 6, 4))
  
  # attempt re-ordering, isn't possible to perfectly align
  d <- SoilTaxonomyDendrogram(s, cex.taxon.labels=0.8, width=0.25, y.offset = 0.4, rotationOrder = toupper(s.list))
  
  expect_true(inherits(d, 'list'))
})

## TODO: once latest SoilTaxonomy pkg hist CRAN, add another 2 tests for KST.order = TRUE
