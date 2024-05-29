context("component.adj.matrix")

## sample data
data(amador)

## tests

test_that("component.adj.matrix works as expected", {

  skip_if_not_installed("vegan")
  
  # both methods
  m.cm <- component.adj.matrix(amador, method = 'community.matrix')
  m.oc <- component.adj.matrix(amador, method = 'occurrence')
  
  # check metadata
  expect_true(attr(m.cm, 'method') == 'community.matrix')
  expect_true(attr(m.oc, 'method') == 'occurrence')
  
  # spot-check values
  
  # co-occurrence counts
  expect_true(
    all(m.oc[1, ] == c(0, 2, 1, 4, 2, 4, 2, 7, 4, 2, 2))
  )
  
  # community matrix based similarity
  expect_equal(
    as.vector(m.cm[1, ]),
    c(0, 0.003922, 0.01235, 0.02614, 0.1975, 0.02614, 0.01481, 0.04256, 
      0.01874, 0.01481, 0.01481), 
    tolerance = 0.0001
  )
  
  # correlation between methods
  expect_equal(
    signif(cor(as.vector(m.oc), as.vector(m.cm)), 3),
    0.519,
    threshold = 0.1
  )
  
})

