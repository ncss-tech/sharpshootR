if (as.integer(R.Version()$major) >= 4) {
  library(testthat)
  library(sharpshootR)

  test_check("sharpshootR")
}