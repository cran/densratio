context("compute_kernel_Gaussian")

test_that("squared_euclid_distance", {
  set.seed(3)
  x <- rnorm(30)
  y <- rnorm(1)
  act <- squared_euclid_distance(x, y)

  expect_equal(act, 57.61699, tolerance = 1e-6)
})
