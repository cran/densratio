context("RuLSIF")

test_that("RuLSIF", {
  set.seed(3)
  x <- rnorm(200, mean = 1, sd = 1/8)
  y <- rnorm(200, mean = 1, sd = 1/2)

  result <- RuLSIF(x, y)

  kernel_weights <- result$kernel_weights
  sigma <- result$kernel_info$sigma
  lambda <- result$lambda

  expected_kernel_weights <- c(0.070454411, 0.028303149, 0.003146211,
                               0.010641579, 0.055200243, 0.012069721)

  testthat::skip_on_cran()
  expect_equal(head(kernel_weights), expected_kernel_weights)
  expect_equal(sigma, 0.1)
  expect_equal(lambda, 0.1)
})
