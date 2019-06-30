context("uLSIF")

test_that("uLSIF", {
  set.seed(3)
  x <- rnorm(200, mean = 1, sd = 1/8)
  y <- rnorm(200, mean = 1, sd = 1/2)

  result <- uLSIF(x, y)

  kernel_weights <- result$kernel_weights
  sigma <- result$kernel_info$sigma
  lambda <- result$lambda

  expected_kernel_weights <- c(0.0674550859, 0.0400446153, 0.0004589047,
                               0.0168489465, 0.0670843163, 0.0189929309)

  testthat::skip_on_cran()
  expect_equal(head(kernel_weights), expected_kernel_weights)
  expect_equal(sigma, 0.1)
  expect_equal(lambda, 1)
})
