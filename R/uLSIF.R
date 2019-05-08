#' Estimate Density Ratio p(x)/q(y) by uLSIF (unconstrained Least-Square Importance Fitting)
#'
#' @param x numeric vector or matrix. Data from a numerator distribution p(x).
#' @param y numeric vector or matrix. Data from a denominator distribution q(y).
#' @param sigma positive numeric vector. Search range of Gaussian kernel bandwidth.
#' @param lambda positive numeric vector. Search range of regularization parameter.
#' @param kernel_num positive integer. Number of kernels.
#' @param verbose logical(default TRUE).
#'
#' @return uLSIF object that contains a function to compute estimated density ratio.
#'
#' @export
uLSIF <- function(x, y,
                  sigma = 10 ^ seq(-3, 1, length.out = 9),
                  lambda = 10 ^ seq(-3, 1, length.out = 9),
                  kernel_num = 100, verbose = TRUE) {

  if(verbose) message("################## Start uLSIF ##################")
  if(is.vector(x)) x <- matrix(x)
  if(is.vector(y)) y <- matrix(y)
  if(ncol(x) != ncol(y)) stop("x and y must be same dimensions.")

  nx <- nrow(x)
  ny <- nrow(y)

  kernel_num <- min(kernel_num, nx)
  centers <- x[sample(nx, size = kernel_num), , drop = FALSE]

  if(length(sigma) != 1 || length(lambda) != 1) {
    if(verbose) message("Searching optimal sigma and lambda...")
    opt_params <- uLSIF_search_sigma_and_lambda(x, y, centers, sigma, lambda, verbose)
    sigma <- opt_params$sigma
    lambda <- opt_params$lambda
    if(verbose) message(sprintf("Found optimal sigma = %.3f, lambda = %.3f.", sigma, lambda))
  }

  if(verbose) message("Optimizing alpha...")
  phi_x <- compute_kernel_Gaussian(x, centers, sigma)
  phi_y <- compute_kernel_Gaussian(y, centers, sigma)
  H <- crossprod(phi_y) / ny
  h <- colMeans(phi_x)
  alpha <- solve(H + diag(lambda, kernel_num, kernel_num)) %*% h
  alpha[alpha < 0] <- 0
  if(verbose) message("End.")

  result <- list(alpha = as.vector(alpha),
                 lambda = lambda,
                 kernel_info = list(
                   kernel = "Gaussian RBF",
                   kernel_num = kernel_num,
                   sigma = sigma,
                   centers = centers
                 ),
                 compute_density_ratio = function(x) {
                   if(is.vector(x)) x <- matrix(x)
                   phi_x <- compute_kernel_Gaussian(x, centers, sigma)
                   density_ratio <- as.vector(phi_x %*% alpha)
                   density_ratio
                 }
  )
  class(result) <- c("uLSIF", class(result))
  if(verbose) message("################## Finished uLSIF ###############")
  result
}
