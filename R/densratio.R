#' Estimate Density Ratio p(x)/q(y)
#'
#' @param x numeric vector or matrix. Data from a numerator distribution p(x).
#' @param y numeric vector or matrix. Data from a denominator distribution q(y).
#' @param method "uLSIF"(default) or "KLIEP".
#' @param sigma positive numeric vector. Search range of Gaussian kernel bandwidth.
#' @param lambda positive numeric vector. Search range of regularization parameter for uLSIF.
#' @param kernel_num positive integer. Number of kernels.
#' @param fold positive integer. Numer of the folds of cross validation for KLIEP.
#' @param verbose logical(default TRUE).
#'
#' @return densratio object that contains a function to compute estimated density ratio.
#'
#' @examples
#' x <- rnorm(200, mean = 1, sd = 1/8)
#' y <- rnorm(200, mean = 1, sd = 1/2)
#'
#' result <- densratio(x, y)
#'
#' new_x <- seq(0, 2, by = 0.06)
#' estimated_density_ratio <- result$compute_density_ratio(new_x)
#'
#' plot(new_x, estimated_density_ratio, pch=19)
#'
#' @export
densratio <- function(x, y, method = c("uLSIF", "KLIEP"),
                      sigma = "auto", lambda = "auto",
                      kernel_num = 100, fold = 5, verbose = TRUE) {
  # Prepare Arguments -------------------------------------------------------
  method <- match.arg(method)

  # To Retain Default Arguments in Functions of Methods ---------------------
  params <- alist(x = x, y = y, kernel_num = kernel_num, verbose = verbose)
  if(!identical(sigma, "auto")) {
    params <- c(params, alist(sigma = sigma))
  }

  # Run ---------------------------------------------------------------------
  if(method == "uLSIF") {
    if(!identical(lambda, "auto")) params <- c(params, alist(lambda = lambda))
    result <- do.call(uLSIF, params)
  } else {
    params <- c(params, alist(fold = fold))
    result <- do.call(KLIEP, params)
  }

  # Result ------------------------------------------------------------------
  cl <- match.call()
  cl[["method"]] <- method
  result$call <- cl

  class(result) <- c("densratio", class(result))
  result
}


