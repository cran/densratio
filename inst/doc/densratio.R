## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE)
library(mvtnorm)

## ------------------------------------------------------------------------
set.seed(3)
x1 <- rnorm(200, mean = 1, sd = 1/8)
x2 <- rnorm(200, mean = 1, sd = 1/2)

library(densratio)
densratio_obj <- densratio(x1, x2)

## ----fig.width=5, fig.height=4-------------------------------------------
new_x <- seq(0, 2, by = 0.05)
w_hat <- densratio_obj$compute_density_ratio(new_x)

plot(new_x, w_hat, pch=19)

## ----fig.width=5, fig.height=4-------------------------------------------
true_density_ratio <- function(x) dnorm(x, 1, 1/8) / dnorm(x, 1, 1/2)

plot(true_density_ratio, xlim=c(0, 2), lwd=2, col="red", xlab = "x", ylab = "Density Ratio")
plot(densratio_obj$compute_density_ratio, xlim=c(0, 2), lwd=2, col="green", add=TRUE)
legend("topright", legend=c(expression(w(x)), expression(hat(w)(x))), col=2:3, lty=1, lwd=2, pch=NA)

## ----eval=FALSE----------------------------------------------------------
#  install.packages("densratio")

## ----eval=FALSE----------------------------------------------------------
#  install.packages("remotes") # If you have not installed "remotes" package
#  remotes::install_github("hoxo-m/densratio")

## ----eval=FALSE----------------------------------------------------------
#  library(densratio)
#  
#  x1 <- rnorm(200, mean = 1, sd = 1/8)
#  x2 <- rnorm(200, mean = 1, sd = 1/2)
#  
#  result <- densratio(x1, x2)

## ----fig.width=5, fig.height=4-------------------------------------------
new_x <- seq(0, 2, by = 0.05)
w_hat <- densratio_obj$compute_density_ratio(new_x)

plot(new_x, w_hat, pch=19)

## ------------------------------------------------------------------------
densratio_obj

## ------------------------------------------------------------------------
library(densratio)
library(mvtnorm)

set.seed(3)
x1 <- rmvnorm(300, mean = c(1, 1), sigma = diag(1/8, 2))
x2 <- rmvnorm(300, mean = c(1, 1), sigma = diag(1/2, 2))

densratio_obj_d2 <- densratio(x1, x2)
densratio_obj_d2

## ----fig.width=7, fig.height=4-------------------------------------------
true_density_ratio <- function(x) {
  dmvnorm(x, mean = c(1, 1), sigma = diag(1/8, 2)) /
    dmvnorm(x, mean = c(1, 1), sigma = diag(1/2, 2))
}

N <- 20
range <- seq(0, 2, length.out = N)
input <- expand.grid(range, range)
w_true <- matrix(true_density_ratio(input), nrow = N)
w_hat <- matrix(densratio_obj_d2$compute_density_ratio(input), nrow = N)

par(mfrow = c(1, 2))
contour(range, range, w_true, main = "True Density Ratio")
contour(range, range, w_hat, main = "Estimated Density Ratio")

