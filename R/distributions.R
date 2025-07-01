## CKutils: an R package with some utility functions I use regularly
## Copyright (C) 2025  Chris Kypridemos

## CKutils is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, see <http://www.gnu.org/licenses/>
## or write to the Free Software Foundation, Inc., 51 Franklin Street,
## Fifth Floor, Boston, MA 02110-1301  USA.

#' @useDynLib CKutils, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

#' @name fqBCPEo
#' @title Box-Cox Power Exponential Distribution (BCPEo) - Quantile Function
#' @description Quantile function for the Box-Cox Power Exponential distribution with log link for mu,
#' optimized for SIMD vectorization and unique parameter values.
#' @param p_input vector of probabilities.
#' @param mu vector of (positive) location parameters.
#' @param sigma vector of (positive) scale parameters.
#' @param nu vector of shape parameters.
#' @param tau vector of (positive) shape parameters.
#' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x].
#' @param log_p logical; if TRUE, probabilities p are given as log(p).
#' @return A numeric vector of quantiles.
#' @details The Box-Cox Power Exponential (BCPEo) distribution is a four-parameter continuous
#' distribution defined on the positive real line. The BCPEo variant uses a log link
#' for the location parameter mu.
#' This implementation is based on the gamlss.dist package qBCPEo function
#' but optimized for performance with unique parameter values and SIMD vectorization.
#' @examples
#' # Basic usage
#' p <- c(0.1, 0.5, 0.9)
#' mu <- c(2, 2, 2)
#' sigma <- c(0.5, 0.5, 0.5)
#' nu <- c(1, 1, 1)
#' tau <- c(2, 2, 2)
#'
#' # Calculate quantiles
#' fqBCPEo(p, mu, sigma, nu, tau)
#' @seealso \code{\link{fdBCPEo}}, \code{\link{fpBCPEo}}
#' @references Rigby, R. A. and Stasinopoulos, D. M. (2003) Flexible regression smoothing
#' using GAMLSS. Applied Statistics, 52, 229-237.
#' Stasinopoulos, D. M., Rigby, R. A., Heller, G. Z., Voudouris, V., and
#' De Bastiani, F. (2017) Flexible Regression and Smoothing: Using GAMLSS in R,
#' Chapman and Hall/CRC.
#' @export

#' @name fpBCPEo
#' @title Box-Cox Power Exponential Distribution (BCPEo) - Distribution Function
#' @description Distribution function for the Box-Cox Power Exponential distribution with log link for mu,
#' optimized for SIMD vectorization and unique parameter values.
#' @param q vector of (non-negative) quantiles.
#' @param mu vector of (positive) location parameters.
#' @param sigma vector of (positive) scale parameters.
#' @param nu vector of shape parameters.
#' @param tau vector of (positive) shape parameters.
#' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x].
#' @param log_p logical; if TRUE, probabilities p are given as log(p).
#' @return A numeric vector of probabilities.
#' @details The Box-Cox Power Exponential (BCPEo) distribution is a four-parameter continuous
#' distribution defined on the positive real line. The BCPEo variant uses a log link
#' for the location parameter mu.
#' This implementation is based on the gamlss.dist package pBCPEo function
#' but optimized for performance with unique parameter values and SIMD vectorization.
#' @examples
#' # Basic usage
#' q <- c(1, 2, 3, 4, 5)
#' mu <- c(2, 2, 2, 2, 2)
#' sigma <- c(0.5, 0.5, 0.5, 0.5, 0.5)
#' nu <- c(1, 1, 1, 1, 1)
#' tau <- c(2, 2, 2, 2, 2)
#'
#' # Calculate probabilities
#' fpBCPEo(q, mu, sigma, nu, tau)
#' @seealso \code{\link{fdBCPEo}}, \code{\link{fqBCPEo}}
#' @references Rigby, R. A. and Stasinopoulos, D. M. (2003) Flexible regression smoothing
#' using GAMLSS. Applied Statistics, 52, 229-237.
#' Stasinopoulos, D. M., Rigby, R. A., Heller, G. Z., Voudouris, V., and
#' De Bastiani, F. (2017) Flexible Regression and Smoothing: Using GAMLSS in R,
#' Chapman and Hall/CRC.
#' @export
