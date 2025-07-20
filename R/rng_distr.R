#' Random Generation for Box-Cox Power Exponential (BCPEo) Distribution
#' 
#' Generates random deviates from the Box-Cox Power Exponential distribution 
#' using high-quality pseudo-random number generation via the dqrng package.
#' 
#' @param n Number of observations to generate. If length(n) > 1, the length 
#'   is taken to be the number required.
#' @param mu Vector of location parameters (positive). Default is 5. Note that 
#'   mu represents the median of the distribution.
#' @param sigma Vector of scale parameters (positive). Default is 0.1.
#' @param nu Vector of first shape parameters (real). Default is 1. Controls 
#'   the skewness of the distribution.
#' @param tau Vector of second shape parameters (positive). Default is 2. 
#'   Controls the kurtosis of the distribution.
#' 
#' @details
#' This function generates random variates from the BCPEo distribution by:
#' \enumerate{
#'   \item Generating high-quality uniform random numbers using \code{dqrng::dqrunif}
#'   \item Applying the inverse CDF transformation using \code{fqBCPEo}
#' }
#' 
#' The BCPEo distribution is a four-parameter continuous distribution that 
#' extends the Box-Cox normal distribution. It's particularly useful for 
#' modelling positive continuous data with flexible skewness and kurtosis.
#' 
#' The use of \code{dqrng::dqrunif} provides superior random number generation 
#' compared to base R's \code{runif}, with better statistical properties and 
#' faster generation speeds.
#' 
#' @return Vector of random deviates from the BCPEo distribution.
#' 
#' @examples
#' # Generate 100 random values with default parameters
#' x <- frBCPEo(100)
#' hist(x, main = "BCPEo Random Variates")
#' 
#' # Generate with custom parameters
#' x <- frBCPEo(1000, mu = 2, sigma = 0.5, nu = 0.5, tau = 3)
#' summary(x)
#' 
#' # Vector of parameters (recycling applies)
#' x <- frBCPEo(10, mu = c(1, 2), sigma = 0.3, nu = c(-0.5, 0.5), tau = 4)
#' 
#' @references
#' Rigby, R.A., Stasinopoulos, D.M., Heller, G.Z., and De Bastiani, F. (2019). 
#' Distributions for modelling location, scale, and shape: Using GAMLSS in R. 
#' Chapman and Hall/CRC.
#' 
#' @seealso 
#' \code{\link{fdBCPEo}}, \code{\link{fpBCPEo}}, \code{\link{fqBCPEo}} for 
#' other BCPEo distribution functions.
#' 
#' \code{\link[gamlss.dist]{rBCPE}} for the original implementation.
#' 
#' \code{\link[dqrng]{dqrunif}} for the high-quality random number generator used.
#' 
#' @export
#' @importFrom dqrng dqrunif
frBCPEo <- function(n, mu = 5, sigma = 0.1, nu = 1, tau = 2) {
    if (any(n <= 0)) {
        stop(paste("n must be a positive integer", "\n", ""))
    }
    n <- ceiling(n)
    p <- dqrng::dqrunif(n)
    r <- fqBCPEo(p, mu = mu, sigma = sigma, nu = nu, tau = tau)
    r
}

#' Random Generation for Box-Cox t (BCT) Distribution
#' 
#' Generates random deviates from the Box-Cox t distribution using high-quality 
#' pseudo-random number generation via the dqrng package.
#' 
#' @param n Number of observations to generate. If length(n) > 1, the length 
#'   is taken to be the number required.
#' @param mu Vector of location parameters (positive). Default is 5. Note that 
#'   mu represents the median of the distribution.
#' @param sigma Vector of scale parameters (positive). Default is 0.1. For 
#'   moderate nu > 0 and moderate or large tau, sigma*sqrt(tau/(tau-2)) 
#'   approximates the coefficient of variation.
#' @param nu Vector of shape parameters (real). Default is 1. Controls the 
#'   skewness of the distribution.
#' @param tau Vector of degrees of freedom parameters (positive). Default is 2. 
#'   Controls the kurtosis of the distribution.
#' 
#' @details
#' This function generates random variates from the BCT distribution by:
#' \enumerate{
#'   \item Generating high-quality uniform random numbers using \code{dqrng::dqrunif}
#'   \item Applying the inverse CDF transformation using \code{fqBCT}
#' }
#' 
#' The BCT distribution is a four-parameter continuous distribution that extends 
#' the Box-Cox normal distribution by replacing the normal kernel with a 
#' t-distribution kernel. This provides additional flexibility for modelling 
#' heavy-tailed data while maintaining the Box-Cox transformation properties.
#' 
#' Key characteristics:
#' \itemize{
#'   \item \strong{Location}: mu is the median of the distribution
#'   \item \strong{Scale}: sigma controls the spread
#'   \item \strong{Skewness}: nu controls asymmetry (nu = 0 gives symmetry)
#'   \item \strong{Kurtosis}: tau controls tail heaviness (larger tau = lighter tails)
#' }
#' 
#' The use of \code{dqrng::dqrunif} provides superior random number generation 
#' compared to base R's \code{runif}, with better statistical properties and 
#' significantly faster generation speeds.
#' 
#' @return Vector of random deviates from the BCT distribution.
#' 
#' @examples
#' # Generate 100 random values with default parameters
#' x <- frBCT(100)
#' hist(x, main = "BCT Random Variates", breaks = 30)
#' 
#' # Generate with custom parameters for heavy-tailed data
#' x <- frBCT(1000, mu = 2, sigma = 0.5, nu = 0.3, tau = 3)
#' summary(x)
#' 
#' # Compare different tau values (tail heaviness)
#' x_light <- frBCT(500, mu = 1, sigma = 0.3, nu = 0, tau = 10)  # Light tails
#' x_heavy <- frBCT(500, mu = 1, sigma = 0.3, nu = 0, tau = 2.5) # Heavy tails
#' 
#' # Vector of parameters (recycling applies)
#' x <- frBCT(10, mu = c(1, 2), sigma = 0.3, nu = c(-0.5, 0.5), tau = 4)
#' 
#' # Verify median property
#' x <- frBCT(10000, mu = 5, sigma = 0.2, nu = 0.1, tau = 6)
#' median(x)  # Should be approximately 5
#' 
#' @references
#' Rigby, R.A. and Stasinopoulos, D.M. (2006). Using the Box-Cox t distribution 
#' in GAMLSS to model skewness and kurtosis. Statistical Modelling, 6(3), 200.
#' 
#' Rigby, R.A., Stasinopoulos, D.M., Heller, G.Z., and De Bastiani, F. (2019). 
#' Distributions for modelling location, scale, and shape: Using GAMLSS in R. 
#' Chapman and Hall/CRC.
#' 
#' @seealso 
#' \code{\link{fdBCT}}, \code{\link{fpBCT}}, \code{\link{fqBCT}} for 
#' other BCT distribution functions.
#' 
#' \code{\link[gamlss.dist]{rBCT}} for the original implementation.
#' 
#' \code{\link[dqrng]{dqrunif}} for the high-quality random number generator used.
#' 
#' @export
#' @importFrom dqrng dqrunif
frBCT <- function(n, mu = 5, sigma = 0.1, nu = 1, tau = 2) {
    if (any(n <= 0)) {
        stop(paste("n must be a positive integer", "\n", ""))
    }
    n <- ceiling(n)
    p <- dqrng::dqrunif(n)
    r <- fqBCT(p, mu = mu, sigma = sigma, nu = nu, tau = tau)
    r
}



#' Random Generation for Beta Negative Binomial (BNB) Distribution
#' 
#' Generates random deviates from the Beta Negative Binomial distribution 
#' using high-quality pseudo-random number generation via the dqrng package.
#' 
#' @param n Number of observations to generate. If length(n) > 1, the length 
#'   is taken to be the number required.
#' @param mu Vector of positive mean parameters. Default is 1.
#' @param sigma Vector of positive dispersion parameters. Default is 1.
#' @param nu Vector of positive shape parameters. Default is 1.
#' 
#' @details
#' This function generates random variates from the BNB distribution by:
#' \enumerate{
#'   \item Generating high-quality uniform random numbers using \code{dqrng::dqrunif}
#'   \item Applying the inverse CDF transformation using \code{fqBNB}
#' }
#' 
#' The Beta Negative Binomial distribution is a discrete distribution that 
#' extends the negative binomial distribution by allowing for additional 
#' overdispersion through a beta-distributed mixing parameter. It's particularly 
#' useful for modelling count data with excess variability.
#' 
#' The probability mass function is:
#' \deqn{f(y|\mu,\sigma,\nu) = \frac{\Gamma(y+1/\nu)\mathrm{B}(y+(\mu\nu)/\sigma, 1/\sigma+1/\nu+1)}{\Gamma(y+1)\Gamma(1/\nu)\mathrm{B}((\mu\nu)/\sigma, 1/\sigma+1)}}
#' for \eqn{y = 0, 1, 2, \ldots}, \eqn{\mu > 0}, \eqn{\sigma > 0}, and \eqn{\nu > 0}.
#' 
#' The use of \code{dqrng::dqrunif} provides superior random number generation 
#' compared to base R's \code{runif}, with better statistical properties and 
#' significantly faster generation speeds.
#' 
#' @return Vector of random integers from the BNB distribution.
#' 
#' @examples
#' # Generate 100 random values with default parameters
#' x <- frBNB(100)
#' table(x)
#' 
#' # Generate with custom parameters
#' x <- frBNB(1000, mu = 5, sigma = 2, nu = 1.5)
#' summary(x)
#' mean(x)  # Should be approximately mu = 5
#' 
#' # Compare overdispersion with different sigma values
#' x_low <- frBNB(500, mu = 3, sigma = 0.5, nu = 1)   # Low overdispersion
#' x_high <- frBNB(500, mu = 3, sigma = 2, nu = 1)    # High overdispersion
#' var(x_low); var(x_high)  # Higher sigma gives more variability
#' 
#' # Vector of parameters (recycling applies)
#' x <- frBNB(10, mu = c(2, 4), sigma = c(1, 1.5), nu = 1)
#' 
#' @references
#' Rigby, R.A., Stasinopoulos, D.M., Heller, G.Z., and De Bastiani, F. (2019). 
#' Distributions for modelling location, scale, and shape: Using GAMLSS in R. 
#' Chapman and Hall/CRC.
#' 
#' @seealso 
#' \code{\link{fdBNB}}, \code{\link{fpBNB}}, \code{\link{fqBNB}} for 
#' other BNB distribution functions.
#' 
#' \code{\link[gamlss.dist]{rBNB}} for the original implementation.
#' 
#' \code{\link[dqrng]{dqrunif}} for the high-quality random number generator used.
#' 
#' @export
#' @importFrom dqrng dqrunif
frBNB <- function(n, mu = 1, sigma = 1, nu = 1) {
    if (any(n <= 0)) {
        stop(paste("n must be a positive integer", "\n", ""))
    }
    n <- ceiling(n)
    p <- dqrng::dqrunif(n)
    r <- fqBNB(p, mu = mu, sigma = sigma, nu = nu)
    r
}


#' Random Generation for Zero Inflated Beta Negative Binomial (ZIBNB) Distribution
#' 
#' Generates random deviates from the Zero Inflated Beta Negative Binomial 
#' distribution using high-quality pseudo-random number generation via the 
#' dqrng package.
#' 
#' @param n Number of observations to generate. If length(n) > 1, the length 
#'   is taken to be the number required.
#' @param mu Vector of positive mean parameters for the BNB component. Default is 1.
#' @param sigma Vector of positive dispersion parameters for the BNB component. Default is 1.
#' @param nu Vector of positive shape parameters for the BNB component. Default is 1.
#' @param tau Vector of zero inflation probabilities (0 < tau < 1). Default is 0.1.
#' 
#' @details
#' This function generates random variates from the ZIBNB distribution by:
#' \enumerate{
#'   \item Generating high-quality uniform random numbers using \code{dqrng::dqrunif}
#'   \item Applying the inverse CDF transformation using \code{fqZIBNB}
#' }
#' 
#' The Zero Inflated Beta Negative Binomial distribution is a discrete distribution 
#' that extends the Beta Negative Binomial distribution to handle excess zeros. 
#' It's a mixture of a point mass at zero and a BNB distribution, making it 
#' particularly useful for modelling count data with structural zeros.
#' 
#' The probability mass function is:
#' \deqn{f(y|\mu,\sigma,\nu,\tau) = \begin{cases}
#' \tau + (1-\tau) \cdot f_{BNB}(0|\mu,\sigma,\nu) & \text{if } y = 0 \\
#' (1-\tau) \cdot f_{BNB}(y|\mu,\sigma,\nu) & \text{if } y > 0
#' \end{cases}}
#' where \eqn{f_{BNB}} is the BNB probability mass function.
#' 
#' The use of \code{dqrng::dqrunif} provides superior random number generation 
#' compared to base R's \code{runif}, with better statistical properties and 
#' significantly faster generation speeds.
#' 
#' @return Vector of random integers from the ZIBNB distribution.
#' 
#' @examples
#' # Generate 100 random values with default parameters
#' x <- frZIBNB(100)
#' table(x)
#' sum(x == 0) / length(x)  # Proportion of zeros
#' 
#' # Generate with custom parameters - high zero inflation
#' x <- frZIBNB(1000, mu = 5, sigma = 2, nu = 1.5, tau = 0.3)
#' sum(x == 0) / length(x)  # Should be approximately 0.3 + some structural zeros
#' 
#' # Compare with regular BNB (no zero inflation)
#' x_bnb <- frBNB(1000, mu = 5, sigma = 2, nu = 1.5)
#' x_zibnb <- frZIBNB(1000, mu = 5, sigma = 2, nu = 1.5, tau = 0.2)
#' sum(x_bnb == 0) / length(x_bnb)    # Natural zeros only
#' sum(x_zibnb == 0) / length(x_zibnb) # Natural + inflated zeros
#' 
#' # Vector of parameters (recycling applies)
#' x <- frZIBNB(10, mu = c(2, 4), sigma = 1.5, nu = 1, tau = c(0.1, 0.2))
#' 
#' @references
#' Rigby, R.A., Stasinopoulos, D.M., Heller, G.Z., and De Bastiani, F. (2019). 
#' Distributions for modelling location, scale, and shape: Using GAMLSS in R. 
#' Chapman and Hall/CRC.
#' 
#' @seealso 
#' \code{\link{fqZIBNB}} for the quantile function.
#' 
#' \code{\link{frBNB}} for the base BNB distribution without zero inflation.
#' 
#' \code{\link[gamlss.dist]{rZIBNB}} for the original implementation.
#' 
#' \code{\link[dqrng]{dqrunif}} for the high-quality random number generator used.
#' 
#' @export
#' @importFrom dqrng dqrunif
frZIBNB <- function(n, mu = 1, sigma = 1, nu = 1, tau = 0.1) {
    if (any(n <= 0)) {
        stop(paste("n must be a positive integer", "\n", ""))
    }
    n <- ceiling(n)
    p <- dqrng::dqrunif(n)
    r <- fqZIBNB(p, mu = mu, sigma = sigma, nu = nu, tau = tau)
    r
}



#' Random Generation for Zero Adjusted Beta Negative Binomial (ZABNB) Distribution
#' 
#' Generates random deviates from the Zero Adjusted (Hurdle) Beta Negative 
#' Binomial distribution using high-quality pseudo-random number generation 
#' via the dqrng package.
#' 
#' @param n Number of observations to generate. If length(n) > 1, the length 
#'   is taken to be the number required.
#' @param mu Vector of positive mean parameters for the BNB component. Default is 1.
#' @param sigma Vector of positive dispersion parameters for the BNB component. Default is 1.
#' @param nu Vector of positive shape parameters for the BNB component. Default is 1.
#' @param tau Vector of hurdle probabilities (0 < tau < 1). Default is 0.1.
#' 
#' @details
#' This function generates random variates from the ZABNB distribution by:
#' \enumerate{
#'   \item Generating high-quality uniform random numbers using \code{dqrng::dqrunif}
#'   \item Applying the inverse CDF transformation using \code{fqZABNB}
#' }
#' 
#' The Zero Adjusted (Hurdle) Beta Negative Binomial distribution is a discrete 
#' distribution that models count data with a two-part process: a hurdle component 
#' that determines whether the count is zero, and a truncated BNB component for 
#' positive counts. Unlike zero inflation, this is a true hurdle model where 
#' zeros can only come from the hurdle process.
#' 
#' The probability mass function is:
#' \deqn{f(y|\mu,\sigma,\nu,\tau) = \begin{cases}
#' \tau & \text{if } y = 0 \\
#' (1-\tau) \cdot \frac{f_{BNB}(y|\mu,\sigma,\nu)}{1-f_{BNB}(0|\mu,\sigma,\nu)} & \text{if } y > 0
#' \end{cases}}
#' where \eqn{f_{BNB}} is the BNB probability mass function.
#' 
#' The key difference from zero inflation is that this model explicitly separates 
#' the zero-generating process from the count-generating process, making it 
#' appropriate when zeros and positive counts arise from different mechanisms.
#' 
#' The use of \code{dqrng::dqrunif} provides superior random number generation 
#' compared to base R's \code{runif}, with better statistical properties and 
#' significantly faster generation speeds.
#' 
#' @return Vector of random integers from the ZABNB distribution.
#' 
#' @examples
#' # Generate 100 random values with default parameters
#' x <- frZABNB(100)
#' table(x)
#' sum(x == 0) / length(x)  # Proportion of zeros from hurdle
#' 
#' # Generate with custom parameters - moderate hurdle probability
#' x <- frZABNB(1000, mu = 5, sigma = 2, nu = 1.5, tau = 0.25)
#' sum(x == 0) / length(x)  # Should be approximately 0.25
#' 
#' # Compare hurdle vs zero inflation models
#' x_zabnb <- frZABNB(1000, mu = 5, sigma = 2, nu = 1.5, tau = 0.2)  # Hurdle
#' x_zibnb <- frZIBNB(1000, mu = 5, sigma = 2, nu = 1.5, tau = 0.2)  # Zero inflation
#' sum(x_zabnb == 0) / length(x_zabnb)  # Exactly tau proportion
#' sum(x_zibnb == 0) / length(x_zibnb)  # tau + natural zeros
#' 
#' # Study the truncated positive part
#' x_positive <- x_zabnb[x_zabnb > 0]
#' summary(x_positive)  # No zeros by design
#' 
#' # Vector of parameters (recycling applies)
#' x <- frZABNB(10, mu = c(2, 4), sigma = 1.5, nu = 1, tau = c(0.15, 0.25))
#' 
#' @references
#' Rigby, R.A., Stasinopoulos, D.M., Heller, G.Z., and De Bastiani, F. (2019). 
#' Distributions for modelling location, scale, and shape: Using GAMLSS in R. 
#' Chapman and Hall/CRC.
#' 
#' @seealso 
#' \code{\link{fqZABNB}} for the quantile function.
#' 
#' \code{\link{frBNB}} for the base BNB distribution without zero adjustment.
#' 
#' \code{\link{frZIBNB}} for the zero inflated version.
#' 
#' \code{\link[gamlss.dist]{rZABNB}} for the original implementation.
#' 
#' \code{\link[dqrng]{dqrunif}} for the high-quality random number generator used.
#' 
#' @export
#' @importFrom dqrng dqrunif
frZABNB <- function(n, mu = 1, sigma = 1, nu = 1, tau = 0.1) {
    if (any(n <= 0)) {
        stop(paste("n must be a positive integer", "\n", ""))
    }
    n <- ceiling(n)
    p <- dqrng::dqrunif(n)
    r <- fqZABNB(p, mu = mu, sigma = sigma, nu = nu, tau = tau)
    r
}

#' Random Generation for Double Poisson (DPO) Distribution
#' 
#' Generates random deviates from the Double Poisson distribution using 
#' high-quality pseudo-random number generation via the dqrng package.
#' 
#' @param n Number of observations to generate. If length(n) > 1, the length 
#'   is taken to be the number required.
#' @param mu Vector of positive mean parameters. Default is 1.
#' @param sigma Vector of positive dispersion parameters. Default is 1.
#' 
#' @details
#' This function generates random variates from the DPO distribution by:
#' \enumerate{
#'   \item Generating high-quality uniform random numbers using \code{dqrng::dqrunif}
#'   \item Applying the inverse CDF transformation using \code{fqDPO}
#' }
#' 
#' The Double Poisson distribution is a discrete distribution that extends 
#' the Poisson distribution by adding a dispersion parameter sigma. It's 
#' particularly useful for modelling count data that exhibits either 
#' overdispersion (sigma > 1) or underdispersion (sigma < 1).
#' 
#' The probability mass function is:
#' \deqn{f(y|\mu,\sigma) = \frac{1}{C(\mu,\sigma)} \sqrt{\frac{1}{2\pi\sigma}} e^{-\frac{\mu}{\sigma}} \frac{e^{y \log y - y}}{y!} e^{\frac{y \log \mu}{\sigma}} e^{\frac{y}{\sigma}} e^{-\frac{y \log y}{\sigma}}}
#' for \eqn{y = 0, 1, 2, \ldots}, \eqn{\mu > 0}, and \eqn{\sigma > 0}, where 
#' \eqn{C(\mu,\sigma)} is a normalizing constant.
#' 
#' Key properties:
#' \itemize{
#'   \item \strong{Mean}: \eqn{E[Y] = \mu}
#'   \item \strong{Variance}: \eqn{Var[Y] = \mu \sigma}
#'   \item \strong{Overdispersion}: When \eqn{\sigma > 1}, variance > mean
#'   \item \strong{Underdispersion}: When \eqn{\sigma < 1}, variance < mean
#'   \item \strong{Poisson limit}: When \eqn{\sigma = 1}, reduces to Poisson(\eqn{\mu})
#' }
#' 
#' The use of \code{dqrng::dqrunif} provides superior random number generation 
#' compared to base R's \code{runif}, with better statistical properties and 
#' significantly faster generation speeds.
#' 
#' @return Vector of random integers from the DPO distribution.
#' 
#' @examples
#' # Generate 100 random values with default parameters (Poisson-like)
#' x <- frDPO(100)
#' table(x)
#' mean(x)  # Should be approximately mu = 1
#' var(x)   # Should be approximately mu * sigma = 1
#' 
#' # Generate with overdispersion (sigma > 1)
#' x_over <- frDPO(1000, mu = 5, sigma = 2)
#' mean(x_over)  # Should be approximately 5
#' var(x_over)   # Should be approximately 10 (5 * 2)
#' 
#' # Generate with underdispersion (sigma < 1)
#' x_under <- frDPO(1000, mu = 5, sigma = 0.5)
#' mean(x_under)  # Should be approximately 5
#' var(x_under)   # Should be approximately 2.5 (5 * 0.5)
#' 
#' # Compare dispersion patterns
#' x_poisson <- rpois(1000, lambda = 5)           # Regular Poisson
#' x_dpo_over <- frDPO(1000, mu = 5, sigma = 2)   # Overdispersed
#' x_dpo_under <- frDPO(1000, mu = 5, sigma = 0.5)  # Underdispersed
#' 
#' var(x_poisson)    # ≈ 5 (variance = mean)
#' var(x_dpo_over)   # > 5 (overdispersed)
#' var(x_dpo_under)  # < 5 (underdispersed)
#' 
#' # Vector of parameters (recycling applies)
#' x <- frDPO(10, mu = c(2, 4), sigma = c(0.8, 1.2))
#' 
#' @references
#' Rigby, R.A., Stasinopoulos, D.M., Heller, G.Z., and De Bastiani, F. (2019). 
#' Distributions for modelling location, scale, and shape: Using GAMLSS in R. 
#' Chapman and Hall/CRC.
#' 
#' Efron, B. (1986). Double exponential families and their use in generalized 
#' linear regression. Journal of the American Statistical Association, 81(395), 709-721.
#' 
#' @seealso 
#' \code{\link{fdDPO}}, \code{\link{fpDPO}}, \code{\link{fqDPO}} for 
#' other DPO distribution functions.
#' 
#' \code{\link[gamlss.dist]{rDPO}} for the original implementation.
#' 
#' \code{\link[dqrng]{dqrunif}} for the high-quality random number generator used.
#' 
#' @export
#' @importFrom dqrng dqrunif
frDPO <- function(n, mu = 1, sigma = 1) {
    if (any(n <= 0)) {
        stop(paste("n must be a positive integer", "\n", ""))
    }
    n <- ceiling(n)
    p <- dqrng::dqrunif(n)
    r <- fqDPO(p, mu = mu, sigma = sigma)
    r
}

#' Random Generation for Delaporte Distribution
#' 
#' Generates random deviates from the Delaporte distribution using high-quality 
#' pseudo-random number generation via the dqrng package.
#' 
#' @param n Number of observations to generate. If length(n) > 1, the length 
#'   is taken to be the number required.
#' @param mu Vector of positive mean parameters. Default is 1.
#' @param sigma Vector of positive dispersion parameters. Default is 1.
#' @param nu Vector of parameters between 0 and 1. Default is 0.5.
#' 
#' @details
#' This function generates random variates from the Delaporte distribution by:
#' \enumerate{
#'   \item Generating high-quality uniform random numbers using \code{dqrng::dqrunif}
#'   \item Applying the inverse CDF transformation using \code{fqDEL}
#' }
#' 
#' The Delaporte distribution is a discrete distribution that can be expressed 
#' as a compound Poisson distribution where the intensity parameter follows 
#' a gamma distribution. It arises as the convolution of a Poisson distribution 
#' with a negative binomial distribution, making it particularly useful for 
#' modelling overdispersed count data.
#' 
#' The Delaporte distribution can be parameterized as the sum of:
#' \itemize{
#'   \item A Poisson random variable with parameter \eqn{\mu \nu}
#'   \item A negative binomial random variable with mean \eqn{\mu(1-\nu)} and 
#'         dispersion related to \eqn{\sigma}
#' }
#' 
#' Key properties:
#' \itemize{
#'   \item \strong{Mean}: \eqn{E[Y] = \mu}
#'   \item \strong{Variance}: \eqn{Var[Y] = \mu + \mu^2 \sigma (1-\nu)}
#'   \item \strong{Flexibility}: Can model a wide range of count distributions
#'   \item \strong{Overdispersion}: Always overdispersed relative to Poisson
#'   \item \strong{Special cases}: 
#'     \itemize{
#'       \item When \eqn{\nu \to 1}: approaches Poisson(\eqn{\mu})
#'       \item When \eqn{\nu = 0}: becomes shifted negative binomial
#'     }
#' }
#' 
#' The use of \code{dqrng::dqrunif} provides superior random number generation 
#' compared to base R's \code{runif}, with better statistical properties and 
#' significantly faster generation speeds.
#' 
#' @return Vector of random integers from the Delaporte distribution.
#' 
#' @examples
#' # Generate 100 random values with default parameters
#' x <- frDEL(100)
#' table(x)
#' mean(x)  # Should be approximately mu = 1
#' 
#' # Generate with custom parameters
#' x <- frDEL(1000, mu = 5, sigma = 1.5, nu = 0.3)
#' mean(x)  # Should be approximately 5
#' var(x)   # Should be > 5 (overdispersed)
#' 
#' # Compare different nu values (Poisson component weight)
#' x_high_nu <- frDEL(1000, mu = 5, sigma = 1, nu = 0.8)  # More Poisson-like
#' x_low_nu <- frDEL(1000, mu = 5, sigma = 1, nu = 0.2)   # More NB-like
#' 
#' var(x_high_nu)  # Lower variance (closer to Poisson)
#' var(x_low_nu)   # Higher variance (more overdispersed)
#' 
#' # Study the effect of sigma on overdispersion
#' x_low_sigma <- frDEL(1000, mu = 5, sigma = 0.5, nu = 0.5)
#' x_high_sigma <- frDEL(1000, mu = 5, sigma = 2, nu = 0.5)
#' 
#' var(x_low_sigma)   # Less overdispersion
#' var(x_high_sigma)  # More overdispersion
#' 
#' # Vector of parameters (recycling applies)
#' x <- frDEL(10, mu = c(2, 4), sigma = c(1, 1.5), nu = c(0.3, 0.7))
#' 
#' # Demonstrate flexibility in modelling different count patterns
#' # Low count, high variability
#' x_variable <- frDEL(500, mu = 2, sigma = 3, nu = 0.1)
#' # Moderate count, moderate variability  
#' x_moderate <- frDEL(500, mu = 5, sigma = 1, nu = 0.5)
#' # High count, Poisson-like
#' x_poisson_like <- frDEL(500, mu = 10, sigma = 0.1, nu = 0.9)
#' 
#' @references
#' Rigby, R.A., Stasinopoulos, D.M., Heller, G.Z., and De Bastiani, F. (2019). 
#' Distributions for modelling location, scale, and shape: Using GAMLSS in R. 
#' Chapman and Hall/CRC.
#' 
#' Delaporte, P.J. (1960). Quelques aspects de la classification automatique. 
#' Bulletin de l'Institut International de Statistique, 38, 321-344.
#' 
#' Johnson, N.L., Kemp, A.W., and Kotz, S. (2005). Univariate Discrete 
#' Distributions, 3rd Edition. Hoboken, NJ: Wiley.
#' 
#' @seealso 
#' \code{\link{fdDEL}}, \code{\link{fpDEL}}, \code{\link{fqDEL}} for 
#' other Delaporte distribution functions.
#' 
#' \code{\link[gamlss.dist]{rDEL}} for the original implementation.
#' 
#' \code{\link[dqrng]{dqrunif}} for the high-quality random number generator used.
#' 
#' @export
#' @importFrom dqrng dqrunif
frDEL <- function(n, mu = 1, sigma = 1, nu = 0.5) {
    if (any(n <= 0)) {
        stop(paste("n must be a positive integer", "\n", ""))
    }
    n <- ceiling(n)
    p <- dqrng::dqrunif(n)
    r <- fqDEL(p, mu = mu, sigma = sigma, nu = nu)
    r
}

#' Random Generation for Negative Binomial Type I (NBI) Distribution
#' 
#' Generates random deviates from the Negative Binomial type I distribution 
#' using high-quality pseudo-random number generation via the dqrng package.
#' 
#' @param n Number of observations to generate. If length(n) > 1, the length 
#'   is taken to be the number required.
#' @param mu Vector of positive mean parameters. Default is 1.
#' @param sigma Vector of positive dispersion parameters. Default is 1. For 
#'   sigma < 0.0001, the distribution reduces to Poisson.
#' 
#' @details
#' This function generates random variates from the NBI distribution by:
#' \enumerate{
#'   \item Generating high-quality uniform random numbers using \code{dqrng::dqrunif}
#'   \item Applying the inverse CDF transformation using \code{fqNBI}
#' }
#' 
#' The NBI distribution is a two-parameter discrete distribution that models 
#' count data with overdispersion. It's parameterized by the mean (mu) and 
#' dispersion (sigma) parameters.
#' 
#' Key characteristics:
#' \itemize{
#'   \item Support: {0, 1, 2, ...} (non-negative integers)
#'   \item Mean: mu
#'   \item Variance: mu + sigma * mu^2
#'   \item For sigma → 0, reduces to Poisson distribution
#' }
#' 
#' For very small sigma values (< 0.0001), the function automatically switches 
#' to Poisson random generation for computational efficiency and numerical 
#' stability.
#' 
#' @return Vector of random deviates from the NBI distribution.
#' 
#' @examples
#' # Generate 100 random values with default parameters
#' x <- frNBI(100)
#' hist(x, main = "NBI Random Variates")
#' 
#' # Generate with custom parameters
#' x <- frNBI(1000, mu = 5, sigma = 0.5)
#' summary(x)
#' 
#' # Vector of parameters (recycling applies)
#' x <- frNBI(10, mu = c(1, 2, 3), sigma = 0.8)
#' 
#' # Compare with Poisson when sigma is very small
#' x_nbi <- frNBI(500, mu = 3, sigma = 1e-6)
#' x_pois <- rpois(500, lambda = 3)
#' # Should be very similar
#' 
#' @references
#' Rigby, R.A., Stasinopoulos, D.M., Heller, G.Z., and De Bastiani, F. (2019). 
#' Distributions for modelling location, scale, and shape: Using GAMLSS in R. 
#' Chapman and Hall/CRC.
#' 
#' @seealso 
#' \code{\link{fdNBI}}, \code{\link{fpNBI}}, \code{\link{fqNBI}} for 
#' other NBI distribution functions.
#' 
#' \code{\link{frZINBI}} for the zero-inflated version.
#' 
#' \code{\link{frZANBI}} for the zero-altered version.
#' 
#' \code{\link[gamlss.dist]{rNBI}} for the original implementation.
#' 
#' \code{\link[dqrng]{dqrunif}} for the high-quality random number generator used.
#' 
#' @export
#' @importFrom dqrng dqrunif
frNBI <- function(n, mu = 1, sigma = 1) {
    if (any(n <= 0)) {
        stop(paste("n must be a positive integer", "\n", ""))
    }
    n <- ceiling(n)
    p <- dqrng::dqrunif(n)
    r <- fqNBI(p, mu = mu, sigma = sigma)
    r
}

#' Random Generation for Zero-Inflated Negative Binomial Type I (ZINBI) Distribution
#' 
#' Generates random deviates from the Zero-Inflated Negative Binomial type I 
#' distribution using high-quality pseudo-random number generation via the dqrng package.
#' 
#' @param n Number of observations to generate. If length(n) > 1, the length 
#'   is taken to be the number required.
#' @param mu Vector of positive mean parameters. Default is 1.
#' @param sigma Vector of positive dispersion parameters. Default is 1.
#' @param nu Vector of zero-inflation probabilities (0 < nu < 1). Default is 0.1.
#' 
#' @details
#' This function generates random variates from the ZINBI distribution by:
#' \enumerate{
#'   \item Generating high-quality uniform random numbers using \code{dqrng::dqrunif}
#'   \item Applying the inverse CDF transformation using \code{fqZINBI}
#' }
#' 
#' The ZINBI distribution is a three-parameter discrete distribution that models 
#' count data with excess zeros. It's a mixture of a point mass at zero and a 
#' standard NBI distribution.
#' 
#' Key characteristics:
#' \itemize{
#'   \item Support: {0, 1, 2, ...} (non-negative integers)
#'   \item P(X = 0) = nu + (1-nu) * P_NBI(0)
#'   \item P(X = k) = (1-nu) * P_NBI(k) for k > 0
#'   \item Higher proportion of zeros than standard NBI
#' }
#' 
#' @return Vector of random deviates from the ZINBI distribution.
#' 
#' @examples
#' # Generate 100 random values with default parameters
#' x <- frZINBI(100)
#' hist(x, main = "ZINBI Random Variates")
#' 
#' # Generate with custom parameters
#' x <- frZINBI(1000, mu = 5, sigma = 0.5, nu = 0.2)
#' summary(x)
#' 
#' # Compare proportion of zeros with regular NBI
#' x_nbi <- frNBI(1000, mu = 2, sigma = 1)
#' x_zinbi <- frZINBI(1000, mu = 2, sigma = 1, nu = 0.3)
#' mean(x_nbi == 0)    # Proportion of zeros in NBI
#' mean(x_zinbi == 0)  # Should be higher in ZINBI
#' 
#' @references
#' Rigby, R.A., Stasinopoulos, D.M., Heller, G.Z., and De Bastiani, F. (2019). 
#' Distributions for modelling location, scale, and shape: Using GAMLSS in R. 
#' Chapman and Hall/CRC.
#' 
#' @seealso 
#' \code{\link{fdZINBI}}, \code{\link{fpZINBI}}, \code{\link{fqZINBI}} for 
#' other ZINBI distribution functions.
#' 
#' \code{\link{frNBI}} for the standard NBI distribution.
#' 
#' \code{\link{frZANBI}} for the zero-altered version.
#' 
#' \code{\link[gamlss.dist]{rZINBI}} for the original implementation.
#' 
#' \code{\link[dqrng]{dqrunif}} for the high-quality random number generator used.
#' 
#' @export
#' @importFrom dqrng dqrunif
frZINBI <- function(n, mu = 1, sigma = 1, nu = 0.1) {
    if (any(n <= 0)) {
        stop(paste("n must be a positive integer", "\n", ""))
    }
    n <- ceiling(n)
    p <- dqrng::dqrunif(n)
    r <- fqZINBI(p, mu = mu, sigma = sigma, nu = nu)
    r
}

#' Random Generation for Zero-Altered Negative Binomial Type I (ZANBI) Distribution
#' 
#' Generates random deviates from the Zero-Altered Negative Binomial type I 
#' distribution using high-quality pseudo-random number generation via the dqrng package.
#' 
#' @param n Number of observations to generate. If length(n) > 1, the length 
#'   is taken to be the number required.
#' @param mu Vector of positive mean parameters. Default is 1.
#' @param sigma Vector of positive dispersion parameters. Default is 1.
#' @param nu Vector of zero-alteration probabilities (0 < nu < 1). Default is 0.1.
#' 
#' @details
#' This function generates random variates from the ZANBI distribution by:
#' \enumerate{
#'   \item Generating high-quality uniform random numbers using \code{dqrng::dqrunif}
#'   \item Applying the inverse CDF transformation using \code{fqZANBI}
#' }
#' 
#' The ZANBI distribution is a three-parameter discrete distribution that models 
#' count data where the probability at zero is altered. Unlike zero-inflation, 
#' zero-alteration modifies the probability mass at zero while keeping the 
#' remaining distribution proportional to the original.
#' 
#' Key characteristics:
#' \itemize{
#'   \item Support: {0, 1, 2, ...} (non-negative integers)
#'   \item P(X = 0) = nu
#'   \item P(X = k) = (1-nu) * P_NBI(k) / (1 - P_NBI(0)) for k > 0
#'   \item Probability at zero is directly controlled by nu
#' }
#' 
#' @return Vector of random deviates from the ZANBI distribution.
#' 
#' @examples
#' # Generate 100 random values with default parameters
#' x <- frZANBI(100)
#' hist(x, main = "ZANBI Random Variates")
#' 
#' # Generate with custom parameters
#' x <- frZANBI(1000, mu = 5, sigma = 0.5, nu = 0.2)
#' summary(x)
#' 
#' # Compare with ZINBI and regular NBI
#' x_nbi <- frNBI(1000, mu = 2, sigma = 1)
#' x_zinbi <- frZINBI(1000, mu = 2, sigma = 1, nu = 0.3)
#' x_zanbi <- frZANBI(1000, mu = 2, sigma = 1, nu = 0.3)
#' c(mean(x_nbi == 0), mean(x_zinbi == 0), mean(x_zanbi == 0))
#' 
#' @references
#' Rigby, R.A., Stasinopoulos, D.M., Heller, G.Z., and De Bastiani, F. (2019). 
#' Distributions for modelling location, scale, and shape: Using GAMLSS in R. 
#' Chapman and Hall/CRC.
#' 
#' @seealso 
#' \code{\link{fdZANBI}}, \code{\link{fpZANBI}}, \code{\link{fqZANBI}} for 
#' other ZANBI distribution functions.
#' 
#' \code{\link{frNBI}} for the standard NBI distribution.
#' 
#' \code{\link{frZINBI}} for the zero-inflated version.
#' 
#' \code{\link[gamlss.dist]{rZANBI}} for the original implementation.
#' 
#' \code{\link[dqrng]{dqrunif}} for the high-quality random number generator used.
#' 
#' @export
#' @importFrom dqrng dqrunif
frZANBI <- function(n, mu = 1, sigma = 1, nu = 0.1) {
    if (any(n <= 0)) {
        stop(paste("n must be a positive integer", "\n", ""))
    }
    n <- ceiling(n)
    p <- dqrng::dqrunif(n)
    r <- fqZANBI(p, mu = mu, sigma = sigma, nu = nu)
    r
}

#' Random Generation for Sichel Distribution
#' 
#' Generates random deviates from the Sichel distribution using high-quality 
#' pseudo-random number generation via the dqrng package.
#' 
#' @param n Number of observations to generate. If length(n) > 1, the length 
#'   is taken to be the number required.
#' @param mu Vector of positive mean parameters. Default is 1.
#' @param sigma Vector of positive dispersion parameters. Default is 1.
#' @param nu Vector of shape parameters (real values). Default is -0.5.
#' 
#' @details
#' This function generates random variates from the Sichel distribution by:
#' \enumerate{
#'   \item Generating high-quality uniform random numbers using \code{dqrng::dqrunif}
#'   \item Applying the inverse CDF transformation using \code{fqSICHEL}
#' }
#' 
#' The Sichel distribution is a three-parameter discrete distribution that 
#' extends the Poisson-inverse Gaussian distribution. It's particularly useful 
#' for modelling count data with varying levels of overdispersion and can 
#' accommodate a wide range of distributional shapes.
#' 
#' The probability mass function is:
#' \deqn{f(y|\mu,\sigma,\nu)= \frac{(\mu/c)^y K_{y+\nu}(\alpha)}{y!(\alpha \sigma)^{y+\nu} K_\nu(\frac{1}{\sigma})}}
#' for \eqn{y=0,1,2,...}, \eqn{\mu>0}, \eqn{\sigma>0} and \eqn{-\infty<\nu<\infty}
#' where \eqn{\alpha^2= 1/\sigma^2 +2*\mu/\sigma}, 
#' \eqn{c=K_{\nu+1}(1/\sigma)/K_{\nu}(1/\sigma)}, and 
#' \eqn{K_{\lambda}(t)} is the modified Bessel function of the third kind.
#' 
#' Key characteristics:
#' \itemize{
#'   \item Support: {0, 1, 2, ...} (non-negative integers)
#'   \item Mean: mu
#'   \item Very flexible in modelling different count patterns
#'   \item Can handle both light and heavy-tailed distributions
#'   \item Special cases include Poisson-inverse Gaussian when specific parameters
#' }
#' 
#' The use of \code{dqrng::dqrunif} provides superior random number generation 
#' compared to base R's \code{runif}, with better statistical properties and 
#' significantly faster generation speeds.
#' 
#' @return Vector of random integers from the Sichel distribution.
#' 
#' @examples
#' # Generate 100 random values with default parameters
#' x <- frSICHEL(100)
#' table(x)
#' mean(x)  # Should be approximately mu = 1
#' 
#' # Generate with custom parameters
#' x <- frSICHEL(1000, mu = 5, sigma = 1.5, nu = -0.5)
#' summary(x)
#' mean(x)  # Should be approximately 5
#' 
#' # Explore different nu values (shape parameter)
#' x_neg <- frSICHEL(1000, mu = 3, sigma = 1, nu = -1)   # Negative nu
#' x_zero <- frSICHEL(1000, mu = 3, sigma = 1, nu = 0)   # Zero nu
#' x_pos <- frSICHEL(1000, mu = 3, sigma = 1, nu = 1)    # Positive nu
#' 
#' # Compare variance patterns
#' var(x_neg)
#' var(x_zero) 
#' var(x_pos)
#' 
#' # Study effect of sigma on dispersion
#' x_low_sigma <- frSICHEL(1000, mu = 4, sigma = 0.5, nu = -0.5)
#' x_high_sigma <- frSICHEL(1000, mu = 4, sigma = 2, nu = -0.5)
#' 
#' var(x_low_sigma)   # Lower dispersion
#' var(x_high_sigma)  # Higher dispersion
#' 
#' # Vector of parameters (recycling applies)
#' x <- frSICHEL(10, mu = c(2, 4), sigma = c(1, 1.5), nu = c(-0.5, 0.5))
#' 
#' # Generate data with different distributional shapes
#' # Heavy-tailed count data
#' x_heavy <- frSICHEL(500, mu = 5, sigma = 2, nu = -2)
#' # Light-tailed count data  
#' x_light <- frSICHEL(500, mu = 5, sigma = 0.8, nu = 1)
#' 
#' # Compare tail behaviour
#' quantile(x_heavy, c(0.9, 0.95, 0.99))
#' quantile(x_light, c(0.9, 0.95, 0.99))
#' 
#' @references
#' Rigby, R.A., Stasinopoulos, D.M., Heller, G.Z., and De Bastiani, F. (2019). 
#' Distributions for modelling location, scale, and shape: Using GAMLSS in R. 
#' Chapman and Hall/CRC.
#' 
#' Stein, G. Z., Zucchini, W. and Juritz, J. M. (1987). Parameter
#' Estimation of the Sichel Distribution and its Multivariate Extension.
#' Journal of American Statistical Association, 82, 938-944.
#' 
#' @seealso 
#' \code{\link{fdSICHEL}}, \code{\link{fpSICHEL}}, \code{\link{fqSICHEL}} for 
#' other Sichel distribution functions.
#' 
#' \code{\link{frZISICHEL}} for the zero-inflated version.
#' 
#' \code{\link[gamlss.dist]{rSICHEL}} for the original implementation.
#' 
#' \code{\link[dqrng]{dqrunif}} for the high-quality random number generator used.
#' 
#' @export
#' @importFrom dqrng dqrunif
frSICHEL <- function(n, mu = 1, sigma = 1, nu = -0.5) {
    if (any(n <= 0)) {
        stop(paste("n must be a positive integer", "\n", ""))
    }
    n <- ceiling(n)
    p <- dqrng::dqrunif(n)
    r <- fqSICHEL(p, mu = mu, sigma = sigma, nu = nu)
    r
}

#' Random Generation for Zero-Inflated Sichel Distribution
#' 
#' Generates random deviates from the Zero-Inflated Sichel distribution using 
#' high-quality pseudo-random number generation via the dqrng package.
#' 
#' @param n Number of observations to generate. If length(n) > 1, the length 
#'   is taken to be the number required.
#' @param mu Vector of positive mean parameters. Default is 1.
#' @param sigma Vector of positive dispersion parameters. Default is 1.
#' @param nu Vector of shape parameters (real values). Default is -0.5.
#' @param tau Vector of zero-inflation probabilities (0 < tau < 1). Default is 0.1.
#' 
#' @details
#' This function generates random variates from the Zero-Inflated Sichel distribution by:
#' \enumerate{
#'   \item Generating high-quality uniform random numbers using \code{dqrng::dqrunif}
#'   \item Applying the inverse CDF transformation using \code{fqZISICHEL}
#' }
#' 
#' The Zero-Inflated Sichel distribution is a four-parameter discrete distribution 
#' that extends the Sichel distribution to handle excess zeros. It's a mixture 
#' of a point mass at zero and a standard Sichel distribution, making it 
#' particularly useful for modelling count data with structural zeros.
#' 
#' The probability mass function is:
#' \deqn{f(y|\mu,\sigma,\nu,\tau) = \begin{cases}
#' \tau + (1-\tau) \cdot f_{SICHEL}(0|\mu,\sigma,\nu) & \text{if } y = 0 \\
#' (1-\tau) \cdot f_{SICHEL}(y|\mu,\sigma,\nu) & \text{if } y > 0
#' \end{cases}}
#' where \eqn{f_{SICHEL}} is the Sichel probability mass function.
#' 
#' Key characteristics:
#' \itemize{
#'   \item Support: {0, 1, 2, ...} (non-negative integers)
#'   \item Higher proportion of zeros than standard Sichel
#'   \item Combines flexibility of Sichel with zero-inflation capability
#'   \item Useful for modelling count processes with excess zeros
#' }
#' 
#' The use of \code{dqrng::dqrunif} provides superior random number generation 
#' compared to base R's \code{runif}, with better statistical properties and 
#' significantly faster generation speeds.
#' 
#' @return Vector of random integers from the Zero-Inflated Sichel distribution.
#' 
#' @examples
#' # Generate 100 random values with default parameters
#' x <- frZISICHEL(100)
#' table(x)
#' sum(x == 0) / length(x)  # Proportion of zeros
#' 
#' # Generate with custom parameters - moderate zero inflation
#' x <- frZISICHEL(1000, mu = 5, sigma = 1.5, nu = -0.5, tau = 0.2)
#' mean(x)  # Mean will be affected by zero inflation
#' sum(x == 0) / length(x)  # Should be > 0.2 due to natural + inflated zeros
#' 
#' # Compare with regular Sichel (no zero inflation)
#' x_sichel <- frSICHEL(1000, mu = 5, sigma = 1.5, nu = -0.5)
#' x_zi_sichel <- frZISICHEL(1000, mu = 5, sigma = 1.5, nu = -0.5, tau = 0.3)
#' 
#' sum(x_sichel == 0) / length(x_sichel)       # Natural zeros only
#' sum(x_zi_sichel == 0) / length(x_zi_sichel) # Natural + inflated zeros
#' 
#' # Study effect of tau on zero inflation
#' x_low_tau <- frZISICHEL(1000, mu = 3, sigma = 1, nu = 0, tau = 0.1)
#' x_high_tau <- frZISICHEL(1000, mu = 3, sigma = 1, nu = 0, tau = 0.4)
#' 
#' sum(x_low_tau == 0) / length(x_low_tau)   # Lower zero proportion
#' sum(x_high_tau == 0) / length(x_high_tau) # Higher zero proportion
#' 
#' # Vector of parameters (recycling applies)
#' x <- frZISICHEL(10, mu = c(2, 4), sigma = 1.5, nu = c(-0.5, 0.5), tau = c(0.1, 0.2))
#' 
#' # Model data with high zero inflation and flexible count part
#' x <- frZISICHEL(1000, mu = 8, sigma = 2, nu = -1, tau = 0.4)
#' 
#' # Examine the structure
#' table(x[x <= 10])  # Focus on lower counts
#' sum(x == 0) / length(x)  # Zero proportion
#' mean(x[x > 0])  # Mean of non-zero part
#' 
#' @references
#' Rigby, R.A., Stasinopoulos, D.M., Heller, G.Z., and De Bastiani, F. (2019). 
#' Distributions for modelling location, scale, and shape: Using GAMLSS in R. 
#' Chapman and Hall/CRC.
#' 
#' @seealso 
#' \code{\link{fdZISICHEL}}, \code{\link{fpZISICHEL}}, \code{\link{fqZISICHEL}} for 
#' other Zero-Inflated Sichel distribution functions.
#' 
#' \code{\link{frSICHEL}} for the standard Sichel distribution.
#' 
#' \code{\link[gamlss.dist]{rZISICHEL}} for the original implementation.
#' 
#' \code{\link[dqrng]{dqrunif}} for the high-quality random number generator used.
#' 
#' @export
#' @importFrom dqrng dqrunif
frZISICHEL <- function(n, mu = 1, sigma = 1, nu = -0.5, tau = 0.1) {
    if (any(n <= 0)) {
        stop(paste("n must be a positive integer", "\n", ""))
    }
    n <- ceiling(n)
    p <- dqrng::dqrunif(n)
    r <- fqZISICHEL(p, mu = mu, sigma = sigma, nu = nu, tau = tau)
    r
}
