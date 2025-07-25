% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R, R/rng_distr.R
\name{frZINBI}
\alias{frZINBI}
\title{Zero-Inflated Negative Binomial Type I Distribution Random Generation}
\usage{
frZINBI(n, mu = 1, sigma = 1, nu = 0.1)

frZINBI(n, mu = 1, sigma = 1, nu = 0.1)
}
\arguments{
\item{n}{Number of observations to generate. If length(n) > 1, the length 
is taken to be the number required.}

\item{mu}{Vector of positive mean parameters. Default is 1.}

\item{sigma}{Vector of positive dispersion parameters. Default is 1.}

\item{nu}{Vector of zero-inflation probabilities (0 < nu < 1). Default is 0.1.}
}
\value{
A numeric vector of random variates.

Vector of random deviates from the ZINBI distribution.
}
\description{
Random generation for the Zero-Inflated Negative Binomial type I (ZINBI)
distribution with parameters mu (mean), sigma (dispersion), and nu (zero-inflation probability).

Generates random deviates from the Zero-Inflated Negative Binomial type I 
distribution using high-quality pseudo-random number generation via the dqrng package.
}
\details{
Random variates are generated using a mixture approach: with probability \eqn{\nu}
the value is 0, and with probability \eqn{1-\nu} the value is drawn from the
standard NBI distribution.

This function generates random variates from the ZINBI distribution by:
\enumerate{
  \item Generating high-quality uniform random numbers using \code{dqrng::dqrunif}
  \item Applying the inverse CDF transformation using \code{fqZINBI}
}

The ZINBI distribution is a three-parameter discrete distribution that models 
count data with excess zeros. It's a mixture of a point mass at zero and a 
standard NBI distribution.

Key characteristics:
\itemize{
  \item Support: 0, 1, 2, ... (non-negative integers)
  \item P(X = 0) = nu + (1-nu) * P_NBI(0)
  \item P(X = k) = (1-nu) * P_NBI(k) for k > 0
  \item Higher proportion of zeros than standard NBI
}
}
\examples{
# Generate random variates
frZINBI(10, mu=2, sigma=1, nu=0.1)

# Vector inputs with recycling
frZINBI(5, mu=c(1,2), sigma=0.5, nu=0.1)

# Generate 100 random values with default parameters
x <- frZINBI(100)
hist(x, main = "ZINBI Random Variates")

# Generate with custom parameters
x <- frZINBI(1000, mu = 5, sigma = 0.5, nu = 0.2)
summary(x)

# Compare proportion of zeros with regular NBI
x_nbi <- frNBI(1000, mu = 2, sigma = 1)
x_zinbi <- frZINBI(1000, mu = 2, sigma = 1, nu = 0.3)
mean(x_nbi == 0)    # Proportion of zeros in NBI
mean(x_zinbi == 0)  # Should be higher in ZINBI

}
\references{
Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019) 
Distributions for modelling location, scale, and shape: Using GAMLSS in R, 
Chapman and Hall/CRC.

Rigby, R.A., Stasinopoulos, D.M., Heller, G.Z., and De Bastiani, F. (2019). 
Distributions for modelling location, scale, and shape: Using GAMLSS in R. 
Chapman and Hall/CRC.
}
\seealso{
\code{\link{fdZINBI}}, \code{\link{fpZINBI}}, \code{\link{fqZINBI}} for 
other ZINBI distribution functions.

\code{\link{frNBI}} for the standard NBI distribution.

\code{\link{frZANBI}} for the zero-altered version.

\code{\link[gamlss.dist]{rZINBI}} for the original implementation.

\code{\link[dqrng]{dqrunif}} for the high-quality random number generator used.
}
