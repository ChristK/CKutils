% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{fqDPO}
\alias{fqDPO}
\title{Quantile Function for the DPO Distribution}
\usage{
fqDPO(p, mu, sigma, lower_tail = TRUE, log_p = FALSE, max_value = 0L)
}
\arguments{
\item{p}{Vector of probabilities.}

\item{mu}{Vector of mu (location/mean) parameters (positive).}

\item{sigma}{Vector of sigma (dispersion) parameters (positive).}

\item{lower_tail}{Logical; if TRUE (default), probabilities are P[X ≤ x],
otherwise P[X > x].}

\item{log_p}{Logical; if TRUE, probabilities p are given as log(p).}

\item{max_value}{Maximum value to search for quantiles (for performance tuning).}
}
\value{
Vector of quantiles corresponding to the given probabilities.
}
\description{
Computes quantiles of the DPO (Double Poisson) distribution, a discrete
distribution that extends the Poisson distribution with a dispersion parameter.
}
\details{
The DPO distribution is a two-parameter discrete distribution that reduces
to the Poisson distribution when sigma = 1. This implementation uses an
optimised search algorithm with SIMD acceleration where available, and
includes intelligent caching of intermediate CDF calculations for improved
performance with repeated quantile computations.

Parameter recycling is performed automatically - all parameter vectors
are recycled to the length of the longest vector.
}
\note{
This function is based on the DPO distribution implementation from
the \pkg{gamlss.dist} package by Mikis Stasinopoulos, Robert Rigby,
and colleagues. The original gamlss.dist implementation is acknowledged
with gratitude.
}
\section{Parameter Validation}{

- \code{p} must be in [0,1] for \code{log_p = FALSE}, or in (-Inf, 0] for \code{log_p = TRUE}
- \code{mu}, \code{sigma} must both be positive
- Invalid parameters result in \code{NA} values in the output
}

\examples{
# Basic quantile computation
fqDPO(c(0.25, 0.5, 0.75), mu=5, sigma=1)

# With parameter recycling
fqDPO(0.5, mu=c(1,5,10), sigma=c(0.5,1,2))

# Using log probabilities
fqDPO(log(c(0.25, 0.5, 0.75)), mu=5, sigma=1, log_p=TRUE)

# Upper tail probabilities
fqDPO(c(0.25, 0.5, 0.75), mu=5, sigma=1, lower_tail=FALSE)

}
\references{
Rigby, R. A. and Stasinopoulos D. M. (2005). Generalized additive models
for location, scale and shape,(with discussion), \emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R}, Chapman and Hall/CRC.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location
scale and shape (GAMLSS) in R. \emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007.
}
\seealso{
\code{\link{fdDPO}}, \code{\link{fpDPO}}
}
\author{
Chris Kypridemos [aut, cre], based on gamlss.dist by Mikis Stasinopoulos,
Robert Rigby, and colleagues
}
