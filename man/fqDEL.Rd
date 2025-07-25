% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{fqDEL}
\alias{fqDEL}
\title{Quantile Function for the Delaporte Distribution}
\usage{
fqDEL(p, mu, sigma, nu, lower_tail = TRUE, log_p = FALSE)
}
\arguments{
\item{p}{Vector of probabilities.}

\item{mu}{Vector of mu (location/mean) parameters (positive).}

\item{sigma}{Vector of sigma (scale) parameters (positive).}

\item{nu}{Vector of nu (shape) parameters (positive).}

\item{lower_tail}{Logical; if TRUE (default), probabilities are P[X ≤ x],
otherwise P[X > x].}

\item{log_p}{Logical; if TRUE, probabilities p are given as log(p).}
}
\value{
Vector of quantiles corresponding to the given probabilities.
}
\description{
Computes quantiles of the Delaporte distribution, a compound distribution of
Poisson and shifted negative binomial.
}
\details{
The Delaporte distribution is a three-parameter discrete distribution
defined as the convolution of a Poisson distribution with mean \code{mu}
and a shifted negative binomial distribution with parameters related to
\code{sigma} and \code{nu}.

This implementation uses an optimised binary search algorithm with
SIMD acceleration where available, and includes intelligent caching
of intermediate CDF calculations for improved performance with repeated
quantile computations.

Parameter recycling is performed automatically - all parameter vectors
are recycled to the length of the longest vector.
}
\note{
This function is based on the Delaporte distribution implementation from
the \pkg{gamlss.dist} package by Mikis Stasinopoulos, Robert Rigby,
Calliope Akantziliotou, Vlasios Voudouris, and Fernanda De Bastiani.
The original gamlss.dist implementation is acknowledged with gratitude.
}
\section{Parameter Validation}{

- \code{p} must be in [0,1] for \code{log_p = FALSE}, or in (-Inf, 0] for \code{log_p = TRUE}
- \code{mu}, \code{sigma}, \code{nu} must all be positive
- Invalid parameters result in \code{NA} values in the output
}

\examples{
# Basic quantile computation
fqDEL(c(0.25, 0.5, 0.75), mu=5, sigma=1, nu=0.2)

# With parameter recycling
fqDEL(0.5, mu=c(1,5,10), sigma=c(0.5,1,2), nu=c(0.1,0.2,0.9))

# Using log probabilities
fqDEL(log(c(0.25, 0.5, 0.75)), mu=5, sigma=1, nu=0.2, log_p=TRUE)

# Upper tail probabilities
fqDEL(c(0.25, 0.5, 0.75), mu=5, sigma=1, nu=0.2, lower_tail=FALSE)

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
\code{\link{fdDEL}}, \code{\link{fpDEL}}
}
\author{
Christos Kypraios [aut, cre], based on gamlss.dist by Mikis Stasinopoulos,
Robert Rigby, Calliope Akantziliotou, Vlasios Voudouris, Fernanda De Bastiani
}
