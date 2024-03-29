% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_functions.R
\name{resample}
\alias{resample}
\title{`resample` Gives sample of from the elements of \code{`x`} of the specified size. Both size and \code{`x`} has to be integers}
\usage{
resample(x, ...)
}
\arguments{
\item{x}{A vector to be sampled}

\item{...}{Eventual condition for the vectorisation}
}
\value{
Sample(s) from the \code{`x`} vector, according to eventual conditions provided
}
\description{
`resample` Gives sample of from the elements of \code{`x`} of the specified size. Both size and \code{`x`} has to be integers
}
\examples{
x <- 1:10
resample(x[x >  8]) # length 2
resample(x[x >  9]) # length 1
resample(x[x > 10]) # length 0
}
