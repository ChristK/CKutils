% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_functions.R
\name{symdiff}
\alias{symdiff}
\title{Calculate Symmetric Difference Between Two Sets}
\usage{
symdiff(x, y)
}
\arguments{
\item{x}{A vector (numeric, character, logical, etc.)}

\item{y}{A vector of the same type as x}
}
\value{
A vector containing elements that are in either x or y, but not in both
}
\description{
`symdiff` returns the symmetric difference between two vectors. The symmetric
difference consists of elements that are in either x or y, but not in both.
This is equivalent to the union of the two sets minus their intersection.
}
\examples{
# With numeric vectors
symdiff(c(1, 2, 3, 4), c(3, 4, 5, 6))  # Returns c(1, 2, 5, 6)

# With character vectors
symdiff(c("a", "b", "c"), c("b", "c", "d"))  # Returns c("a", "d")

# With logical vectors
symdiff(c(TRUE, FALSE), c(FALSE))  # Returns TRUE

# Empty result when sets are identical
symdiff(c(1, 2, 3), c(1, 2, 3))  # Returns integer(0)
}
