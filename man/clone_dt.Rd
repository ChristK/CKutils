% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dt_ops.R
\name{clone_dt}
\alias{clone_dt}
\title{Clone a data.table}
\usage{
clone_dt(dtb, times, idcol = TRUE)
}
\arguments{
\item{dtb}{A data.table to be cloned}

\item{times}{The number of cloning iterations}

\item{idcol}{= TRUE Creates a new column containing the id of each data.table iteration}
}
\value{
A data.table binding the original one and the new iterations with a column for the id of each iteration, invisibly
}
\description{
`clone_dt` clones a data.table and binds the copies at the bottom of
the original data.table. It also creates an column named \code{`.id`}
to identify each iteration. The keys of the input data.table are retained.
}
\examples{
library(data.table)
library(CKutils)
x <- c(1, 5, 3, 6, 4, 2, 9, 8, 7)
dtb <- data.table(x)
clone_dt(dtb, 3, idcol = TRUE)
}
