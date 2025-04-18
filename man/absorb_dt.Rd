% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dt_ops.R
\name{absorb_dt}
\alias{absorb_dt}
\title{Absorb Columns from One data.table into Another}
\usage{
absorb_dt(dt_x, dt_i, on = ".NATURAL", exclude_col = NULL, verbose = FALSE)
}
\arguments{
\item{dt_x}{A data.table that will have its columns updated.}

\item{dt_i}{A data.table from which columns are to be copied.}

\item{on}{A character vector specifying the join keys. If set to ".NATURAL", the keys are automatically determined as the common columns between `dt_x` and `dt_i`, excluding any specified in `exclude_col`.}

\item{exclude_col}{An optional character vector of column names to exclude from the automatically determined join keys.}

\item{verbose}{Logical. If TRUE, outputs messages indicating which columns in `dt_x` have been replaced by columns from `dt_i`.}
}
\value{
Returns `dt_x` invisibly after modifying it in place by copying columns from `dt_i`.
}
\description{
This function copies columns from a secondary data.table (`dt_i`) into a primary data.table (`dt_x`) based on matching key columns. If the join key is set to ".NATURAL" (the default), the function automatically determines the keys as the intersection of column names from `dt_x` and `dt_i`, excluding any specified in `exclude_col`. Any non-key columns in `dt_x` that share names with columns in `dt_i` are replaced by the corresponding columns from `dt_i`. This is achieved by temporarily renaming the columns in `dt_i`, performing the join, and then restoring the original column names.
}
\examples{
library(data.table)
dt_x <- data.table(a = 1:5, b = 1:5, c = 5:9, d = 5:1, e = 1:5)
dt_i <- data.table(a = 1:5, b = 1:5, c = 1:5, d = 1:5)
absorb_dt(dt_x, dt_i, on = c("a", "b"))
}
