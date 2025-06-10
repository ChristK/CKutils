## CKutils: an R package with some utility functions I use regularly
## Copyright (C) 2022  Chris Kypridemos

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

#' CKutils: A package with utility functions I use regularly.
#'
#' This package contains some functions I use regularly when I develop simulation models.
#
#' @keywords internal
#' @docType package
#' @author Chris Kypridemos
#' @name CKutils

#' @import data.table fst

#' @importFrom methods as
#' @importFrom graphics abline legend lines par plot title
#' @importFrom stats density .checkMFClasses delete.response model.frame model.matrix plogis predict

## usethis namespace: start
#' @importFrom data.table :=
#' @importFrom data.table .BY
#' @importFrom data.table .EACHI
#' @importFrom data.table .GRP
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table .NGRP
#' @importFrom data.table .SD
#' @importFrom Rcpp sourceCpp
#' @importFrom utils installed.packages
#' @importFrom remotes install_local
#' @importFrom roxygen2 roxygenise
#' @useDynLib CKutils, .registration = TRUE

# Make sure data.table knows we know we're using it
.datatable.aware = TRUE

# Initialize data.table API when package loads
.onLoad <- function(libname, pkgname) {
  # Initialize the data.table API for C++ functions
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("data.table package is required but not available")
  }
  
  # Explicitly load data.table to ensure C API is available
  library(data.table)
  
  # On Windows, we might need to explicitly initialize data.table internals
  if (.Platform$OS.type == "windows") {
    # Force data.table initialization by accessing a data.table function
    tryCatch({
      dt_test <- data.table::data.table(x = 1)
      invisible(dt_test[, get("x")])
    }, error = function(e) {
      warning("Could not initialize data.table on Windows: ", e$message)
    })
  }
}

# Prevent R CMD check from complaining about the use of pipe expressions
# standard data.table variables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", ".I", ".N", ".SD"), utils::packageName())
}

## usethis namespace: end
NULL
