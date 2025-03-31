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




# lookup_tbl = CJ(b=1:4, a = factor(letters[1:4]))[, c:=rep(1:4, 4)]
# tbl = data.table(b=0:5, a = factor(letters[1:4]))

# lookup_dt ----
#' Perform Table Lookup and Merge
#'
#' Executes a lookup operation between two data.tables by matching on common key columns.
#' The lookup is conducted on key columns (which are factors or integers starting from 1) and maps each
#' unique combination to a corresponding index. The lookup values are then merged into the main table
#' or returned separately based on the \code{merge} parameter.
#'
#' @param tbl The main data.table on which the lookup operation is performed.
#' @param lookup_tbl A data.table containing the lookup values, with key columns matching those in \code{tbl}.
#' @param merge Logical. If \code{TRUE}, the lookup results are merged into \code{tbl}; if \code{FALSE}, only the lookup results are returned.
#' @param exclude_col A character vector specifying column names to exclude from the lookup keys.
#' @param check_lookup_tbl_validity Logical. If \code{TRUE}, validates the structure of \code{lookup_tbl}.
#'
#' @return A data.table. When \code{merge = TRUE}, \code{tbl} is returned with additional lookup columns;
#' otherwise, a data.table containing only the lookup results is returned.
#'
#' @export
lookup_dt <- function(tbl,
                      lookup_tbl,
                      merge = TRUE,
                      exclude_col = NULL,
                      check_lookup_tbl_validity = FALSE) {
  # Ensure both inputs are data.tables
  if (!is.data.table(tbl)) {
    stop("tbl must be a data.table")
  }
  if (!is.data.table(lookup_tbl)) {
    stop("lookup_tbl must be a data.table")
  }

  # Identify common key columns, excluding any specified columns
  nam_x <- names(tbl)
  nam_i <- names(lookup_tbl)
  on <- sort(setdiff(intersect(nam_x, nam_i), exclude_col))
  # Prioritize 'year' if present
  on <- on[order(match(on, "year"))]

  # Identify value columns from lookup_tbl (columns not used as keys)
  return_cols_nam <- setdiff(nam_i, on)
  return_cols <- which(nam_i %in% return_cols_nam)

  if (length(on) == 0L) {
    stop("No common keys found between tbl and lookup_tbl")
  }
  if (length(on) == length(nam_i)) {
    stop("No value columns identified in lookup_tbl. Consider using the 'exclude_col' argument.")
  }

  # Optionally validate lookup_tbl structure
  if (check_lookup_tbl_validity) {
    is_valid_lookup_tbl(lookup_tbl, on)
  }

  # Prepare lookup_tbl by setting its key to the common columns
  setkeyv(lookup_tbl, cols = on)

  # Initialize cardinality and min_lookup for key columns
  cardinality <- vector("integer", length(on))
  names(cardinality) <- on
  min_lookup <- cardinality

  # Calculate cardinality and minimum lookup values for each key column
  for (j in on) {
    if (is.factor(lookup_tbl[[j]])) {
      lv <- levels(lookup_tbl[[j]])
      if (check_lookup_tbl_validity && !identical(lv, levels(tbl[[j]]))) {
        stop(j, " has different levels in tbl and lookup_tbl!")
      }
      cardinality[[j]] <- length(lv)
      min_lookup[[j]] <- 1L
    } else {
      # For integer keys, assume values are sorted and consecutive
      xmax <- last(lookup_tbl[[j]])
      xmin <- first(lookup_tbl[[j]])
      if (check_lookup_tbl_validity &&
        (min(tbl[[j]], na.rm = TRUE) < xmin || max(tbl[[j]], na.rm = TRUE) > xmax)) {
        message(j, " has rows in tbl without a match in lookup_tbl!")
      }
      cardinality[[j]] <- xmax - xmin + 1L
      min_lookup[[j]] <- xmin
    }
  }

  # Compute the cumulative product of cardinalities (in reverse) for index mapping
  cardinality_prod <- shift(rev(cumprod(rev(cardinality))), -1, fill = 1L)

  # Map each row in tbl to a unique lookup index using the starts_from_1 function
  rownum <- as.integer(starts_from_1_cpp(tbl, on, 1L, min_lookup, cardinality) * cardinality_prod[[1L]])
  if (length(on) > 1L) {
    for (i in 2:length(on)) {
      rownum <- as.integer(rownum -
        (cardinality[[i]] - starts_from_1_cpp(tbl, on, i, min_lookup, cardinality)) * cardinality_prod[[i]])
    }
  }

  # Merge lookup values into tbl or return them separately
  if (merge) {
    tbl[, (return_cols_nam) := dtsubset(lookup_tbl, rownum, return_cols)]
    return(invisible(tbl))
  } else {
    return(invisible(dtsubset(lookup_tbl, rownum, return_cols)))
  }
}


# is_valid_lookup_tbl ----
#' Check Validity of Lookup Table
#'
#' This function verifies that a lookup table meets the required structural conditions
#' for key-based lookups. It checks that key columns are of type integer (or factors stored as integers),
#' that each key column has a consecutive sequence of values, and that the table has the expected
#' number of rows based on all possible combinations of key values.
#'
#' @param lookup_tbl The data.table representing the lookup table.
#' @param keycols A character vector specifying the key columns in the lookup table.
#' @param fixkey Logical. If TRUE, the function will automatically set the key of the lookup table to the provided key columns for best performance; default is FALSE.
#'
#' @return TRUE if the lookup table is valid; otherwise, an error is raised.
#'
#' @export
is_valid_lookup_tbl <- function(lookup_tbl, keycols, fixkey = FALSE) {
  if (!is.data.table(lookup_tbl)) {
    stop("lookup_tbl should be a data.table.")
  }

  if (missing(keycols) || length(keycols) == 0L) {
    stop("keycols argument is missing.")
  }

  # Sort key columns and prioritize 'year' if present
  keycols <- sort(keycols)
  keycols <- keycols[order(match(keycols, "year"))]

  # Ensure unique combinations of key columns
  if (any(duplicated(lookup_tbl, by = keycols))) {
    stop("Lookup table must have a unique combination of key columns.")
  }

  # Compute the expected number of rows based on unique key combinations
  expected_rows <- prod(sapply(keycols, function(j) {
    if (is.integer(lookup_tbl[[j]])) {
      uniqueN(lookup_tbl[[j]])
    } else {
      length(levels(lookup_tbl[[j]]))
    }
  }))

  # Validate each key column
  for (j in keycols) {
    # Check that the column is of type integer (factors are stored as integers)
    if (typeof(lookup_tbl[[j]]) != "integer") {
      stop(paste0("Lookup table key columns must be of type integer (or factor). Column '", j, "' is not integer."))
    }

    # For integer columns, ensure the values form a consecutive sequence
    if (is.integer(lookup_tbl[[j]])) {
      x <- sort(lookup_tbl[[j]])
      if (length(x) > 1 && any(diff(x) > 1L)) {
        stop(paste0("Lookup table key column '", j, "' does not contain consecutive integer values."))
      }
    }

    # Recommend setting the key for best performance if not already set
    if (!identical(key(lookup_tbl), keycols)) {
      message("For best performance, consider setting the key of lookup_tbl to: ", paste(keycols, collapse = ", "))
      if (fixkey) {
        setkeyv(lookup_tbl, keycols)
        message("Key has been set to: ", paste(keycols, collapse = ", "))
      }
    }

    # Verify the lookup table has the expected number of rows
    if (nrow(lookup_tbl) != expected_rows) {
      stop(paste0("Lookup table should have ", expected_rows, " rows based on key combinations, but has ", nrow(lookup_tbl), " rows."))
    }

    return(TRUE)
  }
}


#' Set Lookup Table Key
#'
#' Sets the key columns of a lookup table to optimize performance for lookup operations.
#' The key columns are sorted (with a priority given to "year" if present) and then set
#' as the key for the data.table.
#'
#' @param lookup_tbl A data.table whose key columns are to be set.
#' @param keycols A character vector specifying the key columns to be used.
#'
#' @return The original lookup table (invisibly) with its key set for efficient subsetting.
#'
#' @export
set_lookup_tbl_key <- function(lookup_tbl, keycols) {
  if (!is.data.table(lookup_tbl)) {
    stop("lookup_tbl should be a data.table.")
  }
  if (missing(keycols) || length(keycols) == 0L) {
    stop("keycols argument is missing.")
  }

  # Ensure keycols are sorted and prioritize 'year' if present
  keycols <- sort(keycols)
  keycols <- keycols[order(match(keycols, "year"))]

  # Set the key for best performance
  setkeyv(new_lookup_tbl, keycols)

  return(invisible(lookup_tbl))
}


# # fct_to_int ----
# #' @title Convert Factor to Integer
# #'
# #' @description
# #' Converts a factor to its underlying integer representation. If \code{byref = FALSE}
# #' (default), a copy is made and the conversion occurs on the copy. If \code{byref = TRUE},
# #' the conversion is performed in-place.
# #'
# #' @param x A factor variable to convert.
# #' @param byref Logical. If \code{TRUE}, modifies the object in-place; if \code{FALSE},
# #' works on a copy (default).
# #'
# #' @return An integer vector. If \code{byref = TRUE}, returns the modified object; otherwise,
# #' returns a new vector with class and levels attributes removed.
# #'
# #' @details
# #' This function is especially useful when preparing factor columns for indexing or
# #' performing efficient lookups, where integer values are required.
# #'
# #' @examples
# #' x <- factor(c("a", "b", "c"))
# #' fct_to_int(x)
# #'
# #' @export
# fct_to_int <- function(x, byref = FALSE) {
#   # converts factor to integer
#   if (!byref) x <- copy(x)
#   setattr(x, name = "levels", value = NULL)
#   setattr(x, name = "class", value = NULL)
#   x
# }

# # starts_from_1 ----
# #' Adjust Integer or Factor Column values to Start from 1
# #'
# #' Adjusts the values of an integer or factor column in a data frame so that they begin at 1.
# #' This adjustment is performed by subtracting the minimum expected value (minus one) from each element,
# #' making it ideal for preparing key columns for lookup tables or join operations.
# #'
# #' @param tbl A data frame or data.table containing the target column.
# #' @param on A character vector of column names; the i-th element specifies the column to be adjusted.
# #' @param i An integer index indicating which column (from \code{on}) to adjust.
# #' @param min_lookup A list of minimum expected values for each column in \code{on}.
# #' @param cardinality A list of cardinalities representing the number of distinct values for each column in \code{on}.
# #' @return An integer vector of adjusted values starting at 1. Values outside the expected range are replaced with \code{NA}.
# starts_from_1 <- function(tbl, on, i, min_lookup, cardinality) {
#   coldata <- tbl[[on[[i]]]]
#   minx <- min_lookup[[i]]
#   offset <- minx - 1L

#   if (is.integer(coldata)) {
#     out <- coldata - offset
#     out[out < 1L | out > cardinality[[i]]] <- NA_integer_
#     return(out)
#   } else if (is.factor(coldata)) {
#     return(fct_to_int(coldata) - offset)
#   } else {
#     stop("Column data must be either an integer or a factor.")
#   }
# }


# #' @export
# lookup_dt_r <- function(tbl,
#                       lookup_tbl,
#                       merge = TRUE,
#                       exclude_col = NULL,
#                       check_lookup_tbl_validity = FALSE) {
#   # Ensure both inputs are data.tables
#   if (!is.data.table(tbl)) {
#     stop("tbl must be a data.table")
#   }
#   if (!is.data.table(lookup_tbl)) {
#     stop("lookup_tbl must be a data.table")
#   }

#   # Identify common key columns, excluding any specified columns
#   nam_x <- names(tbl)
#   nam_i <- names(lookup_tbl)
#   on <- sort(setdiff(intersect(nam_x, nam_i), exclude_col))
#   # Prioritize 'year' if present
#   on <- on[order(match(on, "year"))]

#   # Identify value columns from lookup_tbl (columns not used as keys)
#   return_cols_nam <- setdiff(nam_i, on)
#   return_cols <- which(nam_i %in% return_cols_nam)

#   if (length(on) == 0L) {
#     stop("No common keys found between tbl and lookup_tbl")
#   }
#   if (length(on) == length(nam_i)) {
#     stop("No value columns identified in lookup_tbl. Consider using the 'exclude_col' argument.")
#   }

#   # Optionally validate lookup_tbl structure
#   if (check_lookup_tbl_validity) {
#     is_valid_lookup_tbl(lookup_tbl, on)
#   }

#   # Prepare lookup_tbl by setting its key to the common columns
#   setkeyv(lookup_tbl, cols = on)

#   # Initialize cardinality and min_lookup for key columns
#   cardinality <- vector("integer", length(on))
#   names(cardinality) <- on
#   min_lookup <- cardinality

#   # Calculate cardinality and minimum lookup values for each key column
#   for (j in on) {
#     if (is.factor(lookup_tbl[[j]])) {
#       lv <- levels(lookup_tbl[[j]])
#       if (check_lookup_tbl_validity && !identical(lv, levels(tbl[[j]]))) {
#         stop(j, " has different levels in tbl and lookup_tbl!")
#       }
#       cardinality[[j]] <- length(lv)
#       min_lookup[[j]] <- 1L
#     } else {
#       # For integer keys, assume values are sorted and consecutive
#       xmax <- last(lookup_tbl[[j]])
#       xmin <- first(lookup_tbl[[j]])
#       if (check_lookup_tbl_validity &&
#         (min(tbl[[j]], na.rm = TRUE) < xmin || max(tbl[[j]], na.rm = TRUE) > xmax)) {
#         message(j, " has rows in tbl without a match in lookup_tbl!")
#       }
#       cardinality[[j]] <- xmax - xmin + 1L
#       min_lookup[[j]] <- xmin
#     }
#   }

#   # Compute the cumulative product of cardinalities (in reverse) for index mapping
#   cardinality_prod <- shift(rev(cumprod(rev(cardinality))), -1, fill = 1L)

#   # Map each row in tbl to a unique lookup index using the starts_from_1 function
#   rownum <- as.integer(starts_from_1(tbl, on, 1L, min_lookup, cardinality) * cardinality_prod[[1L]])
#   if (length(on) > 1L) {
#     for (i in 2:length(on)) {
#       rownum <- as.integer(rownum -
#         (cardinality[[i]] - starts_from_1(tbl, on, i, min_lookup, cardinality)) * cardinality_prod[[i]])
#     }
#   }

#   # Merge lookup values into tbl or return them separately
#   if (merge) {
#     tbl[, (return_cols_nam) := dtsubset(lookup_tbl, rownum, return_cols)]
#     return(invisible(tbl))
#   } else {
#     return(invisible(dtsubset(lookup_tbl, rownum, return_cols)))
#   }
# }