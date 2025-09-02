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
#' @param check_lookup_tbl_validity Logical. If \code{TRUE} (default), validates the structure of \code{lookup_tbl}.
#'
#' @return A data.table. When \code{merge = TRUE}, \code{tbl} is returned with additional lookup columns;
#' otherwise, a data.table containing only the lookup results is returned.
#'
#' @details
#' The \code{lookup_dt} function is designed for efficient data merging and lookup operations
#' on \code{data.table} objects. It works by identifying common key columns between the main
#' table (\code{tbl}) and the lookup table (\code{lookup_tbl}). These key columns should
#' ideally be factors or integers representing categorical data or ordered sequences.
#'
#' The core logic involves mapping each unique combination of key values in \code{tbl}
#' to a specific row index in \code{lookup_tbl}. This is achieved by calculating a
#' unique integer index for each row in \code{tbl} based on the cardinalities (number of
#' unique values) of the key columns. The \code{starts_from_1_cpp} function (a C++
#' helper) is used to efficiently convert key column values into 1-based indices.
#'
#' If \code{merge = TRUE}, the values from the non-key columns in \code{lookup_tbl}
#' (i.e., the lookup values) are added as new columns to \code{tbl}. If \code{merge = FALSE},
#' only the selected lookup values corresponding to the rows in \code{tbl} are returned
#' as a new \code{data.table}.
#'
#' The \code{exclude_col} parameter allows specific columns to be ignored during the
#' key matching process, which can be useful if some common columns are not part of
#' the intended join key.
#'
#' The \code{check_lookup_tbl_validity} parameter, when \code{TRUE}, invokes
#' \code{is_valid_lookup_tbl} to ensure that \code{lookup_tbl} is structured correctly
#' for the lookup (e.g., unique keys, consecutive integer values for non-factor keys).
#'
#' This function is particularly useful when dealing with large datasets where
#' standard merge operations might be less performant or when a more controlled
#' lookup based on pre-defined key structures is required.
#'
#' @examples
#' library(data.table)
#' # Example 1: Basic lookup and merge
#' main_dt <- data.table(id = 1:5, category = factor(c("A", "B", "A", "C", "B")))
#' lookup_values <- data.table(category = factor(c("A", "B", "C")),
#'                             value = c(10, 20, 30))
#' result_dt <- lookup_dt(main_dt, lookup_values, merge = TRUE)
#' print(result_dt)
#' # Returns main_dt with an added 'value' column:
#' #    id category value
#' # 1:  1        A    10
#' # 2:  2        B    20
#' # 3:  3        A    10
#' # 4:  4        C    30
#' # 5:  5        B    20
#'
#' # Example 2: Lookup without merging, returning only lookup results
#' main_dt2 <- data.table(year = c(2020L, 2021L, 2020L),
#'                        product_id = c(101L, 102L, 101L))
#' price_lookup <- data.table(year = c(2020L, 2021L, 2020L, 2021L),
#'                            product_id = c(101L, 102L, 102L, 101L),
#'                            price = c(5.99, 8.50, 8, 6.75))
#' # Ensure lookup_tbl has keys set for is_valid_lookup_tbl if used,
#' # or for the main lookup_dt logic.
#' setkeyv(price_lookup, c("year", "product_id"))
#' prices_only <- lookup_dt(main_dt2, price_lookup, merge = FALSE)
#' print(prices_only)
#' # Returns a data.table with prices corresponding to main_dt2 rows:
#' #    price
#' # 1:  5.99
#' # 2:  8.50
#' # 3:  5.99
#'
#' # Example 3: Using exclude_col
#' sales_data <- data.table(region = c("North", "South", "North"),
#'                          item = factor(c("apple", "banana", "apple"),
#'                                        levels = c("apple", "banana")),
#'                          sales_rep_id = c(1, 2, 1))
#' item_details <- data.table(item = factor(c("apple", "banana"), 
#'                                          levels = c("apple", "banana")),
#'                            category = c("fruit", "fruit"),
#'                            supplier_id = c(10, 20),
#'                            sales_rep_id = c(99, 99)) # This sales_rep_id should be ignored
#' # We want to lookup 'category' and 'supplier_id' based on 'item' only.
#' setkey(item_details, item) # Key for lookup
#' sales_with_details <- lookup_dt(sales_data, item_details,
#'                                 exclude_col = "sales_rep_id", merge = TRUE)
#' print(sales_with_details)
#' #    region   item sales_rep_id category supplier_id
#' # 1:  North  apple            1    fruit          10
#' # 2:  South banana            2    fruit          20
#' # 3:  North  apple            1    fruit          10
#'
#' @seealso \code{\link[data.table]{setkeyv}}, \code{\link{is_valid_lookup_tbl}}
#' @keywords data manipulation utilities
#' @export
lookup_dt <- function(
  tbl,
  lookup_tbl,
  merge = TRUE,
  exclude_col = NULL,
  check_lookup_tbl_validity = TRUE
) {
  # Ensure both inputs are data.tables
  if (!is.data.table(tbl)) {
    stop("tbl must be a data.table")
  }
  if (!is.data.table(lookup_tbl)) {
    stop("lookup_tbl must be a data.table")
  }
  
  # Check for empty tables
  if (nrow(tbl) == 0) {
    stop("tbl cannot be empty")
  }
  if (nrow(lookup_tbl) == 0) {
    stop("lookup_tbl cannot be empty")
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
    stop(
      "No value columns identified in lookup_tbl. Consider using the 'exclude_col' argument."
    )
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
    # Additional safety checks for column data
    if (is.null(tbl[[j]]) || is.null(lookup_tbl[[j]])) {
      stop("Column '", j, "' contains NULL data")
    }
    
    if (is.factor(lookup_tbl[[j]])) {
      lv <- levels(lookup_tbl[[j]])
      if (check_lookup_tbl_validity && !identical(lv, levels(tbl[[j]]))) {
        stop(j, " has different levels in tbl and lookup_tbl!")
      }
      cardinality[[j]] <- length(lv)
      min_lookup[[j]] <- 1L
    } else {
      # For integer keys, assume values are sorted and consecutive
      # Add safety checks for integer overflow
      if (!is.integer(lookup_tbl[[j]]) && !is.numeric(lookup_tbl[[j]])) {
        stop("Column '", j, "' must be integer or numeric")
      }
      
      # Check for NA values that could cause issues
      if (any(is.na(lookup_tbl[[j]]))) {
        stop("Column '", j, "' in lookup_tbl contains NA values")
      }
      
      xmax <- last(lookup_tbl[[j]])
      xmin <- first(lookup_tbl[[j]])
      
      # Check for integer overflow potential
      if (is.infinite(xmax) || is.infinite(xmin) || xmax - xmin + 1L > .Machine$integer.max) {
        stop("Column '", j, "' range too large, potential integer overflow")
      }
      
      if (
        check_lookup_tbl_validity &&
          (min(tbl[[j]], na.rm = TRUE) < xmin ||
            max(tbl[[j]], na.rm = TRUE) > xmax)
      ) {
        message(j, " has rows in tbl without a match in lookup_tbl!")
      }
      cardinality[[j]] <- as.integer(xmax - xmin + 1L)
      min_lookup[[j]] <- as.integer(xmin)
    }
  }

  # Compute the cumulative product of cardinalities (in reverse) for index mapping
  cardinality_prod <- shift(rev(cumprod(rev(cardinality))), -1, fill = 1L)

  # Map each row in tbl to a unique lookup index using the starts_from_1 function
  # Add error handling around C++ calls
  tryCatch({
    rownum <- as.integer(
      starts_from_1_cpp(tbl, on, 1L, min_lookup, cardinality) *
        cardinality_prod[[1L]]
    )
    if (length(on) > 1L) {
      for (i in 2:length(on)) {
        rownum <- as.integer(
          rownum -
            (cardinality[[i]] -
              starts_from_1_cpp(tbl, on, i, min_lookup, cardinality)) *
              cardinality_prod[[i]]
        )
      }
    }
  }, error = function(e) {
    stop("Error in C++ index calculation: ", e$message)
  })

  # Additional validation of rownum before calling dtsubset
  if (check_lookup_tbl_validity && anyNA(rownum)) {
    warning("Some row indices are NA, results may be incomplete")
  }
  
  # Check bounds, handling NA values properly
  valid_indices <- !is.na(rownum)
  if (any(valid_indices) && any(rownum[valid_indices] < 1L | rownum[valid_indices] > nrow(lookup_tbl))) {
    stop("Calculated row indices are out of bounds")
  }

  # Merge lookup values into tbl or return them separately
  tryCatch({
    if (merge) {
      tbl[, (return_cols_nam) := dtsubset(lookup_tbl, rownum, return_cols)]
      return(invisible(tbl))
    } else {
      return(invisible(dtsubset(lookup_tbl, rownum, return_cols)))
    }
  }, error = function(e) {
    stop("Error in data.table subset operation: ", e$message)
  })
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
#' @details
#' The \code{is_valid_lookup_tbl} function checks the structural validity of a lookup table
#' used in conjunction with the \code{lookup_dt} function. It ensures that the key columns
#' are appropriately defined and that the table contains all necessary combinations of key
#' values without gaps or duplicates.
#'
#' Key checks include:
#' - Presence of duplicate rows based on key columns.
#' - Non-consecutive integer values in key columns (where applicable).
#' - Correct number of rows based on the Cartesian product of key levels/values.
#'
#' @examples
#' library(data.table)
#' # Example 1: Valid lookup table
#' valid_lt <- data.table(id = 1:3, category = factor(letters[1:2]), value = runif(6))
#' setkeyv(valid_lt, c("id", "category")) # Set keys
#' # Manually ensure it meets criteria for a real use case, e.g., all combinations present
#' # For this example, let's assume it's structured correctly for its intended keys.
#' # is_valid_lookup_tbl(valid_lt, keycols = c("id", "category"))
#' # This would typically run if valid_lt had unique combinations of id & category
#' # and id was consecutive, category levels were fully represented.
#'
#' # Example 2: Invalid lookup table (duplicate keys)
#' invalid_lt_dup <- data.table(id = c(1, 1, 2), value = c(10, 20, 30))
#' try(is_valid_lookup_tbl(invalid_lt_dup, keycols = "id"))
#'
#' # Example 3: Invalid lookup table (non-consecutive integer key)
#' invalid_lt_gap <- data.table(id = c(1, 3, 4), value = c(10, 20, 30))
#' setkey(invalid_lt_gap, id)
#' # try(is_valid_lookup_tbl(invalid_lt_gap, keycols = "id")) # Will error due to gap
#'
#' @keywords internal utilities validation
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
      stop(paste0(
        "Lookup table key columns must be of type integer (or factor). Column '",
        j,
        "' is not integer."
      ))
    }

    # For integer columns, ensure the values form a consecutive sequence
    if (is.integer(lookup_tbl[[j]])) {
      x <- sort(lookup_tbl[[j]])
      if (length(x) > 1 && any(diff(x) > 1L)) {
        stop(paste0(
          "Lookup table key column '",
          j,
          "' does not contain consecutive integer values."
        ))
      }
    }

    # Recommend setting the key for best performance if not already set
    if (!identical(key(lookup_tbl), keycols)) {
      message(
        "For best performance, consider setting the key of lookup_tbl to: ",
        paste(keycols, collapse = ", ")
      )
      if (fixkey) {
        setkeyv(lookup_tbl, keycols)
        message("Key has been set to: ", paste(keycols, collapse = ", "))
      }
    }

    # Verify the lookup table has the expected number of rows
    if (nrow(lookup_tbl) != expected_rows) {
      stop(paste0(
        "Lookup table should have ",
        expected_rows,
        " rows based on key combinations, but has ",
        nrow(lookup_tbl),
        " rows."
      ))
    }

    return(TRUE)
  }
}


#' Set Lookup Table Key
#'
#' Sets the key columns of a lookup table to optimise performance for lookup operations.
#' The key columns are sorted (with a priority given to "year" if present) and then set
#' as the key for the data.table.
#'
#' @param lookup_tbl A data.table whose key columns are to be set.
#' @param keycols A character vector specifying the key columns to be used.
#'
#' @return The original lookup table (invisibly) with its key set for efficient subsetting.
#'
#' @details
#' The \code{set_lookup_tbl_key} function assigns key columns to a lookup table, enhancing
#' the performance of subsequent lookup operations. It is essential that the specified key
#' columns are appropriate for the data and that they uniquely identify rows in the table.
#'
#' @examples
#' library(data.table)
#' my_lookup <- data.table(year = rep(2020:2021, each = 2),
#'                         product_id = rep(1:2, 2),
#'                         price = rnorm(4, 10, 2))
#' print(key(my_lookup)) # NULL
#' set_lookup_tbl_key(my_lookup, keycols = c("year", "product_id"))
#' print(key(my_lookup)) # "year" "product_id"
#'
#' @keywords internal utilities
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
  setkeyv(lookup_tbl, keycols)

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
