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
#'   A column named here that both tables have is then a value column: it is returned with the
#'   other lookup values, so with \code{merge = TRUE} it overwrites that column of \code{tbl}.
#' @param check_lookup_tbl_validity Logical. If \code{TRUE} (default), validates the structure of \code{lookup_tbl}.
#'   If \code{FALSE}, \code{lookup_tbl} is not validated, apart from a few checks that cost
#'   little: among them, that it has as many rows as its key values have combinations.
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
#' the intended join key. Such a column is not ignored altogether: if
#' \code{lookup_tbl} has it, it is looked up like any other value column, so with
#' \code{merge = TRUE} its values replace those of \code{tbl}.
#'
#' The \code{check_lookup_tbl_validity} parameter, when \code{TRUE}, invokes
#' \code{is_valid_lookup_tbl} to ensure that \code{lookup_tbl} is structured correctly
#' for the lookup (e.g., unique keys, consecutive integer values for non-factor keys).
#' It also makes \code{lookup_dt} check that the rows of \code{lookup_tbl} follow
#' its key columns, rather than trust a key or index it already carries, which
#' tools outside data.table (base \code{[[<-}, dplyr verbs) can leave stale.
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
#'                            sales_rep_id = c(99, 99))
#' # Look up by 'item' only. sales_rep_id, in both tables, is then not a key
#' # but a value column: it is looked up too, and replaces sales_data's own.
#' setkey(item_details, item) # Key for lookup
#' sales_with_details <- lookup_dt(sales_data, item_details,
#'                                 exclude_col = "sales_rep_id", merge = TRUE)
#' print(sales_with_details)
#' #    region   item sales_rep_id category supplier_id
#' # 1:  North  apple           99    fruit          10
#' # 2:  South banana           99    fruit          20
#' # 3:  North  apple           99    fruit          10
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
    key_order <- .validate_lookup_tbl(lookup_tbl, on)
    # Tools outside data.table (base [[<-, dplyr verbs) can leave a key or an
    # index that the rows no longer follow. setkeyv() would trust it, and the
    # row arithmetic below would read the wrong rows. The validation has read
    # the rows (one pass): if they are in key order, mark the key, so that
    # setkeyv() has nothing to do; if not, drop key and indices, so that it
    # really sorts.
    if (key_order == 2L) {
      setattr(lookup_tbl, "sorted", on)
    } else {
      setkeyv(lookup_tbl, NULL)
    }
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
    if (is.null(tbl[[j]]) || is.null(lookup_tbl[[j]])) {       # nocov start
      stop("Column '", j, "' contains NULL data")
    }                                                          # nocov end
    
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
      
      # Check for integer overflow potential, in double (in integer arithmetic
      # the span itself overflowed to NA): the span and the values must both
      # fit in an integer
      span <- as.numeric(xmax) - as.numeric(xmin) + 1
      if (!is.finite(span) || span > .Machine$integer.max ||
          max(abs(as.numeric(c(xmin, xmax)))) > .Machine$integer.max) {
        stop("Column '", j, "' range too large, potential integer overflow")
      }
      # With the rows in key order, a key of a full grid ends where it is largest
      if (span < 1) {
        stop("lookup_tbl is not a full grid of its key values (key column '", j, "').")
      }

      if (
        check_lookup_tbl_validity &&
          (min(tbl[[j]], na.rm = TRUE) < xmin ||
            max(tbl[[j]], na.rm = TRUE) > xmax)
      ) {
        message(j, " has rows in tbl without a match in lookup_tbl!")
      }
      cardinality[[j]] <- as.integer(span)
      min_lookup[[j]] <- as.integer(xmin)
    }
  }

  # The row arithmetic below needs one row per combination of key values.
  # Checked whatever check_lookup_tbl_validity says, as it costs nothing: it
  # catches a gap, a missing or extra row, an unused factor level -- though not
  # everything the full validation does (e.g. NA in a factor key).
  if (!isTRUE(nrow(lookup_tbl) == prod(cardinality))) {
    stop(
      "lookup_tbl is not a full grid of its key values: it has ",
      nrow(lookup_tbl), " rows for ",
      format(prod(cardinality), scientific = FALSE), " combinations."
    )
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
    # Provide detailed diagnostic information about column types
    col_info <- sapply(on, function(col) {
      paste0(col, ": class=", class(tbl[[col]])[1], ", typeof=", typeof(tbl[[col]]))
    })
    stop("Error in C++ index calculation: ", e$message, 
         "\nColumn types: ", paste(col_info, collapse = "; "))
  })

  # Additional validation of rownum before calling dtsubset
  if (check_lookup_tbl_validity && anyNA(rownum)) {
    warning("Some row indices are NA, results may be incomplete")
  }
  
  # Check bounds, handling NA values properly. Defensive: with as many rows as
  # key combinations (checked above), every index is within 1..nrow.
  valid_indices <- !is.na(rownum)
  if (any(valid_indices) && any(rownum[valid_indices] < 1L | rownum[valid_indices] > nrow(lookup_tbl))) {
    stop("Calculated row indices are out of bounds")                 # nocov
  }

  # Merge lookup values into tbl or return them separately
  tryCatch({
    if (merge) {
      tbl[, (return_cols_nam) := dtsubset(lookup_tbl, rownum, return_cols)]
      return(invisible(tbl))
    } else {
      return(invisible(dtsubset(lookup_tbl, rownum, return_cols)))
    }
  }, error = function(e) {                                     # nocov start
    stop("Error in data.table subset operation: ", e$message)
  })                                                           # nocov end
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
#' @param keycols A character vector of distinct column names: the key columns in the lookup table.
#' @param fixkey Logical. If TRUE, the function will automatically set the key of the lookup table to \code{keycols} for best performance, in the order \code{lookup_dt} uses (sorted, with "year" first), once the table has passed every check; default is FALSE.
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
#' - keycols names distinct columns of lookup_tbl, and the table has rows.
#' - Every key column is integer or factor, without NA; an integer key column
#'   takes consecutive values, and a factor uses all its levels (the errors name
#'   the missing ones).
#' - No two rows share a combination of key values.
#' - The number of rows is the product of the key columns' numbers of values
#'   (of levels, for a factor), so that every combination is present.
#'
#' @examples
#' library(data.table)
#' # Example 1: Valid lookup table (every id x category combination, once)
#' valid_lt <- CJ(id = 1:3, category = factor(c("a", "b")))
#' valid_lt[, value := runif(.N)]
#' is_valid_lookup_tbl(valid_lt, keycols = c("id", "category"))
#'
#' # Example 2: Invalid lookup table (duplicate keys)
#' invalid_lt_dup <- data.table(id = c(1L, 1L, 2L), value = c(10, 20, 30))
#' try(is_valid_lookup_tbl(invalid_lt_dup, keycols = "id"))
#'
#' # Example 3: Invalid lookup table (non-consecutive integer key: no id 2)
#' invalid_lt_gap <- data.table(id = c(1L, 3L, 4L), value = c(10, 20, 30))
#' try(is_valid_lookup_tbl(invalid_lt_gap, keycols = "id"))
#'
#' @keywords internal utilities validation
#' @export
is_valid_lookup_tbl <- function(lookup_tbl, keycols, fixkey = FALSE) {
  .validate_lookup_tbl(lookup_tbl, keycols, fixkey)
  TRUE
}

# is_valid_lookup_tbl()'s checks. Returns, invisibly, how the rows are ordered by
# the key columns (key_order_cpp()), which lookup_dt() uses so that it does not
# read the rows a second time.
.validate_lookup_tbl <- function(lookup_tbl, keycols, fixkey = FALSE) {
  if (!is.data.table(lookup_tbl)) {
    stop("lookup_tbl should be a data.table.")
  }

  if (missing(keycols) || length(keycols) == 0L) {
    stop("keycols argument is missing.")
  }
  if (!is.character(keycols) || anyNA(keycols) || anyDuplicated(keycols)) {
    stop("keycols must be distinct column names, without NA.")
  }
  absent <- setdiff(keycols, names(lookup_tbl))
  if (length(absent)) {
    stop("keycols not found in lookup_tbl: ", paste(absent, collapse = ", "), ".")
  }

  if (nrow(lookup_tbl) == 0L) {
    stop("Lookup table has no rows.")
  }

  # Sort key columns and prioritize 'year' if present
  keycols <- sort(keycols)
  keycols <- keycols[order(match(keycols, "year"))]

  # Validate each key column, and count the values it takes
  n_vals <- numeric(length(keycols))
  for (i in seq_along(keycols)) {
    j <- keycols[[i]]
    x <- lookup_tbl[[j]]

    # Check that the column is of type integer (factors are stored as integers)
    if (typeof(x) != "integer") {
      stop(paste0(
        "Lookup table key columns must be of type integer (or factor). Column '",
        j,
        "' is not integer."
      ))
    }

    # lookup_dt() finds rows by arithmetic on the key values, which NA breaks.
    # The values of an integer key must fill [min, max]; a factor must use
    # every level.
    if (is.integer(x)) {
      if (anyNA(x)) {
        stop(paste0("Lookup table key column '", j, "' contains NA values."))
      }
      # as.numeric(): no integer overflow, and no date arithmetic on IDate keys
      n_vals[[i]] <- uniqueN(x)
      if (as.numeric(max(x)) - as.numeric(min(x)) + 1 != n_vals[[i]]) {
        stop(paste0(
          "Lookup table key column '",
          j,
          "' does not contain consecutive integer values",
          .missing_key_values(x),
          "."
        ))
      }
    } else {
      # A factor. tabulate() counts its codes in one pass, skipping NA; anyNA()
      # would allocate a logical vector as long as the column (is.na() on a
      # classed vector).
      n_vals[[i]] <- nlevels(x)
      counts <- tabulate(x, nbins = n_vals[[i]])
      if (sum(counts) != length(x)) {
        stop(paste0(
          "Lookup table key column '",
          j,
          "' contains NA values (in a factor, often a label that is not one of its levels)."
        ))
      }
      if (any(counts == 0L)) {
        unused <- levels(x)[counts == 0L]
        stop(paste0(
          "Lookup table key column '",
          j,
          "' does not contain every level of the factor",
          .missing_note(unused[seq_len(min(length(unused), 10L))], length(unused)),
          "."
        ))
      }
    }
  }

  # Ensure unique combinations of key columns. One pass over the rows tells
  # whether they are in key order and, if so, whether two neighbours are
  # equal. Not duplicated(by = keycols): it trusts a key that keycols prefix and
  # then compares adjacent rows only, so a key left stale by tools outside
  # data.table (base [[<-, dplyr verbs) hid duplicates. Rows out of key order
  # are checked on a new table of the key columns, which has no key or index.
  key_order <- key_order_cpp(lookup_tbl, keycols)
  if (key_order == 1L ||
      (key_order == 0L &&
       anyDuplicated(setDT(`names<-`(lapply(keycols, function(k) lookup_tbl[[k]]), keycols))))) {
    stop("Lookup table must have a unique combination of key columns.")
  }

  # Verify the lookup table has the expected number of rows
  expected_rows <- prod(n_vals)
  if (nrow(lookup_tbl) != expected_rows) {
    stop(paste0(
      "Lookup table should have ",
      expected_rows,
      " rows based on key combinations, but has ",
      nrow(lookup_tbl),
      " rows."
    ))
  }

  # Recommend setting the key for best performance if not already set, or if
  # the key is stale (the rows do not follow it)
  if (!identical(key(lookup_tbl), keycols) || key_order == 0L) {
    message(
      "For best performance, consider setting the key of lookup_tbl to: ",
      paste(keycols, collapse = ", "),
      if (identical(key(lookup_tbl), keycols)) " (its key is stale: the rows do not follow it)"
    )
    if (fixkey) {
      # Mark the key if the rows already follow it; else sort them, trusting
      # no key or index (setkeyv() would reuse a stale one)
      if (key_order == 2L) {
        setattr(lookup_tbl, "sorted", keycols)
      } else {
        setkeyv(lookup_tbl, NULL)
        setkeyv(lookup_tbl, keycols)
      }
      message("Key has been set to: ", paste(keycols, collapse = ", "))
    }
  }

  invisible(key_order)
}

# The values in the range of the integer key x that x lacks, for the message of
# is_valid_lookup_tbl(): the first `show` of them, in the key's own class (so
# that e.g. IDate keys show dates), and how many in all. Runs only once the
# check has failed; never materialises the range.
.missing_key_values <- function(x, show = 10L) {
  u <- as.numeric(sort(unique(x)))
  gap <- diff(u) - 1
  vals <- numeric(0)
  for (k in which(gap > 0)) {
    vals <- c(vals, u[[k]] + seq_len(min(gap[[k]], show - length(vals))))
    if (length(vals) >= show) break
  }
  .missing_note(as.character(structure(as.integer(vals), class = oldClass(x))), sum(gap))
}

# " (missing: a, b, ... n in all)": the values shown and, if there are more,
# how many in all
.missing_note <- function(shown, n_missing) {
  paste0(
    " (missing: ",
    paste(shown, collapse = ", "),
    if (n_missing > length(shown)) {
      paste0(", ... ", format(n_missing, big.mark = ",", scientific = FALSE), " in all")
    },
    ")"
  )
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
