## reference_lookup.R
## A faithful, dependency-light port of CKutils::lookup_dt's index algorithm.
##
## CKutils::lookup_dt maps each row of `tbl` to a row of a dense, key-sorted
## `lookup_tbl` by computing a direct-address index from the key columns. The
## compiled package does this with starts_from_1_cpp + dtsubset; this file
## reproduces the IDENTICAL arithmetic in pure R (it mirrors the commented-out
## reference `lookup_dt_r`/`starts_from_1` in R/lookup_dt.R), so it can run with
## only data.table installed.
##
## Use this as the oracle for validating the C++ `cklut` library. To validate
## against the REAL function instead, replace `lookup_dt_ref(...)` with
## `CKutils::lookup_dt(...)` in gen_expected.R -- the results are identical by
## construction.

suppressWarnings(suppressMessages(library(data.table)))

# Mirror of CKutils:::starts_from_1 (pure-R form).
starts_from_1 <- function(tbl, on, i, min_lookup, cardinality) {
  coldata <- tbl[[on[[i]]]]
  offset  <- min_lookup[[i]] - 1L
  if (is.integer(coldata) && !is.factor(coldata)) {
    out <- coldata - offset
    out[out < 1L | out > cardinality[[i]]] <- NA_integer_
    out
  } else if (is.factor(coldata)) {
    as.integer(coldata) - offset
  } else if (is.numeric(coldata)) {
    out <- as.integer(coldata) - offset
    out[out < 1L | out > cardinality[[i]]] <- NA_integer_
    out
  } else {
    stop("Column data must be integer or factor.")
  }
}

# Faithful port of lookup_dt's row-index computation. Returns a data.table of
# the looked-up value columns, one row per row of `tbl` (i.e. merge = FALSE).
lookup_dt_ref <- function(tbl, lookup_tbl, exclude_col = NULL) {
  stopifnot(is.data.table(tbl), is.data.table(lookup_tbl))

  nam_x <- names(tbl)
  nam_i <- names(lookup_tbl)
  on <- sort(setdiff(intersect(nam_x, nam_i), exclude_col))
  on <- on[order(match(on, "year"))]                      # prioritise 'year'

  return_cols_nam <- setdiff(nam_i, on)
  return_cols     <- which(nam_i %in% return_cols_nam)

  setkeyv(lookup_tbl, cols = on)                          # sort lookup row-major

  cardinality <- vector("integer", length(on)); names(cardinality) <- on
  min_lookup  <- cardinality
  for (j in on) {
    if (is.factor(lookup_tbl[[j]])) {
      cardinality[[j]] <- length(levels(lookup_tbl[[j]]))
      min_lookup[[j]]  <- 1L
    } else {
      xmax <- last(lookup_tbl[[j]]); xmin <- first(lookup_tbl[[j]])
      cardinality[[j]] <- as.integer(xmax - xmin + 1L)
      min_lookup[[j]]  <- as.integer(xmin)
    }
  }

  cardinality_prod <- shift(rev(cumprod(rev(cardinality))), -1, fill = 1L)

  rownum <- as.integer(
    starts_from_1(tbl, on, 1L, min_lookup, cardinality) * cardinality_prod[[1L]]
  )
  if (length(on) > 1L) {
    for (i in 2:length(on)) {
      rownum <- as.integer(
        rownum -
          (cardinality[[i]] - starts_from_1(tbl, on, i, min_lookup, cardinality)) *
            cardinality_prod[[i]]
      )
    }
  }

  list(
    on      = on,
    rownum  = rownum,
    values  = lookup_tbl[rownum, ..return_cols_nam]
  )
}
