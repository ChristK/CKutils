## CKutils: an R package with some utility functions I use regularly
## Copyright (C) 2025  Chris Kypridemos
##
## This file is part of CKutils and is distributed under the GNU GPL v3+
## (see R/lookup_dt.R for the full header).

## cklut: a memory-mapped, on-disk drop-in for lookup_dt --------------------
##
## lookup_dt maps each row of `tbl` to a row of a dense, key-sorted lookup
## table by computing a direct-address index from the key columns, then returns
## the value columns. cklut stores that dense table once, on disk, in a
## memory-mapped binary format and reads the value columns straight out -- so it
## works for tables larger than RAM, starts instantly, and avoids rebuilding /
## re-subsetting the lookup table on every call. Value columns may be of any
## type (double, integer, logical, factor/character), exactly like lookup_dt.
##
## Typical use:
##   ck <- cklut_build(lookup_tbl, "dist", keys = c("year","age","sex"))
##   res <- cklut_lookup(tbl, ck)                 # merge value cols into tbl
##   cklut_to_csv(ck, "dist.csv")                 # export back out

# internal: map an R column class to a cklut value-type string -----------------
.cklut_value_type <- function(x) {
  if (inherits(x, "integer64")) return("i64")
  if (is.factor(x)) return("str")
  if (is.character(x)) return("str")
  if (is.logical(x)) return("lgl")
  if (is.integer(x)) return("i32")
  if (is.double(x)) return("f64")
  stop("cklut: unsupported value column type: ", class(x)[1])
}

.cklut_type_code <- c(f64 = 0L, f32 = 1L, i32 = 2L, i64 = 3L, lgl = 4L, str = 5L)

# internal: sort keys the way lookup_dt does (alphabetical, 'year' first) ------
.cklut_order_keys <- function(keys) {
  keys <- sort(keys)
  keys[order(match(keys, "year"))]
}

# internal: row-major strides (last dim innermost, stride 1) -------------------
.cklut_strides <- function(sizes) {
  nd <- length(sizes)
  st <- numeric(nd)
  s <- 1
  for (d in seq.int(nd, 1L)) { st[d] <- s; s <- s * sizes[d] }
  st
}

#' Build an on-disk cklut lookup table
#'
#' Writes a dense lookup table (every combination of the key columns present
#' exactly once) to a memory-mapped, sharded binary format that
#' \code{\link{cklut_lookup}} can query. The source may be a \code{data.table},
#' or a path to a CSV or Parquet file.
#'
#' @param x A \code{data.table}, or a path to a \code{.csv}/\code{.parquet} file
#'   holding the dense lookup table.
#' @param out_base Output path prefix; the manifest \code{<out_base>.ckmeta} and
#'   payload shards \code{<out_base>.NNNN.ckdat} are written next to it.
#' @param keys Character vector of key (dimension) columns. Numeric keys must be
#'   consecutive integers; factor/character keys use their level order. The
#'   remaining columns become value columns.
#' @param values Character vector of value columns (default: all non-key columns).
#' @param value_types Optional named character vector overriding the inferred
#'   storage type of any value column. One of
#'   \code{"f64","f32","i32","i64","lgl","str"}.
#' @param max_bytes Shard size cap in bytes (default 100 MB).
#' @param check Logical; if \code{TRUE} (default) validate that the table is a
#'   dense, unique, consecutive grid (as \code{lookup_dt} requires).
#'
#' @return Invisibly, a \code{cklut} handle (as returned by \code{\link{cklut_open}}).
#' @seealso \code{\link{cklut_lookup}}, \code{\link{cklut_to_csv}}
#' @export
cklut_build <- function(x, out_base, keys, values = NULL,
                        value_types = NULL, max_bytes = 100 * 1024^2,
                        check = TRUE) {
  if (is.character(x) && length(x) == 1L) {
    ext <- tolower(tools::file_ext(x))
    if (ext == "csv") x <- data.table::fread(x)
    else if (ext %in% c("parquet", "pq")) x <- read_parquet_dt(x)
    else stop("cklut_build: unsupported file type '", ext, "' (use .csv or .parquet)")
  }
  if (!data.table::is.data.table(x)) x <- data.table::as.data.table(x)
  if (missing(keys) || length(keys) == 0L) stop("cklut_build: `keys` is required")
  if (!all(keys %in% names(x))) stop("cklut_build: keys not in data: ",
                                     paste(setdiff(keys, names(x)), collapse = ", "))
  keys <- .cklut_order_keys(keys)
  if (is.null(values)) values <- setdiff(names(x), keys)
  if (length(values) == 0L) stop("cklut_build: no value columns")

  # ---- key dimension metadata + per-row 0-based dense indices ----------------
  nd <- length(keys)
  dim_is_string <- logical(nd)
  dim_min <- numeric(nd); dim_size <- numeric(nd)
  dim_cats <- vector("list", nd)
  dim_index <- vector("list", nd)
  for (i in seq_len(nd)) {
    col <- x[[keys[i]]]
    if (is.factor(col)) {
      lv <- levels(col)
      dim_is_string[i] <- TRUE; dim_cats[[i]] <- lv
      dim_min[i] <- 0; dim_size[i] <- length(lv)
      dim_index[[i]] <- as.integer(col) - 1L
    } else if (is.character(col)) {
      lv <- sort(unique(col))
      dim_is_string[i] <- TRUE; dim_cats[[i]] <- lv
      dim_min[i] <- 0; dim_size[i] <- length(lv)
      dim_index[[i]] <- match(col, lv) - 1L
    } else if (is.numeric(col)) {
      mn <- min(col); mx <- max(col)
      dim_is_string[i] <- FALSE; dim_cats[[i]] <- NULL
      dim_min[i] <- mn; dim_size[i] <- mx - mn + 1
      dim_index[[i]] <- as.integer(col) - as.integer(mn)
    } else stop("cklut_build: key '", keys[i], "' must be integer, numeric or factor")
  }

  if (check) {
    expected <- prod(dim_size)
    if (nrow(x) != expected)
      stop("cklut_build: not a dense grid: ", nrow(x), " rows but ",
           format(expected, scientific = FALSE), " key combinations expected")
    if (anyDuplicated(x, by = keys))
      stop("cklut_build: key columns are not unique")
  }

  # ---- value column metadata -------------------------------------------------
  nv <- length(values)
  vt <- vapply(values, function(nm) .cklut_value_type(x[[nm]]), character(1))
  if (!is.null(value_types)) {
    hit <- intersect(names(value_types), values)
    vt[hit] <- value_types[hit]
  }
  value_data <- vector("list", nv); value_levels <- vector("list", nv)
  for (i in seq_len(nv)) {
    col <- x[[values[i]]]
    ty <- vt[i]
    if (ty == "str") {
      if (!is.factor(col)) col <- factor(col)
      value_levels[[i]] <- levels(col)
      value_data[[i]] <- as.integer(col)
    } else if (ty %in% c("f64", "f32", "i64")) {
      value_data[[i]] <- as.double(col)
    } else if (ty == "i32") {
      value_data[[i]] <- as.integer(col)
    } else if (ty == "lgl") {
      value_data[[i]] <- as.logical(col)
    } else stop("cklut_build: bad value type '", ty, "'")
  }

  cklut_build_cpp(out_base,
    dim_names = keys, dim_is_string = dim_is_string,
    dim_min = dim_min, dim_size = dim_size,
    dim_cats = dim_cats, dim_index = dim_index,
    value_names = values, value_types = unname(.cklut_type_code[vt]),
    value_data = value_data, value_levels = value_levels,
    max_bytes = max_bytes)

  invisible(cklut_open(paste0(out_base, ".ckmeta")))
}

#' Open a cklut table
#'
#' @param meta_path Path to the \code{.ckmeta} manifest.
#' @param warm Logical; if \code{TRUE}, eagerly load the whole table into the
#'   page cache before returning (good for latency-sensitive repeated use that
#'   fits in RAM). Default \code{FALSE} keeps the lazy, demand-paged behaviour.
#' @return A \code{cklut} handle.
#' @export
cklut_open <- function(meta_path, warm = FALSE) {
  xp <- cklut_open_cpp(meta_path, warm)
  structure(list(xp = xp, meta = meta_path, schema = cklut_schema_cpp(xp)),
            class = "cklut")
}

#' @export
print.cklut <- function(x, ...) {
  s <- x$schema
  cat("<cklut>", format(s$n_rows, scientific = FALSE), "rows,",
      s$n_shards, "shard(s)\n")
  cat("  keys  :", paste(s$dim_names, collapse = ", "), "\n")
  cat("  values:", paste0(s$value_names, " (", s$value_types, ")", collapse = ", "), "\n")
  invisible(x)
}

# internal: 1-based grid row numbers for `tbl` (NA = no match), computed in C++
.cklut_rownum <- function(tbl, ck, check = TRUE) {
  keys <- lapply(ck$schema$dim_names, function(nm) tbl[[nm]])   # in dimension order
  rn <- cklut_rownum_cpp(ck$xp, keys)
  if (check) { nmiss <- sum(is.na(rn))
    if (nmiss > 0L) message("cklut_lookup: ", nmiss, " row(s) in tbl without a match (returned NA)") }
  rn
}

#' Look up values from a cklut table (a drop-in for lookup_dt)
#'
#' Matches each row of \code{tbl} against the key columns of a cklut table and
#' returns the corresponding value columns. Mirrors \code{\link{lookup_dt}}:
#' common columns between \code{tbl} and the table's keys form the join, with
#' \code{"year"} prioritised; rows with no match return \code{NA}.
#'
#' @param tbl A \code{data.table} (or data.frame) whose key columns are matched.
#' @param ck A \code{cklut} handle (from \code{\link{cklut_open}}/
#'   \code{\link{cklut_build}}) or a path to a \code{.ckmeta} file.
#' @param merge Logical. If \code{TRUE} (default) the value columns are added to
#'   \code{tbl} (by reference if \code{tbl} is a data.table) and \code{tbl} is
#'   returned; if \code{FALSE} only the looked-up values are returned.
#' @param exclude_col Character vector of common columns to exclude from the join key.
#' @param as.data.table Logical. If \code{TRUE} (default) the result is a
#'   \code{data.table}; if \code{FALSE} a plain named \code{list} of column
#'   vectors is returned (cheaper if you only need the vectors).
#' @param check Logical; if \code{TRUE} (default) emit a message when some rows
#'   have no match.
#'
#' @return If \code{merge=TRUE}, \code{tbl} with the value columns added.
#'   Otherwise a \code{data.table} (or list, if \code{as.data.table=FALSE}) of
#'   the value columns, one row per row of \code{tbl}.
#' @seealso \code{\link{lookup_dt}}, \code{\link{cklut_build}}
#' @export
cklut_lookup <- function(tbl, ck, merge = TRUE, exclude_col = NULL,
                         as.data.table = TRUE, check = TRUE) {
  if (is.character(ck)) ck <- cklut_open(ck)
  if (!inherits(ck, "cklut")) stop("cklut_lookup: `ck` must be a cklut handle or .ckmeta path")
  sch <- ck$schema

  need <- setdiff(sch$dim_names, exclude_col)
  missing_keys <- setdiff(need, names(tbl))
  if (length(missing_keys))
    stop("cklut_lookup: tbl is missing key column(s): ", paste(missing_keys, collapse = ", "))

  rn <- .cklut_rownum(tbl, ck, check = check)
  vals <- cklut_gather_cpp(ck$xp, rn)

  if (merge) {
    if (!data.table::is.data.table(tbl)) tbl <- data.table::as.data.table(tbl)
    data.table::set(tbl, j = sch$value_names, value = vals)
    return(invisible(tbl))
  }
  if (as.data.table) return(data.table::setDT(vals)[])
  vals
}

# internal: reconstruct the full key grid (row-major, CJ order) as a data.table
.cklut_key_grid <- function(sch) {
  args <- vector("list", length(sch$dim_names))
  for (d in seq_along(sch$dim_names)) {
    if (sch$dim_is_string[d]) {
      args[[d]] <- factor(sch$dim_cats[[d]], levels = sch$dim_cats[[d]])
    } else {
      mn <- as.integer(sch$dim_min[d])
      args[[d]] <- seq.int(mn, mn + as.integer(sch$dim_size[d]) - 1L)
    }
  }
  names(args) <- sch$dim_names
  args$sorted <- FALSE              # keep dim order; CJ varies last dim fastest
  do.call(data.table::CJ, args)
}

#' Read an entire cklut table back into a data.table
#'
#' Reconstructs the full dense table (all key combinations + value columns) in
#' row-major order.
#'
#' @param ck A \code{cklut} handle or path to a \code{.ckmeta} file.
#' @return A \code{data.table} with the key and value columns.
#' @export
cklut_to_dt <- function(ck) {
  if (is.character(ck)) ck <- cklut_open(ck)
  sch <- ck$schema
  grid <- .cklut_key_grid(sch)
  vals <- cklut_gather_cpp(ck$xp, seq_len(nrow(grid)))
  data.table::set(grid, j = sch$value_names, value = vals)
  grid[]
}

#' Export a cklut table to CSV
#'
#' Writes the full table (keys + values) to CSV using \code{data.table::fwrite}.
#' The output round-trips back through \code{\link{cklut_build}}.
#'
#' @param ck A \code{cklut} handle or path to a \code{.ckmeta} file.
#' @param path Output \code{.csv} path.
#' @param ... Passed to \code{data.table::fwrite}.
#' @return Invisibly, \code{path}.
#' @export
cklut_to_csv <- function(ck, path, ...) {
  data.table::fwrite(cklut_to_dt(ck), path, ...)
  invisible(path)
}

#' Export a cklut table to Parquet
#'
#' Writes the full table (keys + values) to Parquet via
#' \code{\link{write_parquet_dt}} (Apache Arrow).
#'
#' @param ck A \code{cklut} handle or path to a \code{.ckmeta} file.
#' @param path Output \code{.parquet} path.
#' @param ... Passed to \code{write_parquet_dt}.
#' @return Invisibly, \code{path}.
#' @export
cklut_to_parquet <- function(ck, path, ...) {
  write_parquet_dt(cklut_to_dt(ck), path, ...)
  invisible(path)
}
