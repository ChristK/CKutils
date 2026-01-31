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

.onUnload <- function(libpath) {
  library.dynam.unload("CKutils", libpath)
}

#' Get Dropbox path
#'
#' `get_dropbox_path` returns the path of Dropbox. Works for both personal and business accounts
#'
#' This is an auxilliary function: It finds the Dropbox path in Windows, Linux, and OSX operating systems.
#'
#' @param pathtail A String vector (if not a string then it is converted to String).
#'    If present, it gets concatenated with the Dropbox path.
#'    See examples.
#' @param type A String scalar ("personal" or "business"). Which Dropbox path to return? The personal or the business one? It may be abbreviated.
#' @return Dropbox path as a String. If pathtail is present, it concatenates the Dropbox path with pathtail.
#' @export
#' @examples
#' \dontrun{
#' # Only work if Dropbox is installed.
#' get_dropbox_path() # Returns personal Dropbox path
#' get_dropbox_path(type = "business") # Returns business Dropbox path
#' get_dropbox_path("pathdownthetree") # Returns "Dropbox_path/pathdownthetree",
#'  # where Dropbox_path is the path to personal Dropbox
#' }
get_dropbox_path <-
  function(pathtail = character(0), type = c("personal", "business")) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Please install package jsonlite first.")
    }
    type <- match.arg(type)
    if (type[[1]] == "personal") {
      if (.Platform$OS.type == "windows") {
        if (file.exists(paste0(Sys.getenv("APPDATA"), "/Dropbox/info.json"))) {
          # for older versions of Dropbox
          dropbox_path <-
            jsonlite::read_json(paste0(
              Sys.getenv("APPDATA"),
              "/Dropbox/info.json"
            ))$personal$path
        }
        if (
          file.exists(paste0(Sys.getenv("LOCALAPPDATA"), "/Dropbox/info.json"))
        ) {
          dropbox_path <-
            jsonlite::read_json(paste0(
              Sys.getenv("LOCALAPPDATA"),
              "/Dropbox/info.json"
            ))$personal$path
        }
      } else {
        if (file.exists("~/.dropbox/info.json")) {
          dropbox_path <-
            jsonlite::read_json("~/.dropbox/info.json")$personal$path
        }
      }
    }
    if (type[[1]] == "business") {
      if (.Platform$OS.type == "windows") {
        if (file.exists(paste0(Sys.getenv("APPDATA"), "/Dropbox/info.json"))) {
          # for older versions of Dropbox
          dropbox_path <-
            jsonlite::read_json(paste0(
              Sys.getenv("APPDATA"),
              "/Dropbox/info.json"
            ))$business$path
        }
        if (
          file.exists(paste0(Sys.getenv("LOCALAPPDATA"), "/Dropbox/info.json"))
        ) {
          dropbox_path <-
            jsonlite::read_json(paste0(
              Sys.getenv("LOCALAPPDATA"),
              "/Dropbox/info.json"
            ))$business$path
        }
      } else {
        if (file.exists("~/.dropbox/info.json")) {
          dropbox_path <-
            jsonlite::read_json("~/.dropbox/info.json")$business$path
        }
      }
    }
    if (is.null(dropbox_path)) {
      stop("Dropbox path cannot be located.")
    }
    dropbox_path <-
      normalizePath(paste0(dropbox_path, "/", pathtail), mustWork = FALSE)
    return(dropbox_path)
  }


#' Get pCloud path
#'
#' `get_pcloud_path` returns the path of pCloud
#'
#' This is an auxilliary function: It finds the pCloud path in Windows, Linux, and OSX operating systems.
#'
#' @param pathtail A String vector (if not a string then it is converted to String).
#'    If present, it gets concatenated with the pCloud path.
#'    See examples.
#' @return pCloud path as a String. If pathtail is present, it concatenates the Dropbox path with pathtail.
#' @export
#' @examples
#' \dontrun{
#' # Only work if Dropbox is installed.
#' get_pcloud_path() # Returns pCloud path
#' get_pcloud_path("pathdownthetree") # Returns "pcloud_path_path/pathdownthetree",
#' # where pcloud_path is the path to pCloud
#' }
get_pcloud_path <- function(pathtail = character(0)) {
  if (.Platform$OS.type == "windows") {
    pcloud_path <- "p:\\"
  } else {
    pcloud_path <- "~/pCloudDrive/"
  }

  pcloud_path <-
    normalizePath(paste0(pcloud_path, pathtail), mustWork = FALSE)
  return(pcloud_path)
}


#' Generate names for age-group bands
#'
#' `agegrp_name` generates names for age-group bands given lower and upper
#'   age limits, and band width
#'
#'
#' @param min_age A non-negative integer. The lower age limit for which
#'   names will be generated.
#' @param max_age A non-negative integer. The upper age limit for which
#'   names will be generated.
#' @param grp_width A positive integer. The band width of the age-groups.
#' @param grp_lessthan_1 A logical scalar. if \code{TRUE} and
#'  \code{min_age == 0}, then the first age-group name is "<1".
#' @param match_input A logical scalar. If \code{TRUE}, then the names
#'  are repeated to match every single year of age between \code{min_age}
#'  and \code{match_input_max_age}.
#' @param match_input_max_age a non-negative integer. See above.
#' @return A character vector of with the names for the age-groups.
#' @export
#' @examples
#' agegrp_name(20, 79, 5)
#' agegrp_name(20, 80, 5)
#' agegrp_name(0, 80, 10, TRUE)
#' agegrp_name(20, 30, 5, FALSE, TRUE)
#' agegrp_name(20, 30, 5, FALSE, TRUE)
#' agegrp_name(20, 30, 5, FALSE, TRUE, 32)
agegrp_name <-
  function(
    min_age = 0L,
    max_age = 85L,
    grp_width = 5L,
    grp_lessthan_1 = TRUE,
    match_input = FALSE,
    match_input_max_age = max_age
  ) {
    stopifnot(
      min_age >= 0,
      max_age > 0,
      grp_width >= 1,
      max_age > min_age,
      match_input_max_age >= max_age
    )
    if (grp_width > 1) {
      x <- seq(min_age, max_age + 1L, grp_width)
      y <- shift(x, type = "lead") - 1L
      out <- paste0(sprintf("%02.0f", x), "-", sprintf("%02.0f", y))
      if ((tail(x, 1) - 1L) != max_age) {
        out[length(out)] <- paste0(x[length(x)], "+")
      } else {
        out <- head(out, length(out) - 1L)
      }
      if (grp_lessthan_1 && min_age == 0) {
        out <- c("<1", out)
        out[2] <- paste0("01", "-", sprintf("%02.0f", y[1]))
      }

      if (grp_lessthan_1 && min_age == 0) {
        if (match_input && ((tail(x, 1) - 1L) != max_age)) {
          out <- c(
            rep(out[2:((length(out) - 1L))], each = grp_width),
            rep(out[length(out)], match_input_max_age - tail(x, 1) + 1L)
          )
          out[1] <- "<1"
        }
        if (match_input && ((tail(x, 1) - 1L) == max_age)) {
          out <- rep(out[2:length(out)], each = grp_width)
          out[1] <- "<1"
        }
      } else {
        if (match_input && ((tail(x, 1) - 1L) != max_age)) {
          out <- c(
            rep(out[1:((length(out) - 1L))], each = grp_width),
            rep(out[length(out)], match_input_max_age - tail(x, 1) + 1L)
          )
        }
        if (match_input && ((tail(x, 1) - 1L) == max_age)) {
          out <- rep(out, each = grp_width)
        }
      }
    } else {
      out <- paste0(sprintf("%02.0f", seq(min_age, max_age, grp_width)))
    }
    return(out)
  }


#' Replace multiple values in a data.table column
#'
#' `replace_from_table` replace multiple values in a data.table column.
#'  . The values in \code{from} argument are matched
#'    and replaced by those in \code{to} argument.
#'    If \code{newcolname = NULL} the replace is by reference.
#'
#' @param dtb A data.table to be changed by reference.
#' @param colname A string denoting the name of the column to be changed.
#' @param from A vector with values in \code{colname} to be replaced.
#' @param to A vector with values in \code{colname} to be replaced.
#' @param newcolname A string denoting the name of a new column
#'    to be created. If present, \code{colname} is not altered.
#'    If \code{newcolname = NULL}, \code{colname} is altered by reference
#' @return a data.table, invisibly.
#' @export
#' @examples
#' library(data.table)
#' library(CKutils)
#' dtb <- data.table::data.table("a" = 1:5, "b" = seq(1, 2.2, 0.3),
#'  "d" = letters[1:5])
#' dtb[, e := factor(a, labels = LETTERS[1:5])]
#' replace_from_table(data.table::copy(dtb), "a", 1:3, 3L)[]
#' replace_from_table(data.table::copy(dtb), "a", 3L, -11L)[]
#' replace_from_table(data.table::copy(dtb), "a", 3L, -11L, "newcol")[]
#' replace_from_table(data.table::copy(dtb), "b", 1.3, "a")[]
#' replace_from_table(data.table::copy(dtb), "b", 1.3, "a", "newcol")[]
#' replace_from_table(data.table::copy(dtb), "d", "a", "7")[]
#' replace_from_table(data.table::copy(dtb), "d", "a", 7)[]
#' replace_from_table(data.table::copy(dtb), "e", "B", "J")[]
replace_from_table <-
  function(dtb, colname, from, to, newcolname = NULL) {
    old_ <- i.new_ <- NULL
    stopifnot(is.data.table(dtb))
    stopifnot(length(colname) == 1L)
    stopifnot(is.null(newcolname) | length(newcolname) == 1L)
    stopifnot(colname %in% names(dtb))
    stopifnot(length(from) >= length(to))
    # stopifnot(class(from) == dtb[, class(get(colname))]) # not working for factors
    if (!is.null(newcolname) && newcolname %in% names(dtb)) {
      stop(
        "The new column name already exists in the data.table."
      )
    }
    if (length(from) > length(to)) {
      message("Note: matched many to few.")
    }

    colorder <- copy(names(dtb))
    if (class(from) == class(to)) {
      reg <- data.table("old_" = from, "new_" = to)
      dtb[, "old_" := get(colname)]
      dtb[reg, on = "old_", old_ := i.new_]
      if (is.null(newcolname)) {
        dtb[, (colname) := NULL]
        setnames(dtb, "old_", colname)
        setcolorder(dtb, colorder)
      } else {
        setnames(dtb, "old_", newcolname)
      }
    } else {
      reg <- data.table("old_" = as(from, class(to)), "new_" = to)
      dtb[, "old_" := as(get(colname), class(to))]
      dtb[reg, on = "old_", old_ := i.new_]
      if (is.null(newcolname)) {
        message(paste0(
          colname,
          " coerced to ",
          class(to),
          " to match target class."
        ))
        dtb[, (colname) := NULL]
        setnames(dtb, "old_", colname)
        setcolorder(dtb, colorder)
      } else {
        setnames(dtb, "old_", newcolname)
      }
    }
    return(invisible(dtb))
  }


#' Generate age-group from age
#'
#' `to_agegrp` creates a new column
#'
#' @param dtb A data.table with a column named \code{age}.
#' @param grp_width The group width for the age groups.
#' @param max_age The max age for the closed age groups. For ages above the max age,
#'  an open age group will be created, named max_age+ (i.e. 85+).
#' @param age_colname A string denoting the age column in \code{dtb}.
#' @param agegrp_colname A string denoting the name of the column that will be
#'   created for age groups.
#' @param to_factor A logical. If \code{TRUE}, then the age-groups
#'   column is converted to factor.
#' @param min_age The minimum age for the age group. If `NULL` the minimum will be considered the that that is not more than the minimum age in the data that can be divided with grp_width
#' @return a data.table, invisibly.
#' @export
#' @examples
#' library(data.table)
#' library(CKutils)
#' to_agegrp(data.table(age = 0:99))[]
#' to_agegrp(data.table(age = 0:99), max_age = 80L)[]
#' to_agegrp(data.table(age = 0:99), grp_width = 10, max_age = 85)[]
to_agegrp <-
  function(
    dtb,
    grp_width = 5L,
    max_age = 85L,
    age_colname = "age",
    agegrp_colname = "agegrp",
    to_factor = TRUE,
    min_age = NULL
  ) {
    stopifnot(
      is.data.table(dtb),
      age_colname %in% names(dtb),
      length(age_colname) == 1L,
      length(agegrp_colname) == 1L,
      is.logical(to_factor)
    )
    max_age <-
      ifelse(is.null(max_age), max(dtb[[age_colname]]), max_age)
    lage <- min(dtb[[age_colname]])
    min_age <-
      ifelse(is.null(min_age), lage - lage %% grp_width, min_age)
    # Include all ages in the data, and ensure we cover at least up to max_age
    actual_max_age <- max(dtb[[age_colname]])
    effective_max_age <- max(actual_max_age, max_age)
    age_vec <- min_age:effective_max_age
    agegroups <- agegrp_name(
      min_age = min_age,
      max_age,
      grp_width = grp_width,
      match_input = TRUE,
      match_input_max_age = effective_max_age
    )

    replace_from_table(
      dtb,
      colname = age_colname,
      from = age_vec,
      to = agegroups,
      newcolname = agegrp_colname
    )
    # TODO better support of replace_from_table so I can convert agegroups vector
    # to factor, I.e. if (to_factor) agegroups <- factor(agegroups)
    if (to_factor) {
      dtb[, `:=`(
        (agegrp_colname),
        factor(get(agegrp_colname), sort(unique(agegroups)))
      )]
    }
    return(invisible(dtb))
  }


#' Calculate percentile rank
#'
#' `pctl_rank` calculates the percentile rank of a numeric vector
#' @param x A numeric vector to rank
#' @param ties.method A character string specifying how ties are treated
#' @export
#' @return The percentile rank of the \code{`x`} vector calculated according to the ties.method choosen
#' @examples
#' library(data.table)
#' library(CKutils)
#' x = c(2,5,1,3,4,6)
#' pctl_rank(x, ties.method="min") # min assigns every tied element to the lowest rank
pctl_rank <- function(
  x,
  ties.method = c("average", "first", "random", "max", "min", "dense")
) {
  stopifnot(is.numeric(x))
  ties.method <- match.arg(ties.method)
  n <- length(x)
  out <- (frank(x, na.last = F, ties.method = ties.method) - 1) / (n - 1)
  return(out)
}


#' Build Arrow Expression for membership filters
#'
#' @param field Character scalar column name or an arrow::Expression.
#' @param values Vector of values to match.
#'
#' @return An arrow::Expression suitable for use in read_parquet_dt().
#' @examples
#' \dontrun{
#' arrow_in("sex", c("men", "women"))
#' read_parquet_dt(
#'   "./data/myfile.parquet",
#'   filter = arrow_in("sex", "men")
#' )
#' }
#' @export
arrow_in <- function(field, values) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required but not installed.")
  }

  if (inherits(field, "Expression")) {
    field_expr <- field
  } else if (is.character(field) && length(field) == 1L) {
    field_expr <- arrow::Expression$field_ref(field)
  } else {
    stop("'field' must be a single column name or an arrow::Expression.")
  }

  values <- as.vector(values)
  if (length(values) < 1L) {
    stop("'values' must be a non-empty vector.")
  }

  # Use Arrow's native is_in function for O(1) lookup instead of O(n) chained ORs
  value_set <- arrow::Array$create(values)
  arrow::Expression$create(
    "is_in",
    field_expr,
    options = list(value_set = value_set, skip_nulls = TRUE)
  )
}


#' Write data.table to Parquet with key metadata preservation
#'
#' Writes a data.table (or reads from an fst file) to Parquet format while
#' preserving data.table key information in the parquet metadata. The keys
#' are stored as JSON in the `r.data.table.keys` metadata field and can be
#' restored when reading with [read_parquet_dt()].
#'
#' @param x Either a data.table object OR a character path to an fst file.
#'   If a path is provided, the fst file is read as a data.table.
#' @param path Character scalar. The output path for the parquet file or
#'   directory (if partitioning is used).
#' @param keys Specifies which columns to use as data.table keys. Can be:
#'   \itemize{
#'     \item `NULL` (default): Uses existing keys from the data.table
#'     \item A character vector of column names: Uses these as keys
#'     \item `"impactncd"`: Special mode for IMPACTncd project - automatically
#'       determines keys by excluding distribution parameters (mu, sigma, nu,
#'       tau, maxq, minq) and columns ending with digits, then sorts
#'       alphabetically with "year" first if present
#'   }
#' @param partitioning Optional character vector of column names to use for
#'   Hive-style partitioning. If `NULL` (default), writes a single parquet file.
#'   If specified, creates a directory structure with partition folders.
#'   If `TRUE`, automatically partitions by "year" if that column exists.
#' @param compression Character scalar. Compression codec to use.
#'   Default is "snappy". Other options include "gzip", "zstd", "lz4", "uncompressed".
#'
#' @details
#' The function stores data.table keys in the parquet schema metadata under
#' the field `r.data.table.keys` as a JSON-encoded character vector. This
#' metadata is read by [read_parquet_dt()] to automatically restore keys.
#'
#' When `partitioning` is specified, the data is written using
#' `arrow::write_dataset()` which creates a Hive-style partitioned directory.
#' Otherwise, `arrow::write_parquet()` is used for a single file.
#'
#' The `"impactncd"` key mode is designed for the IMPACTncd simulation project
#' where data tables typically have distribution parameters (mu, sigma, nu, tau)
#' and quantile columns that should not be used as keys.
#'
#' @return Invisibly returns the path to the written parquet file/directory.
#'
#' @seealso [read_parquet_dt()] for reading parquet files with key restoration.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#'
#' # Create a keyed data.table
#' dt <- data.table(id = 1:100, year = rep(2020:2024, 20), value = rnorm(100))
#' setkey(dt, id, year)
#'
#' # Write to parquet (keys preserved automatically)
#' write_parquet_dt(dt, "output.parquet")
#'
#' # Read back with keys restored
#' dt2 <- read_parquet_dt("output.parquet")
#' key(dt2)  # Returns c("id", "year")
#'
#' # Write with custom keys
#' write_parquet_dt(dt, "output.parquet", keys = c("id"))
#'
#' # Write with IMPACTncd key logic
#' dt_impact <- data.table(
#'   year = 2020:2024, age = 30:34, sex = "M",
#'   mu = rnorm(5), sigma = runif(5)
#' )
#' write_parquet_dt(dt_impact, "output.parquet", keys = "impactncd")
#' # Keys will be c("age", "sex", "year") - mu/sigma excluded, year moved to end
#'
#' # Write with partitioning by year
#' write_parquet_dt(dt, "output_dir", partitioning = "year")
#'
#' # Auto-partition by year if column exists
#' write_parquet_dt(dt, "output_dir", partitioning = TRUE)
#'
#' # Read from fst and write to parquet
#' write_parquet_dt("data.fst", "output.parquet", keys = c("id", "year"))
#' }
#'
#' @importFrom arrow arrow_table write_parquet write_dataset
#' @export
write_parquet_dt <- function(
    x,
    path,
    keys = NULL,
    partitioning = NULL,
    compression = "snappy"
) {
  # Input validation
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required but not installed.")
  }

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required but not installed.")
  }

  stopifnot(is.character(path), length(path) == 1L)

  # Handle input: data.table or fst file path
  if (is.character(x) && length(x) == 1L && file.exists(x)) {
    if (!requireNamespace("fst", quietly = TRUE)) {
      stop("Package 'fst' is required to read fst files but not installed.")
    }
    dt <- fst::read_fst(x, as.data.table = TRUE)
  } else if (is.data.frame(x)) {
    dt <- data.table::as.data.table(x)
  } else {
    stop("'x' must be a data.table, data.frame, or a path to an fst file.")
  }

  # Determine keys to use
  if (is.null(keys)) {
    # Use existing keys from the data.table
    kc <- data.table::key(dt)
  } else if (identical(keys, "impactncd")) {
    # IMPACTncd special key logic:
    # 1. Exclude distribution parameters and columns ending with digits
    # 2. Sort alphabetically
    # 3. Move "year" to the end if present
    excluded_cols <- c("mu", "sigma", "nu", "tau", "maxq", "minq")
    kc <- sort(
      names(dt)[
        !names(dt) %in% excluded_cols &
          !grepl("[0-9]$", names(dt))
      ]
    )
    # Move "year" to the end of keys (if present)
    if ("year" %in% kc) {
      kc <- c(setdiff(kc, "year"), "year")
    }
    # Reorder columns to match keys first
    data.table::setcolorder(dt, c(kc, setdiff(names(dt), kc)))
  } else if (is.character(keys)) {
    # User-specified keys
    missing_keys <- setdiff(keys, names(dt))
    if (length(missing_keys) > 0L) {
      stop(
        "Keys not found in data: ",
        paste(missing_keys, collapse = ", ")
      )
    }
    kc <- keys
  } else {
    stop("'keys' must be NULL, a character vector, or 'impactncd'.")
  }

  # Set keys on the data.table
  if (!is.null(kc) && length(kc) > 0L) {
    data.table::setkeyv(dt, kc)
  }

  # Convert to Arrow table
  tbl <- arrow::arrow_table(dt)

  # Add key metadata if keys exist
  if (!is.null(kc) && length(kc) > 0L) {
    meta <- tbl$metadata
    meta[["r.data.table.keys"]] <- jsonlite::toJSON(kc, auto_unbox = FALSE)
    tbl <- tbl$ReplaceSchemaMetadata(meta)
  }

  # Handle partitioning argument
  if (isTRUE(partitioning)) {
    # Auto-partition by year if column exists
    if ("year" %in% names(dt)) {
      partitioning <- "year"
    } else {
      partitioning <- NULL
    }
  } else if (!is.null(partitioning) && !is.character(partitioning)) {
    stop("'partitioning' must be NULL, TRUE, or a character vector of column names.")
  }

  # Write to parquet
  if (!is.null(partitioning)) {
    # Validate partitioning columns exist
    missing_parts <- setdiff(partitioning, names(dt))
    if (length(missing_parts) > 0L) {
      stop(
        "Partitioning columns not found in data: ",
        paste(missing_parts, collapse = ", ")
      )
    }
    arrow::write_dataset(
      dataset = tbl,
      path = path,
      format = "parquet",
      partitioning = partitioning
    )
  } else {
    arrow::write_parquet(
      x = tbl,
      sink = path,
      compression = compression
    )
  }

  invisible(path)
}


#' Read Parquet (partitioned dataset or single file) via Arrow, optionally filter/project, return data.table
#'
#' @param path Character scalar (directory or file) OR character vector of parquet files.
#' @param cols Optional character vector of columns to keep (projection). NULL keeps all.
#' @param filter Optional row filter as an arrow::Expression.
#'   Use `arrow::Expression$field_ref("colname")` to reference columns.
#'   Example: `arrow::Expression$field_ref("year") >= 2018`
#' @param partitioning Partitioning spec for datasets. Default "hive" is typical for col=value/ layouts.
#'   If opening fails with this, the function automatically retries without partitioning.
#' @param as_data_table Logical; if TRUE returns data.table; if FALSE returns data.frame.
#' @param keys_fallback Optional character vector of column names to use as data.table keys
#'   if no key metadata is found in the parquet file. This allows specifying default keys
#'   when reading parquet files that were not written with data.table key information.
#'   The keys are only applied if all specified columns exist in the result.
#'
#' @details
#' When `as_data_table = TRUE`, the function attempts to restore data.table keys
#' from parquet metadata. Keys are stored in the parquet file's metadata under
#' the field `r.data.table.keys` as a JSON-encoded character vector (written by
#' functions like `arrow::write_parquet()` when custom metadata is provided).
#'
#' The key restoration logic follows this priority:
#' \enumerate{
#'   \item Read keys from parquet metadata field `r.data.table.keys` (JSON-encoded)
#'   \item If no metadata keys found and `keys_fallback` is provided, use `keys_fallback`
#'   \item Keys are only applied if all key columns exist in the resulting data.table
#' }
#'
#' @return data.table (default) or data.frame
#' @examples
#' \dontrun{
#' # Single file:
#' dt <- read_parquet_dt("./data/myfile.parquet")
#'
#' # Partitioned dataset directory with column selection:
#' dt <- read_parquet_dt(
#'   "./inputs/exposure_distributions/smok_status_table",
#'   cols = c("patid", "year", "smoke_status")
#' )
#'
#' # With filter (requires arrow expressions):
#' dt <- read_parquet_dt(
#'   "./data/myfile.parquet",
#'   filter = arrow::Expression$field_ref("age") >= 40
#' )
#'
#' # With fallback keys (applied if parquet has no key metadata):
#' dt <- read_parquet_dt(
#'   "./data/myfile.parquet",
#'   keys_fallback = c("patid", "year")
#' )
#' }
#' @importFrom arrow open_dataset
#' @export
read_parquet_dt <- function(
  path,
  cols = NULL,
  filter = NULL,
  partitioning = "hive",
  as_data_table = TRUE,
  keys_fallback = NULL
) {
  stopifnot(is.character(path), length(path) >= 1L)
  if (!is.null(cols)) {
    stopifnot(is.character(cols), length(cols) >= 1L)
  }

  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required but not installed.")
  }
  if (as_data_table && !requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }

  # Open dataset (directory, file, or vector of files)
  ds <- tryCatch(
    open_dataset(path, format = "parquet", partitioning = partitioning),
    error = function(e) {
      open_dataset(path, format = "parquet")
    }
  )

  # Use ScannerBuilder for projection and filtering

  scan_builder <- ds$NewScan()

  if (!is.null(cols)) {
    scan_builder$Project(cols)
  }

  if (!is.null(filter)) {
    if (!inherits(filter, "Expression")) {
      stop("'filter' must be an arrow::Expression.")
    }
    scan_builder$Filter(filter)
  }

  scanner <- scan_builder$Finish()
  tab <- scanner$ToTable()
  df <- as.data.frame(tab)

  if (as_data_table) {
    # Attempt to restore data.table keys from parquet metadata
    keys <- NULL

    # Try to read keys from metadata (stored as JSON-encoded character vector)
    keys <- tryCatch(
      jsonlite::fromJSON(ds$metadata[["r.data.table.keys"]]),
      error = function(e) NULL
    )
    
    # Ensure keys is a character vector (fromJSON might return list or other types)
    if (!is.null(keys)) {
      if (is.list(keys)) {
        keys <- unlist(keys)
      }
      if (!is.character(keys) || length(keys) == 0L) {
        keys <- NULL
      }
    }

    # Fallback if metadata not found (optional)
    if (is.null(keys) && !is.null(keys_fallback)) {
      keys <- keys_fallback
    }

    # Validate that all key columns exist in the result before applying
    if (!is.null(keys) && length(keys) > 0L && !all(keys %in% names(df))) {
      keys <- NULL  # Reset keys if not all columns present
    }

    setDT(df, key = keys)
  }

  df
}


#' Stochastic prediction from a gamlss object
#'
#' `validate_gamlss` returns a data.table with the observed and predicted
#' variable. Multiple predictions are drawn from the predicted
#' distributions. Useful for plotting with ggplot.
#'
#' @param dtb A data.table from which come the observed variables
#' @param gamlss_obj A gamlss object
#' @param mc Number of Monte Carlo simulations, by default 10L
#' @param orig_data Initial data.table, defaults to \code{dtb}
#' @return A data.table with observed and predicted variables
#' @export
validate_gamlss <- function(dtb, gamlss_obj, mc = 10L, orig_data = dtb) {
  if (!requireNamespace("gamlss", quietly = TRUE)) {
    stop("Please install package gamlss first.")
  }
  stopifnot(
    "gamlss" %in% class(gamlss_obj),
    is.data.table(dtb),
    mc >= 1,
    is.data.table(orig_data)
  )
  nam_y <- as.character(gamlss_obj$call$formula[[2]])
  nam_var <- all.vars(gamlss_obj$call$formula[[3]])
  nam_dist <- paste0("r", gamlss_obj$family[[1]])
  nam_param <- gamlss_obj$parameters
  x <- copy(dtb)
  x[, type := "Observed"]
  z <- copy(dtb)
  z[,
    (nam_param) := gamlss::predictAll(
      gamlss_obj,
      type = "response",
      newdata = dtb[, .SD, .SDcols = nam_var],
      data = orig_data[, .SD, .SDcols = c(nam_var)]
    )
  ]
  z[, type := "Modelled"]
  z <- rbindlist(rep(list(z), mc))
  z[, (nam_y) := do.call(nam_dist, c(.N, .SD)), , .SDcols = nam_param]
  out <- rbind(x, z, use.names = TRUE, fill = TRUE)
  out[, (nam_param) := NULL]
}

# TODO add documentation
# If I name the function predict_gamlss CKutils:: is necessary when I
# call the function.
#' Prediction from a gamlss object in parallel
#'
#' `guess_gamlss` returns a data.table with the predicted
#'  variable. `dtb` needs to have a column with percentiles named `rank_y`,
#'  where `y` the name of the predicted variable (i.e. bmi).
#' @param dtb A data.table
#' @param gamlss_obj gamlss object
#' @param orig_data original data.table
#' @param nc by default = 1L
#' @export
guess_gamlss <- function(
  dtb,
  gamlss_obj,
  orig_data = gamlss_obj$data,
  nc = 1L
) {
  if (!requireNamespace("gamlss", quietly = TRUE)) {
    stop("Please install package gamlss first.")
  }
  stopifnot(
    "gamlss" %in% class(gamlss_obj),
    is.data.table(dtb),
    nc >= 1L,
    is.data.table(orig_data)
  )
  nam_y <- as.character(gamlss_obj$call$formula[[2]])
  nam_var <- all.vars(gamlss_obj$call$formula[[3]])
  nam_dist <- paste0("q", gamlss_obj$family[[1]])
  nam_param <- gamlss_obj$parameters

  orig_data <- orig_data[, ..nam_var]
  dtu <- unique(dtb[, ..nam_var]) # otherwise too slow
  dtu <- split(dtu, dtu$year)
  if ("RevoUtilsMath" %in% (.packages())) {
    tt <- get("getMKLthreads", mode = "function")()
  }
  if ("RevoUtilsMath" %in% (.packages())) {
    get("setMKLthreads", mode = "function")(1L)
  }
  dtu <- parallel::mclapply(
    dtu,
    function(x) {
      x[,
        (nam_param) := gamlss::predictAll(
          gamlss_obj,
          type = "response",
          newdata = .SD,
          data = orig_data
        )
      ]
    },
    mc.preschedule = FALSE,
    mc.cores = nc
  )
  if ("RevoUtilsMath" %in% (.packages())) {
    get("setMKLthreads", mode = "function")(tt)
  }
  dtu <- rbindlist(dtu)
  # dtu[, (nam_param) := gamlss::predictAll(gamlss_obj,
  #                                        type = "response",
  #                                        newdata = .SD,
  #                                        data = orig_data)]
  dtb[dtu, on = nam_var, (nam_param) := mget(paste0("i.", nam_param))]
  dtb[, p := get(paste0("rank_", nam_y))]
  stopifnot(dt[, all(between(p, 0, 1, incbounds = FALSE))])
  dtb[, (nam_y) := do.call(nam_dist, .SD), .SDcols = c("p", nam_param)]
  dtb[, c("p", nam_param) := NULL]
}


#' Predict ordinal outcomes from a polr model
#'
#' This function generates predictions from a polr (proportional odds logistic regression) model for ordinal data.
#' It computes the linear predictor from the input data.table using the model matrix of the provided polr object,
#' applies the logistic transformation to the model thresholds, and determines the predicted category as the sum of
#' thresholds that are less than a reference value. The predicted ordinal outcome replaces the original outcome column
#' in the data.table.
#'
#' @param dtb A data.table containing the predictor variables used for prediction and a column with a name
#' corresponding to the outcome variable prefixed with 'rank_'.
#' @param polr_obj An object of class \code{polr} from the MASS package representing the fitted ordinal regression model.
#'
#' @return The function modifies the input data.table \code{dtb} in place by replacing the outcome column with the predicted
#' ordinal category. It returns \code{NULL} invisibly.
#'
#' @details This function requires the \code{MASS} and \code{matrixStats} packages.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(MASS)
#' library(matrixStats)
#' library(data.table)
#'
#' # Assuming polr_model is a fitted polr object and dtb is your data.table 
#' # with predictors and a column named, for example, 'rank_y'
#' guess_polr(dtb, polr_model)
#' }
guess_polr <- function(dtb, polr_obj) {
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Please install package MASS first.")
  }
  if (!requireNamespace("matrixStats", quietly = TRUE)) {
    stop("Please install package matrixStats first.")
  }
  stopifnot("polr" %in% class(polr_obj), is.data.table(dtb))
  nam_y <- as.character(polr_obj$call$formula[[2]])
  nam_var <- all.vars(polr_obj$call$formula[[3]])
  #code adapted from method getAnywhere(predict.polr)
  Terms <- delete.response(polr_obj$terms)
  m <- model.frame(
    Terms,
    dtb[, ..nam_var],
    na.action = function(x) x,
    xlev = polr_obj$xlevels
  )
  if (!is.null(cl <- attr(Terms, "dataClasses"))) {
    .checkMFClasses(cl, m)
  }
  X <- model.matrix(Terms, m, contrasts = polr_obj$contrasts)
  xint <- match("(Intercept)", colnames(X), nomatch = 0L)
  if (xint > 0L) {
    X <- X[, -xint, drop = FALSE]
  }
  n <- nrow(X)
  q <- length(polr_obj$zeta)
  eta <- drop(X %*% polr_obj$coefficients)
  cc <- plogis(
    matrix(polr_obj$zeta, n, q, byrow = TRUE) -
      eta
  )
  dtb[, p := get(paste0("rank_", nam_y))]
  dtb[, (nam_y) := matrixStats::rowSums2(cc < p)]
  dtb[, "p" := NULL]
}


# TODO add documentation
#' Deterministic prediction from a gamlss object
#'
#' `crossval_gamlss` returns the observed and predicted values of the dependent
#'  variable. Useful for cross-validation metrics.
#' @param dtb A data. table
#' @param gamlss_obj a gamlss object
#' @param orig_data original data.table
#' @param colnam column names
#' @export
crossval_gamlss <- function(dtb, gamlss_obj, orig_data = dtb, colnam = "rank") {
  stopifnot(
    "gamlss" %in% class(gamlss_obj),
    is.data.table(dtb),
    is.data.table(orig_data)
  )
  out <- list()
  nam_y <- as.character(gamlss_obj$call$formula[[2]])
  nam_var <- all.vars(gamlss_obj$call$formula[[3]])
  nam_dist <- paste0("q", gamlss_obj$family[[1]])
  nam_param <- gamlss_obj$parameters
  out$observed <- dtb[, get(nam_y)]
  z <- copy(dtb)
  z[,
    (nam_param) := gamlss::predictAll(
      gamlss_obj,
      type = "response",
      newdata = dtb[, .SD, .SDcols = nam_var],
      data = orig_data[, .SD, .SDcols = c(nam_y, nam_var)]
    )
  ]
  setnames(z, colnam, "p")
  z[p == 0, p := 0.0001]
  z[p == 1, p := 0.9999]
  z[, (nam_y) := do.call(nam_dist, .SD), .SDcols = c("p", nam_param)]
  out$predicted <- z[, get(nam_y)]
  return(out)
}

#' Generate Counts of Values in a Vector
#'
#' This function uses Rcpp sugar to implement a fast \code{table}, for
#' unique counts of a single vector. This implementation seeks to
#' produce identical output to \code{table(x, useNA="ifany")}. It is borrowed
#' from \code{Kmisc} package for convenience, since \code{Kmisc} is not in CRAN
#'  anymore. \code{Kmisc} is available at https://github.com/kevinushey/Kmisc

#'
#' The order of \code{NA}, \code{NaN} in the output may differ -- even
#' \R is inconsistent with the order that \code{NA} and \code{NaN} elements
#' are inserted.
#'
#' @param x A numeric, integer, character or logical vector, or a (potentially
#'   nested) list of such vectors. If \code{x} is a list, we recursively apply
#'   \code{counts} throughout elements in the list.
#' @export
#' @examples
#' x <- round( rnorm(1E2), 1 )
#' counts(x)
counts <- function(x) {
  if (is.list(x)) {
    output <- rapply(x, counts, how = "list")
    return(output)
  } else {
    return(.Call("_CKutils_counts", x))
  }
}


#' Obtain matching names corresponding to patterns
#'
#' `match_colnames_pattern` returns the matching names of the argument `dtb`
#' (i.e. \code{names(dtb)}) corresponding to the regular expression patterns
#' provided. The patterns must be supported by \code{\link{grep}}.
#' This is based on `data.table:::patterns`
#' @param dtb A data.table from which the column names \code{names(dtb)} or \code{colnames(dtb)} will be tested
#' @param ... A list of the names to match with the data.table ones. Needs to be made of character otherwise the function stops
#' @export
#' @examples
#' library(data.table)
#' library(CKutils)
#' dtb <- data.table(id = c("city", "year", "birth", "idp"), b = c("age", "year", "bp", "name"))
#' z <- list("id", "year", "b")
#' match_colnames_pattern(dtb, z) #[1] "id" "b"
match_colnames_pattern <- function(dtb, ...) {
  p = unlist(list(...), use.names = FALSE)
  if (!is.character(p)) {
    stop("Input patterns must be of type character.")
  }
  cols = names(dtb)
  cols[unlist(sapply(p, grep, cols))]
}

# TODO add documentation
#' Compare two distributions
#'
#' `reldist_diagnostics` Summary statistics for the location/shape decomposition of the relative
#' distribution of the exposure: Modelled to Observed."
#' @param comparison A comparison
#' @param reference a reference
#' @param comparison_wt a comparison
#' @param reference_wt a reference
#' @param main the main part
#' @param smooth smooth object
#' @param discrete discrete value
#' @export
reldist_diagnostics <- function(
  comparison,
  reference,
  comparison_wt,
  reference_wt,
  main,
  smooth = 0.35,
  discrete = FALSE
) {
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  if (!requireNamespace("reldist", quietly = TRUE)) {
    stop(
      "Package \"reldist\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  reference_dens <- density(reference, weights = reference_wt)
  comparison_dens <- density(comparison, weights = comparison_wt)

  par(mfrow = c(2, 2))
  plot(
    reference_dens,
    main = main,
    lty = 3,
    ylim = c(
      0,
      1.1 *
        max(c(
          reference_dens$y,
          comparison_dens$y
        ))
    )
  )
  lines(comparison_dens, col = "red", lty = 2)
  legend(
    "topright",
    bg = "transparent",
    legend = c("Comparison", "Reference"),
    box.lty = 0,
    col = c("red", "black"),
    lty = 2:3,
    cex = 0.8
  )
  g10 <- reldist::reldist(
    y = comparison,
    yo = reference,
    smooth = smooth,
    ci = TRUE,
    ywgt = comparison_wt,
    yowgt = reference_wt,
    #ylim=c(0,1.5),
    #yolabs=seq(-1,3,by=0.5),
    bar = "yes",
    quiet = FALSE,
    discrete = discrete,
    xlab = "proportion of the reference cohort"
  )
  title(main = "Overall relative density", cex = 0.6)
  abline(h = 1, lty = 2)
  g1A <- reldist::reldist(
    y = comparison,
    yo = reference,
    ywgt = comparison_wt,
    yowgt = reference_wt,
    show = "effect",
    bar = "yes",
    quiet = FALSE,
    smooth = smooth,
    ci = TRUE,
    discrete = discrete,
    #ylim=c(0,1.5),
    #yolabs=seq(-1,3,by=0.5),
    xlab = "proportion of the reference cohort"
  )
  title(main = "Effect of the median shift", cex = 0.6)
  abline(h = 1, lty = 2)
  gA0 <- reldist::reldist(
    y = comparison,
    yo = reference,
    smooth = smooth,
    ci = TRUE,
    ywgt = comparison_wt,
    yowgt = reference_wt,
    show = "residual",
    bar = "yes",
    quiet = FALSE,
    discrete = discrete,
    #ylim=c(0,1.5),
    #yolabs=seq(-1,3,by=0.5),
    xlab = "proportion of the reference cohort"
  )
  title(main = "Median-adjusted relative density", cex = 0.6)
  abline(h = 1, lty = 2)

  a1 <- reldist::rpy(
    y = comparison,
    yo = reference,
    ywgt = comparison_wt,
    yowgt = reference_wt,
    pvalue = TRUE
  )
  a2 <- reldist::rpluy(
    y = comparison,
    yo = reference,
    ywgt = comparison_wt,
    yowgt = reference_wt,
    pvalue = TRUE
  )
  a3 <- reldist::rpluy(
    y = comparison,
    yo = reference,
    ywgt = comparison_wt,
    yowgt = reference_wt,
    pvalue = TRUE,
    upper = TRUE
  )
  # p1 <- ifelse(a1[[4]]<0.001, "<0.001", format(a1[[4]], digits = 3))
  # p2 <- ifelse(a2[[4]]<0.001, "<0.001", format(a2[[4]], digits = 3))
  # p3 <- ifelse(a3[[4]]<0.001, "<0.001", format(a3[[4]], digits = 3))

  out <- data.table(
    "Summary statistics" = c(
      "Overall change entropy",
      "Median effect entropy",
      "Shape effect entropy",
      "Median polarization index",
      "Lower polarization index",
      "Upper polarization index"
    ),
    "Measure" = c(
      g10$entropy,
      reldist::entropy(g1A, g10),
      gA0$entropy,
      a1[[2]],
      a2[[2]],
      a3[[2]]
    ),
    "Lower 95% CI" = c(NA, NA, NA, a1[[1]], a2[[1]], a3[[1]]),
    "Upper 95% CI" = c(NA, NA, NA, a1[[3]], a2[[3]], a3[[3]]),
    "p-value" = c(NA, NA, NA, a1[[4]], a2[[4]], a3[[4]])
  )
  return(out[])
}


# TODO add documentation
# Scrambles rank trajectories of simulants
#
# Scrambles the rank trajectories using a continuous space random walk. The \code{jump} parameter defines the maximum distance of jump every year
#
# scramble_trajectories <- function(x, pid, jump = 0.05) {
#   if (all(x < 0 | x > 1))
#     stop("Input needs to be between 0 and 1")
#   if (is.unsorted(pid))
#     stop("IDs must be sorted")
#   if (jump >= 1)
#     stop("Overlap needs to be <= 1")
#   if (jump == 0) return(x) else return(fscramble_trajectories(x, pid, jump))
# }

# tt <- data.table(x = runif(5e5), pid = 1:5e5)
# tt <- CKutils::clone_dt(tt, 50)
# setkey(tt, pid, .id)
# tt[, y := scramble_trajectories(x, pid, 0.05)]
# tt[998:1003]
# print(tt[sample(.N, 1e4), ggplot2::qplot(x, y, alpha = I(1/20))])
# print(tt[.id == 50, hist(y)])
# print(tt[.id == 50 & y > 0.9, hist(y)])
# print(tt[.id == 50 & y < 0.1, hist(y)])
# print(tt[pid == 4,  plot(.id, y, ylim = c(0,1))])

# estimate beta params from mean and variance
#' `estim_beta_params` estimates the beta parameters from a mean and a variance given
#' @param mu An integer between 0 and 1
#' @param var A positive integer
#' @export
#' @return A list made of the beta and alpha parameters calculated, called shape1 and shape2
#' @examples
#' estim_beta_params(0.6, 5) #6.0006e-05   4.0004e-05
estim_beta_params <- function(mu, var) {
  # from https://stats.stackexchange.com/questions/12232/calculating-the-parameters-of-a-beta-distribution-using-the-mean-and-variance and wikipedia
  stopifnot(between(mu, 0, 1), var > 0)
  if (var >= (mu * (1 - mu))) {
    var <- mu * (1 - mu) * 0.9999
  }
  alpha <- mu * ((mu * (1 - mu) / var) - 1) # if var < (mu * (1 - mu))
  beta <- (1 - mu) * ((mu * (1 - mu) / var) - 1) # var < (mu * (1 - mu))
  return(params = list(shape1 = alpha, shape2 = beta))
}

# Define outersect. Like setdiff but symmetrical. I.e. setdiff(a,b) is not the
# same as setdiff(b,a). outersect solve this by calculating both
#' `outersect` Calculates the symmetrical set difference of subsets
#' @param x,y vectors, data frames containing a sequence of items
#' @param ... further arguments to be passed to or from other methods
#' @export
#' @return A vector made of both contents from x and y, except from the duplicated items
#' @examples
#' x <- c("age", "year", "bp", "name")
#' y <- c("city", "year", "birth", "id")
#' outersect(x, y)
#' #"age" "bp" "name" "city" "birth" "id"
outersect <-
  function(x, y, ...) {
    big.vec <- c(x, y, ...)
    duplicates <- big.vec[duplicated(big.vec)]
    setdiff(big.vec, unique(duplicates))
  }


#' Calculate Symmetric Difference Between Two Sets
#'
#' `symdiff` returns the symmetric difference between two vectors. The symmetric
#' difference consists of elements that are in either x or y, but not in both.
#' This is equivalent to the union of the two sets minus their intersection.
#'
#' @param x A vector (numeric, character, logical, etc.)
#' @param y A vector of the same type as x
#'
#' @return A vector containing elements that are in either x or y, but not in both
#'
#' @export
#' @examples
#' # With numeric vectors
#' symdiff(c(1, 2, 3, 4), c(3, 4, 5, 6))  # Returns c(1, 2, 5, 6)
#'
#' # With character vectors
#' symdiff(c("a", "b", "c"), c("b", "c", "d"))  # Returns c("a", "d")
#'
#' # With logical vectors
#' symdiff(c(TRUE, FALSE), c(FALSE))  # Returns TRUE
#'
#' # Empty result when sets are identical
#' symdiff(c(1, 2, 3), c(1, 2, 3))  # Returns integer(0)
symdiff <- function(x, y) {
  setdiff(union(x, y), intersect(x, y))
}

# Define function for sampling. Taken from sample man pages
#' `resample` Gives sample of from the elements of \code{`x`} of the specified size. Both size and \code{`x`} has to be integers
#' @param x A vector to be sampled
#' @param ... Eventual condition for the vectorisation
#' @export
#' @return Sample(s) from the \code{`x`} vector, according to eventual conditions provided
#' @examples
#' x <- 1:10
#' resample(x[x >  8]) # length 2
#' resample(x[x >  9]) # length 1
#' resample(x[x > 10]) # length 0
resample <-
  function(x, ...) {
    x <- na.omit(x)
    x[sample.int(length(x), ...)]
  }


#' Check if All Elements in a Numeric Vector Are Identical
#'
#' This function checks whether all elements in a numeric vector are equal within a specified tolerance.
#' It returns TRUE if all elements are identical (considering the tolerance), and FALSE otherwise.
#'
#' @param x A numeric vector to test.
#' @param tol A numeric tolerance used for the comparison (default: .Machine$double.eps^0.5).
#'
#' @return A logical value indicating whether all elements in \code{x} are identical.
#'
#' @export
identical_elements <-
  function(x, tol = .Machine$double.eps^0.5) {
    stopifnot(is.numeric(x))
    fequal(x, tol)
  }


#' Normalise a Vector to the 0-1 Range
#'
#' This function normalises a numeric vector so that its values are scaled to lie within the 0 to 1 range.
#' If all elements in the vector are identical, the function returns 1.
#'
#' @param x A numeric vector to be normalised.
#' @param ... Additional arguments (currently unused).
#'
#' @return A numeric vector with values scaled between 0 and 1, or a single value 1 if all elements are identical.
#'
#' @export
normalise <-
  function(x, ...) {
    stopifnot(is.numeric(x))
    if (identical_elements(x)) {
      return(1)
    } else {
      return(fnormalise(x))
    }
  }

#' Convert CSV Files to FST Format
#'
#' This function converts one or more CSV files to the FST file format. It reads each CSV file using \code{data.table::fread} and writes it as an FST file using \code{fst::write_fst}. Optionally, the original CSV file(s) can be deleted.
#'
#' @param csv_files A character vector of file paths to CSV files.
#' @param compression An integer specifying the compression level for the FST file. Default is 100L.
#' @param delete_csv Logical. If TRUE, deletes the original CSV file(s) after conversion. Default is FALSE.
#'
#' @return Invisibly returns NULL.
#'
#' @examples
#' \dontrun{
#' csv_to_fst(c("data.csv"), compression = 100L, delete_csv = TRUE)
#' }
#' @export
csv_to_fst <- function(csv_files, compression = 100L, delete_csv = FALSE) {
  hlpfn <- function(nam, compression) {
    # input scalar string
    out <- data.table::fread(nam)
    new_nam <- gsub(".csv$", ".fst", nam)
    fst::write_fst(out, new_nam, compress = compression)
  }
  lapply(csv_files, hlpfn, compression)
  if (delete_csv) {
    file.remove(csv_files)
  }
  return(invisible(NULL))
}


#' Clamp values to a specified range
#'
#' `clamp` limits the values in a numeric vector `x` so that all elements are constrained within the interval [a, b].
#' It dispatches to either `fclamp` for double precision values or `fclamp_int` for integer values.
#'
#' @param x A numeric vector of values to be clamped.
#' @param a A numeric vector specifying the lower bound (default: 0).
#' @param b A numeric vector specifying the upper bound (default: 1).
#' @param inplace Logical. If TRUE, the clamping operation is performed in-place; otherwise, a new vector is returned.
#'
#' @return A numeric vector with values clamped to the interval [a, b].
#' @export
clamp <- function(x, a = 0, b = 1, inplace = FALSE) {
  stopifnot(is.numeric(a), is.numeric(b), is.logical(inplace))
  typ <- typeof(x)
  if (typ == "double") {
    return(fclamp(x, a, b, inplace))
  } else if (typ == "integer") {
    return(fclamp_int(x, as.integer(a), as.integer(b), inplace))
  } else {
    stop("clamp() only accepts doubles or integers")
  }
}

#' Generate folder structure
#'
#' `gnrt_folder_structure` generates the folder structure for an IMPACTncd model
#'
#' This is an auxilliary function: It creates the expected folder structure for
#' an IMPACTncd model.
#'
#' @param path A string scalar. The root folder where the folder structure will
#' be created.
#' @return NULL
#' @export

gnrt_folder_structure <- function(path = getwd()) {
  fldr_strc <- list(
    "inputs" = file.path(path, "inputs"),
    "processed_inputs" = file.path(path, "processed_inputs"),
    "simulation" = file.path(path, "simulation"),
    "outputs" = file.path(path, "outputs"),
    "validation" = file.path(path, "validation"),
    "gui" = file.path(path, "gui"),
    "logs" = file.path(path, "logs")
  )

  fldr_strc$inputs <- list(
    "settings" = file.path(fldr_strc$inputs, "settings"),
    "open_data" = file.path(fldr_strc$inputs, "open_data"),
    "secure_data" = file.path(fldr_strc$inputs, "secure_data")
  )

  fldr_strc$processed_inputs <- list(
    "exposures" = file.path(fldr_strc$processed_inputs, "exposures"),
    "disease_epi" = file.path(fldr_strc$processed_inputs, "disease_epi")
  )

  fldr_strc$inputs$open_data <- list(
    "RR" = file.path(fldr_strc$inputs$open_data, "RR"),
    "population" = file.path(fldr_strc$inputs$open_data, "population")
  )

  NULL
}


#' Shift Values by Groups (Panel Data Lag/Lead)
#'
#' `shift_bypid` performs a lag or lead operation on a vector while respecting 
#' group boundaries defined by an ID variable. This is particularly useful for 
#' panel data where you want to shift values within each individual/group but 
#' not across different groups.
#'
#' @param x A vector to be shifted. Can be numeric (double), integer, logical, 
#'   character, or factor.
#' @param lag An integer specifying the number of periods to shift. Positive 
#'   values create lags (shift backward), negative values create leads (shift forward).
#' @param id An integer vector of the same length as `x` that defines the groups. 
#'   This should be sorted to ensure proper functionality.
#' @param replace The value used to replace positions where no valid shifted 
#'   value exists (e.g., first `lag` observations in each group). Defaults to `NA`. 
#'   The type should be compatible with the type of `x`.
#'
#' @details
#' The function dispatches to different C++ implementations based on the type of `x`:
#' \itemize{
#'   \item For `double` vectors: uses `shift_bypidNum`
#'   \item For `integer` vectors (non-factor): uses `shift_bypidInt`
#'   \item For `factor` vectors: uses `shift_bypidInt` and preserves factor levels
#'   \item For `logical` vectors: uses `shift_bypidBool`
#'   \item For `character` vectors: uses `shift_bypidStr`
#' }
#'
#' The algorithm works by:
#' 1. For the first `lag` positions of each group, filling with `replace` value
#' 2. For subsequent positions, checking if the current ID matches the ID at position `i-lag`
#' 3. If IDs match (same group), copying the value from `x[i-lag]`
#' 4. If IDs don't match (different group), using the `replace` value
#'
#' **Important**: The `id` vector should be sorted to ensure correct behaviour. 
#' Unsorted IDs may lead to unexpected results.
#'
#' @return A vector of the same type and length as `x`, with values shifted 
#'   according to the specified lag and group structure. Factor attributes 
#'   are preserved for factor inputs.
#'
#' @examples
#' library(data.table)
#' 
#' # Create sample panel data
#' dtb <- data.table(
#'   id = rep(1:3, each = 4),
#'   time = rep(1:4, 3),
#'   value = 1:12
#' )
#' 
#' # Add lagged values (lag = 1)
#' dtb[, value_lag1 := shift_bypid(value, lag = 1, id = id)]
#' 
#' # Add leading values (lag = -1)  
#' dtb[, value_lead1 := shift_bypid(value, lag = -1, id = id)]
#' 
#' # Works with different data types
#' dtb[, char_var := letters[1:12]]
#' dtb[, char_lag1 := shift_bypid(char_var, lag = 1, id = id, replace = "missing")]
#' 
#' @export
shift_bypid <-
  function(x, lag, id, replace = NA) {
    # Input validation
    if (!is.vector(x) && !is.factor(x)) {
      stop("Argument 'x' must be a vector or factor")
    }
    if (!is.numeric(lag)) {
      stop("Argument 'lag' must be numeric")
    }
    if (length(lag) != 1L) {
      stop("Argument 'lag' must be a single value (scalar)")
    }
    if (!is.vector(id)) {
      stop("Argument 'id' must be a vector")
    }
    if (!is.numeric(id)) {
      stop("Argument 'id' must be numeric")
    }
    if (length(x) != length(id)) {
      stop("Arguments 'x' and 'id' must have the same length")
    }

    # Handle empty vectors early
    if (length(x) == 0L) {
      return(x)
    }

    # Check for NAs in id - this would cause undefined behavior in C++
    if (anyNA(id)) {
      stop("Argument 'id' must not contain NA values")
    }

    if (is.unsorted(id)) {
      stop("Argument 'id' must be sorted")
    }

    # Validate replace parameter
    if (length(replace) != 1L) {
      stop("Argument 'replace' must be a single value (scalar)")
    }

    # Convert lag to integer
    lag <- as.integer(lag)

    # Convert id to integer if not already
    if (!is.integer(id)) {
      id <- as.integer(id)
    }

    if (lag == 0L) return(x)

    # Type-specific handling with proper replace coercion
    if (typeof(x) == "integer" && !inherits(x, "factor")) {
      replace_val <- if (is.na(replace)) NA_integer_ else as.integer(replace)
      return(shift_bypidInt(x, lag, replace_val, id))
    } else if (typeof(x) == "integer" && inherits(x, "factor")) {
      replace_val <- if (is.na(replace)) NA_integer_ else as.integer(replace)
      int_result <- shift_bypidInt(as.integer(x), lag, replace_val, id)
      return(factor(int_result, levels = seq_along(levels(x)), labels = levels(x)))
    } else if (typeof(x) == "logical") {
      # For logical, replace must be logical vector (C++ expects LogicalVector)
      replace_val <- if (is.na(replace)) NA else as.logical(replace)
      return(shift_bypidBool(x, lag, replace_val, id))
    } else if (typeof(x) == "double") {
      replace_val <- if (is.na(replace)) NA_real_ else as.double(replace)
      return(shift_bypidNum(x, lag, replace_val, id))
    } else if (typeof(x) == "character") {
      replace_val <- if (is.na(replace)) NA_character_ else as.character(replace)
      return(shift_bypidStr(x, lag, replace_val, id))
    } else
      stop("type of x not supported")
  }


#' Generate Correlated Uniform Random Variables
#'
#' `generate_corr_unifs` generates a matrix of correlated uniform random variables
#' from a given correlation matrix. The function transforms the correlation structure
#' to account for the uniform distribution and uses the Cholesky decomposition method
#' to induce the desired correlations.
#'
#' This function implements the method described at
#' \url{http://comisef.wikidot.com/tutorial:correlateduniformvariates}. The algorithm
#' first adjusts the correlation matrix using the transformation \code{2 * sin(pi * r / 6)}
#' to account for the uniform distribution, then generates correlated normal variables
#' and transforms them to uniform variables using the normal cumulative distribution function.
#'
#' @param n A positive integer specifying the number of random samples to generate.
#' @param M A square correlation matrix with values between -1 and 1. The matrix
#'   should be positive semi-definite. Column names, if present, will be preserved
#'   in the output matrix.
#' @param check_eigenvalues A logical value indicating whether to perform an
#'   eigenvalue check to verify that the correlation matrix is positive semi-definite.
#'   Defaults to \code{FALSE} due to numerical issues that can cause crashes.
#'   See Details for more information.
#'
#' @return A matrix of dimensions \code{n} x \code{ncol(M)} containing correlated
#'   uniform random variables with values between 0 and 1. Column names from the
#'   input correlation matrix are preserved.
#'
#' @details The correlation adjustment formula \code{2 * sin(pi * r / 6)} is applied
#'   to transform the target correlations for uniform variables into the appropriate
#'   correlations for normal variables before applying the Cholesky transformation.
#'
#'   The eigenvalue check (\code{check_eigenvalues = TRUE}) verifies that the
#'   correlation matrix is positive semi-definite by ensuring all eigenvalues are
#'   non-negative. However, this check is disabled by default due to numerical
#'   precision issues that can cause false failures and crashes. See
#'   \url{https://stat.ethz.ch/pipermail/r-help/2006-March/102703.html} for
#'   workarounds and \url{https://stat.ethz.ch/pipermail/r-help/2006-March/102647.html}
#'   for explanations of these issues.
#'
#' @examples
#' # Create a simple 2x2 correlation matrix
#' corr_matrix <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
#' colnames(corr_matrix) <- c("X1", "X2")
#'
#' # Generate 1000 correlated uniform variables (default: no eigenvalue check)
#' uniform_vars <- generate_corr_unifs(1000, corr_matrix)
#'
#' # Generate with eigenvalue validation (use with caution)
#' uniform_vars_checked <- generate_corr_unifs(1000, corr_matrix, 
#'                                             check_eigenvalues = TRUE)
#'
#' # Check the correlation structure
#' cor(uniform_vars)
#'
#' # Larger correlation matrix example
#' M <- matrix(c(1.0, 0.3, 0.1,
#'               0.3, 1.0, 0.2,
#'               0.1, 0.2, 1.0), nrow = 3)
#' colnames(M) <- c("Var1", "Var2", "Var3")
#' result <- generate_corr_unifs(500, M)
#'
#' @seealso \code{\link{rnorm}}, \code{\link{pnorm}}, \code{\link{chol}}, 
#'   \code{\link{eigen}}
#' 
#' @export
generate_corr_unifs <- function(n, M, check_eigenvalues = FALSE) {
  # generate normals, check correlations
  # from http://comisef.wikidot.com/tutorial:correlateduniformvariates
  stopifnot(is.matrix(M))
  stopifnot(is.logical(check_eigenvalues), length(check_eigenvalues) == 1L)
  
  # Check that matrix is semi-positive definite
  # NOTE this check can crash frequently!! see
  # https://stat.ethz.ch/pipermail/r-help/2006-March/102703.html for a
  # workaround and https://stat.ethz.ch/pipermail/r-help/2006-March/102647.html
  # for some explanation
  if (check_eigenvalues) {
    stopifnot(min(eigen(M, only.values = TRUE)$values) >= 0)
  }

  M_original <- M


  # adjust correlations for uniforms
  for (i in seq_len(dim(M)[[1L]])) {
    for (j in seq_len(dim(M)[[2L]])) {
      if (i != j) {
        M[i, j] <- 2 * sin(pi * M[i, j] / 6)
        M[j, i] <- 2 * sin(pi * M[j, i] / 6)
      }
    }
  }

  X <- matrix(dqrnorm(n * dim(M)[[2]]), n)
  colnames(X) <- colnames(M)

  # induce correlation, check correlations
  Y <- pnorm(X %*% chol(M))

  # message(paste0("Mean square error is: ", signif(sum((cor(Y) - M_original) ^
  # 2), 3)))
  return(Y)
}


# Ensures that when fwrite appends file colnames of file to be written, match
# those already in the file
#' Write Data Table to File with Column Safety
#'
#' A safer version of \code{\link[data.table]{fwrite}} that ensures column 
#' alignment when appending to existing files. This function automatically 
#' handles missing columns by adding them with NA values and reorders columns 
#' to match the existing file structure.
#'
#' @param x A data.table, data.frame, or matrix to write to file.
#' @param file Character string specifying the file path. If the file exists 
#'   and \code{append = TRUE}, the function will ensure column compatibility.
#' @param append Logical. If \code{TRUE}, the data will be appended to an 
#'   existing file. If \code{FALSE}, any existing file will be overwritten.
#'   Default is \code{TRUE}.
#' @param ... Additional arguments passed to \code{\link[data.table]{fwrite}}.
#'
#' @details
#' When \code{append = TRUE} and the target file exists, this function:
#' \itemize{
#'   \item Reads the column names from the existing file
#'   \item Throws an error if the new data contains columns not present in the original file
#'   \item Identifies missing columns in the new data and adds them with \code{NA} values
#'   \item Reorders columns to match the existing file structure
#' }
#' 
#' This prevents common errors that occur when appending data with different 
#' column structures to CSV files and ensures data integrity by not allowing
#' accidental column additions.
#'
#' @return Invisibly returns \code{TRUE} on success (same as \code{fwrite}).
#'
#' @seealso \code{\link[data.table]{fwrite}}, \code{\link[data.table]{fread}}
#'
#' @examples
#' \dontrun{
#' # Create initial data
#' dt1 <- data.table(a = 1:3, b = letters[1:3], c = 4:6)
#' fwrite_safe(dt1, "test.csv", append = FALSE)
#' 
#' # Append data with different column order
#' dt2 <- data.table(c = 7:8, b = letters[4:5], a = 9:10)
#' fwrite_safe(dt2, "test.csv", append = TRUE)  # Columns automatically aligned
#' 
#' # Append data with missing column
#' dt3 <- data.table(a = 11:12, b = letters[6:7])  # Missing column 'c'
#' fwrite_safe(dt3, "test.csv", append = TRUE)     # Column 'c' added with NA
#' 
#' # This would throw an error (extra column):
#' # dt4 <- data.table(a = 13:14, b = letters[8:9], d = 15:16)
#' # fwrite_safe(dt4, "test.csv", append = TRUE)  # Error: column 'd' not in original
#' }
#'
#' @export
fwrite_safe <- function(
  x,
  file,
  append = TRUE,
  ...
) {
  # Ensure x is a data.table for efficient column operations
  if (!data.table::is.data.table(x)) {
    x <- data.table::as.data.table(x)
  }
  
  if (append && file.exists(file)) {
    # Read column names from existing file without loading data
    col_names_disk <- names(data.table::fread(file, nrows = 0))
    col_names_file <- names(x)
    
    # Check for extra columns that don't exist in the original file
    extra_cols <- setdiff(col_names_file, col_names_disk)
    if (length(extra_cols) > 0) {
      stop("Cannot append data with extra columns. The following columns exist in the new data but not in the original file: ",
           paste(extra_cols, collapse = ", "), 
           ". Original file has columns: ", paste(col_names_disk, collapse = ", "),
           ". New data has columns: ", paste(col_names_file, collapse = ", "),
           call. = FALSE)
    }
    
    # Find columns that exist in file but not in new data
    missing_cols <- setdiff(col_names_disk, col_names_file)
    
    # Add missing columns with NA values
    if (length(missing_cols) > 0) {
      x[, (missing_cols) := NA]
    }
    
    # Reorder columns to match existing file structure
    data.table::setcolorder(x, col_names_disk)
  }
  
  # Write the data using fwrite
  data.table::fwrite(x, file, append = append, ...)
  
  return(TRUE)
}
