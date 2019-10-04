## CKutils: an R package with some utility functions I use regularly
## Copyright (C) 2018  Chris Kypridemos

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

`:=` = function(...)
  NULL # due to NSE notes in R CMD check


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
#' @return Dropbax path as a String. If pathtail is present, it concatenates the Dropbox path with pathtail.
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
  function(pathtail = character(0),
           type = c("personal", "business")) {
    if (!requireNamespace("jsonlite", quietly = TRUE))
      stop("Please install package jsonlite first.")
    type <- match.arg(type)
    if (type[[1]] == "personal") {
      if (.Platform$OS.type == "windows") {
        if (file.exists(paste0(Sys.getenv("APPDATA"), "/Dropbox/info.json"))) {
          # for older versions of Dropbox
          dropbox_path <-
            jsonlite::read_json(paste0(Sys.getenv("APPDATA"), "/Dropbox/info.json"))$personal$path
        }
        if (file.exists(paste0(Sys.getenv("LOCALAPPDATA"), "/Dropbox/info.json"))) {
          dropbox_path <-
            jsonlite::read_json(paste0(Sys.getenv("LOCALAPPDATA"), "/Dropbox/info.json"))$personal$path
        }
      } else {
        if (file.exists("~/.dropbox/info.json"))
          dropbox_path <-
            jsonlite::read_json("~/.dropbox/info.json")$personal$path
      }
    }
    if (type[[1]] == "business") {
      if (.Platform$OS.type == "windows") {
        if (file.exists(paste0(Sys.getenv("APPDATA"), "/Dropbox/info.json"))) {
          # for older versions of Dropbox
          dropbox_path <-
            jsonlite::read_json(paste0(Sys.getenv("APPDATA"), "/Dropbox/info.json"))$business$path
        }
        if (file.exists(paste0(Sys.getenv("LOCALAPPDATA"), "/Dropbox/info.json"))) {
          dropbox_path <-
            jsonlite::read_json(paste0(Sys.getenv("LOCALAPPDATA"), "/Dropbox/info.json"))$business$path
        }
      } else {
        if (file.exists("~/.dropbox/info.json"))
          dropbox_path <-
            jsonlite::read_json("~/.dropbox/info.json")$business$path
      }
    }
    if (is.null(dropbox_path))
      stop("Dropbox path cannot be located.")
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
  } else
    pcloud_path <- "~/pCloudDrive/"

  pcloud_path <-
    normalizePath(paste0(pcloud_path, "/", pathtail), mustWork = FALSE)
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
  function(min_age = 0L,
           max_age = 85L,
           grp_width = 5L,
           grp_lessthan_1 = TRUE,
           match_input = FALSE,
           match_input_max_age = max_age) {
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
          out <- c(rep(out[2:((length(out) - 1L))], each = grp_width),
                   rep(out[length(out)], match_input_max_age - tail(x, 1) + 1L))
          out[1] <- "<1"
        }
        if (match_input && ((tail(x, 1) - 1L) == max_age)) {
          out <- rep(out[2:length(out)], each = grp_width)
          out[1] <- "<1"
        }
      } else {
        if (match_input && ((tail(x, 1) - 1L) != max_age)) {
          out <- c(rep(out[1:((length(out) - 1L))], each = grp_width),
                   rep(out[length(out)], match_input_max_age - tail(x, 1) + 1L))
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
#'  . The values in \code{from} arguement are matched
#'    and replaced by those in \code{to} arguement.
#'    If \code{newcolname = NULL} the replace is by reference.
#'
#' @param dt A data.table to be changed by reference.
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
#' dt <- data.table::data.table("a" = 1:5, "b" = seq(1, 2.2, 0.3),
#'  "d" = letters[1:5])
#' dt[, e := factor(a, labels = LETTERS[1:5])]
#' replace_from_table(data.table::copy(dt), "a", 1:3, 3L)[]
#' replace_from_table(data.table::copy(dt), "a", 3L, -11L)[]
#' replace_from_table(data.table::copy(dt), "a", 3L, -11L, "newcol")[]
#' replace_from_table(data.table::copy(dt), "b", 1.3, "a")[]
#' replace_from_table(data.table::copy(dt), "b", 1.3, "a", "newcol")[]
#' replace_from_table(data.table::copy(dt), "d", "a", "7")[]
#' replace_from_table(data.table::copy(dt), "d", "a", 7)[]
#' replace_from_table(data.table::copy(dt), "e", "B", "J")[]
replace_from_table <-
  function(dt,
           colname,
           from,
           to,
           newcolname = NULL) {
    old_ <- i.new_ <- NULL
    stopifnot(is.data.table(dt))
    stopifnot(length(colname) == 1L)
    stopifnot(is.null(newcolname) | length(newcolname) == 1L)
    stopifnot(colname %in% names(dt))
    stopifnot(length(from) >= length(to))
    # stopifnot(class(from) == dt[, class(get(colname))]) # not working for factors
    if (!is.null(newcolname) && newcolname %in% names(dt)) stop(
      "The new column name already exists in the data.table.")
    if (length(from) > length(to)) message("Note: matched many to few.")

    colorder <- copy(names(dt))
    if (class(from) == class(to)) {
      reg <- data.table("old_" = from, "new_" = to)
      dt[, "old_" := get(colname)]
      dt[reg, on = "old_", old_ := i.new_]
      if (is.null(newcolname)) {
        dt[, (colname) := NULL]
        setnames(dt, "old_", colname)
        setcolorder(dt, colorder)
      } else {
        setnames(dt, "old_", newcolname)
      }
    } else {
      reg <- data.table("old_" = as(from, class(to)),
                        "new_" = to)
      dt[, "old_" := as(get(colname), class(to))]
      dt[reg, on = "old_", old_ := i.new_]
      if (is.null(newcolname)) {
        message(paste0(
          colname,
          " coerced to ",
          class(to),
          " to match target class."
        ))
        dt[, (colname) := NULL]
        setnames(dt, "old_", colname)
        setcolorder(dt, colorder)
      } else {
        setnames(dt, "old_", newcolname)
      }
    }
    return(invisible(dt))
  }



#' Generate age-group from age
#'
#' `to_agegrp` creates a new column
#'
#' @param dt A data.table with a column named \code{age}.
#' @param grp_width The group width for the age groups.
#' @param max_age The max age for the closed agegroups. For ages above the max age,
#'  an open age group will be created, named max_age+ (i.e. 85+).
#' @param age_colname A string denoting the age column in \code{dt}.
#' @param agegrp_colname A string denoting the name of the column that will be
#'   created for age-groups.
#' @param to_factor A logical. If \code{TRUE}, then the age-groups
#'   column is converted to factor.
#' @param ... Pass arguements to \code{\link{agegrp_name}}.
#' @return a data.table, invisibly.
#' @export
#' @examples
#' library(data.table)
#' library(CKutils)
#' to_agegrp(data.table(age = 0:99))[]
#' to_agegrp(data.table(age = 0:99), max_age = 80L)[]
#' to_agegrp(data.table(age = 0:99), grp_width = 10, max_age = 85)[]
to_agegrp <-
  function(dt,
           grp_width = 5L,
           max_age = 85L,
           age_colname = "age",
           agegrp_colname = "agegrp",
           to_factor = TRUE,
           ...) {
    stopifnot(is.data.table(dt), age_colname %in% names(dt),
              length(age_colname) == 1L, length(agegrp_colname) == 1L,
              is.logical(to_factor))

    age_vec <- dt[, min(get(age_colname))]:dt[, max(get(age_colname))]
    agegroups <- agegrp_name(
      min_age = min(age_vec),
      max_age = max_age,
      grp_width = grp_width,
      match_input = FALSE,
      match_input_max_age = max(age_vec),
      ...
    )
    replace_from_table(
      dt,
      colname = age_colname,
      from = age_vec,
      to = agegrp_name(
        min_age = min(age_vec),
        max_age = max_age,
        grp_width = grp_width,
        match_input = TRUE,
        match_input_max_age = max(age_vec),
        ...
      ),
      newcolname = agegrp_colname
    )
    if (to_factor) {
      dt[, (agegrp_colname) := factor(get(agegrp_colname), agegroups)]
    }
    return(invisible(dt))
  }

#' Clone a data.table
#'
#' `clone_dt` clones a data.table and binds the copies at the bottom of
#' the original data.table. It also creates an column named \code{`.id`}
#' to identify each iteration. The keys of the input data.table are retained.
#' @param dt A data.table to be cloned
#' @param times The number of cloning iterations
#' @param idcol = TRUE Creates a new column containing the id of each data.table iteration
#' @return A data.table binding the original one and the new iterations with a column for the id of each iteration, invisibly
#' @export
#' @examples
#' library(data.table)
#' library(CKutils)
#' x <- c(1, 5, 3, 6, 4, 2, 9, 8, 7)
#' dt <- data.table(x)
#' clone_dt(dt, 3, idcol = TRUE)
clone_dt <-
  function(dt, times, idcol = TRUE) {
    xx <- key(dt)
    l <- rep(list(dt), times)
    out <- setkeyv(rbindlist(l, idcol = idcol), xx)
    return(invisible(out))
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
pctl_rank <- function(x, ties.method = c("average", "first", "random",
                                       "max", "min", "dense")) {
  stopifnot(is.numeric(x))
  ties.method <- match.arg(ties.method)
  n   <- length(x)
  out <- (frank(x,
         na.last = F,
         ties.method = ties.method) - 1) / (n - 1)
  return(out)
}


# TODO add documentation
#' Stochastic prediction from a gamlss object
#'
#' `validate_gamlss` returns a data.table with the observed and predicted
#'  variable. If  multiple predictions are drawn from the predicted
#'  distributions. Useful for plotting with ggplot
#'  @param dt A data.table from which come the observed variables
#'  @param gamlss_obj a gamlss object
#'  @param mc by default =10L
#'  @param orig_data initial data.table = \code{dt}
#'  @export
validate_gamlss <- function(dt, gamlss_obj, mc = 10L, orig_data = dt) {
  if (!requireNamespace("gamlss", quietly = TRUE))
    stop("Please install package gamlss first.")
  stopifnot("gamlss" %in% class(gamlss_obj), is.data.table(dt), mc >= 1,
            is.data.table(orig_data))
  nam_y <- as.character(gamlss_obj$call$formula[[2]])
  nam_var <- all.vars(gamlss_obj$call$formula[[3]])
  nam_dist <- paste0("r", gamlss_obj$family[[1]])
  nam_param <- gamlss_obj$parameters
  x <- copy(dt)
  x[, type := "Observed"]
  z <- copy(dt)
  z[, (nam_param) := gamlss::predictAll(gamlss_obj, type = "response",
                                      newdata = dt[, .SD, .SDcols = nam_var],
                                      data = orig_data[, .SD,
                                                       .SDcols = c(nam_var)])]
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
#'  variable. `dt` needs to have a column with percentiles named `rank_y`,
#'  where `y` the name of the predicted variable (i.e. bmi).
#' @param dt A data.table
#' @param gamlss_obj gamlss object
#' @param orig_data original data.table
#' @param nc by default = 1L
#' @export
guess_gamlss <- function(dt, gamlss_obj, orig_data = gamlss_obj$data, nc = 1L) {
  if (!requireNamespace("gamlss", quietly = TRUE))
    stop("Please install package gamlss first.")
  stopifnot("gamlss" %in% class(gamlss_obj),
            is.data.table(dt), nc >= 1L,
            is.data.table(orig_data))
  nam_y <- as.character(gamlss_obj$call$formula[[2]])
  nam_var <- all.vars(gamlss_obj$call$formula[[3]])
  nam_dist <- paste0("q", gamlss_obj$family[[1]])
  nam_param <- gamlss_obj$parameters

  orig_data <- orig_data[, ..nam_var]
  dtu <- unique(dt[, ..nam_var]) # otherwise too slow
  dtu <- split(dtu, dtu$year)
  if("RevoUtilsMath" %in% (.packages())) tt <- getMKLthreads()
  if("RevoUtilsMath" %in% (.packages())) setMKLthreads(1L)
  dtu <- parallel::mclapply(dtu, function(x) {
    x[, (nam_param) := gamlss::predictAll(gamlss_obj,
                                          type = "response",
                                          newdata = .SD,
                                          data = orig_data)]
  },
  mc.preschedule = FALSE,
  mc.cores = nc)
  if("RevoUtilsMath" %in% (.packages())) setMKLthreads(tt)
  dtu <- rbindlist(dtu)
  # dtu[, (nam_param) := gamlss::predictAll(gamlss_obj,
  #                                        type = "response",
  #                                        newdata = .SD,
  #                                        data = orig_data)]
  dt[dtu, on = nam_var, (nam_param) := mget(paste0("i.", nam_param))]
  dt[, p := get(paste0("rank_", nam_y))]
  stopifnot(dt[, all(between(p, 0, 1, incbounds = FALSE))])
  dt[, (nam_y) := do.call(nam_dist, .SD), .SDcols = c("p", nam_param)]
  dt[, c("p", nam_param) := NULL]
}


# TODO add documentation
#' Prediction from a MASS:polr object in parallel
#'
#' `guess_polr` returns a data.table with the predicted
#'  variable. `dt` needs to have a column with percentiles named `rank_y`,
#'  where `y` the name of the predicted variable (i.e. active_days).
#' @param dt a data.table
#' @param polr_obj a polr object
#' @export
guess_polr <- function(dt, polr_obj) {
  if (!requireNamespace("MASS", quietly = TRUE))
    stop("Please install package MASS first.")
  if (!requireNamespace("matrixStats", quietly = TRUE))
    stop("Please install package matrixStats first.")
  stopifnot("polr" %in% class(polr_obj), is.data.table(dt))
  nam_y <- as.character(polr_obj$call$formula[[2]])
  nam_var <- all.vars(polr_obj$call$formula[[3]])
  #code adapted from method getAnywhere(predict.polr)
  Terms <- delete.response(polr_obj$terms)
  m <- model.frame(Terms, dt[, ..nam_var], na.action = function(x) x,
                   xlev = polr_obj$xlevels)
  if (!is.null(cl <- attr(Terms, "dataClasses")))
    .checkMFClasses(cl, m)
  X <- model.matrix(Terms, m, contrasts = polr_obj$contrasts)
  xint <- match("(Intercept)", colnames(X), nomatch = 0L)
  if (xint > 0L)
    X <- X[, -xint, drop = FALSE]
  n <- nrow(X)
  q <- length(polr_obj$zeta)
  eta <- drop(X %*% polr_obj$coefficients)
  cc <- plogis(matrix(polr_obj$zeta, n, q, byrow = TRUE) -
                 eta)
  dt[, p := get(paste0("rank_", nam_y))]
  dt[, (nam_y) := matrixStats::rowSums2(cc < p)]
  dt[, "p" := NULL]
}


# TODO add documentation
#' Deterministic prediction from a gamlss object
#'
#' `crossval_gamlss` returns the observed and predicted values of the dependent
#'  variable. Useful for cross-validation metrics.
#' @param dt A data. table
#' @param gamlss_obj a gamlss object
#' @param orig_data original data.table
#' @param colnam column names
#' @export
crossval_gamlss <- function(dt, gamlss_obj, orig_data = dt, colnam = "rank") {
  stopifnot("gamlss" %in% class(gamlss_obj), is.data.table(dt),
            is.data.table(orig_data))
  out <- list()
  nam_y <- as.character(gamlss_obj$call$formula[[2]])
  nam_var <- all.vars(gamlss_obj$call$formula[[3]])
  nam_dist <- paste0("q", gamlss_obj$family[[1]])
  nam_param <- gamlss_obj$parameters
  out$observed <- dt[, get(nam_y)]
  z <- copy(dt)
  z[, (nam_param) := predictAll(gamlss_obj, type = "response",
                                newdata = dt[, .SD, .SDcols = nam_var],
                                data = orig_data[, .SD,
                                                 .SDcols = c(nam_y, nam_var)])]
  setnames(z, colnam, "p")
  z[p == 0, p := 0.0001]
  z[p == 1, p := 0.9999]
  z[, (nam_y) := do.call(nam_dist, .SD), .SDcols = c("p", nam_param)]
  out$predicted <- z[, get(nam_y)]
  return(out)
}

##' Generate Counts of Values in a Vector
##'
##' This function uses Rcpp sugar to implement a fast \code{table}, for
##' unique counts of a single vector. This implementation seeks to
##' produce identical output to \code{table(x, useNA="ifany")}. It is borrowed
##' from \code{Kmisc} package for convenience, since \code{Kmisc} is not in CRAN
##'  anymore. \code{Kmisc} is available at https://github.com/kevinushey/Kmisc

##'
##' The order of \code{NA}, \code{NaN} in the output may differ -- even
##' \R is inconsistent with the order that \code{NA} and \code{NaN} elements
##' are inserted.
##'
##' @param x A numeric, integer, character or logical vector, or a (potentially
##'   nested) list of such vectors. If \code{x} is a list, we recursively apply
##'   \code{counts} throughout elements in the list.
##' @export
##' @examples
##' x <- round( rnorm(1E2), 1 )
##' counts(x)
counts <- function(x) {
  if (is.list(x)) {
    output <- rapply(x, counts, how="list")
    return(output)
  } else {
    return(.Call('_CKutils_counts', x))
  }
}


#' Obtain matching names corresponding to patterns
#'
#' `match_colnames_pattern` returns the matching names of the argument `dt`
#' (i.e. \code{names(dt)}) corresponding to the regular expression patterns
#' provided. The patterns must be supported by \code{\link{grep}}.
#' This is based on `data.table:::patterns`
#' @param dt A data.table from which the column names \code{names(dt)} or \code{colnames(dt)} will be tested
#' @param ... A list of the names to match with the data.table ones. Needs to be made of character otherwise the function stops
#' @export
#' @examples
#' library(data.table)
#' library(CKutils)
#' dt <- data.table(id = c("city", "year", "birth", "idp"), b = c("age", "year", "bp", "name"))
#' z <- list("id", "year", "b")
#' match_colnames_pattern(dt, z) #[1] "id" "b"
match_colnames_pattern <- function(dt, ...) {
  p = unlist(list(...), use.names = FALSE)
  if (!is.character(p)) stop("Input patterns must be of type character.")
  cols = names(dt)
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
reldist_diagnostics <- function(comparison, reference, comparison_wt, reference_wt,
                                main, smooth = 0.35, discrete = FALSE) {
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  if (!requireNamespace("reldist", quietly = TRUE)) {
    stop("Package \"reldist\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  reference_dens  <- density(reference, weights = reference_wt)
  comparison_dens <- density(comparison, weights = comparison_wt)

  par(mfrow = c(2,2))
  plot(
    reference_dens,
    main = main,
    lty = 3,
    ylim = c(0, 1.1 * max(c(
      reference_dens$y, comparison_dens$y
    )))
  )
  lines(comparison_dens, col = "red", lty = 2)
  legend("topright", bg="transparent" ,
         legend=c("Comparison", "Reference"), box.lty = 0,
         col = c("red", "black"), lty = 2:3, cex = 0.8)
  g10 <- reldist::reldist(y=comparison, yo=reference,
                 smooth=smooth, ci=TRUE,
                 ywgt=comparison_wt, yowgt=reference_wt,
                 #ylim=c(0,1.5),
                 #yolabs=seq(-1,3,by=0.5),
                 bar="yes", quiet=FALSE, discrete = discrete,
                 xlab="proportion of the reference cohort")
  title(main="Overall relative density",cex=0.6)
  abline(h=1,lty=2)
  g1A <- reldist::reldist(y=comparison, yo=reference,
                 ywgt=comparison_wt, yowgt=reference_wt,
                 show="effect",
                 bar="yes", quiet=FALSE,
                 smooth=smooth, ci=TRUE, discrete = discrete,
                 #ylim=c(0,1.5),
                 #yolabs=seq(-1,3,by=0.5),
                 xlab="proportion of the reference cohort")
  title(main= "Effect of the median shift",cex=0.6)
  abline(h=1,lty=2)
  gA0 <- reldist::reldist(y=comparison, yo=reference,
                 smooth=smooth, ci=TRUE,
                 ywgt=comparison_wt, yowgt=reference_wt,
                 show="residual",
                 bar="yes", quiet=FALSE, discrete = discrete,
                 #ylim=c(0,1.5),
                 #yolabs=seq(-1,3,by=0.5),
                 xlab="proportion of the reference cohort")
  title(main="Median-adjusted relative density" ,cex=0.6)
  abline(h=1,lty=2)

  a1 <- reldist::rpy(y=comparison,yo=reference,
      ywgt=comparison_wt,yowgt=reference_wt,pvalue=TRUE)
  a2 <- reldist::rpluy(y=comparison,yo=reference,
              ywgt=comparison_wt,yowgt=reference_wt,pvalue=TRUE)
  a3 <- reldist::rpluy(y=comparison,yo=reference,
              ywgt=comparison_wt,yowgt=reference_wt,pvalue=TRUE,
              upper=TRUE)
  # p1 <- ifelse(a1[[4]]<0.001, "<0.001", format(a1[[4]], digits = 3))
  # p2 <- ifelse(a2[[4]]<0.001, "<0.001", format(a2[[4]], digits = 3))
  # p3 <- ifelse(a3[[4]]<0.001, "<0.001", format(a3[[4]], digits = 3))

  out <- data.table("Summary statistics" = c("Overall change entropy",
                                            "Median effect entropy",
                                            "Shape effect entropy",
                                            "Median polarization index",
                                            "Lower polarization index",
                                            "Upper polarization index"
                                            ),
                    "Measure" = c(g10$entropy,
                                  reldist::entropy(g1A,g10),
                                  gA0$entropy,
                                  a1[[2]],
                                  a2[[2]],
                                  a3[[2]]
                    ),
                    "Lower 95% CI" = c(NA, NA, NA,
                                       a1[[1]],
                                       a2[[1]],
                                       a3[[1]]
                    ),
                    "Upper 95% CI" = c(NA, NA, NA,
                                       a1[[3]],
                                       a2[[3]],
                                       a3[[3]]
                    ),
                    "p-value" = c(NA, NA, NA,
                                       a1[[4]],
                                       a2[[4]],
                                       a3[[4]]
                    )
                    )
  return(out[])
}


#' Simplified loading and installing of packages
#'
#' This is a wrapper to \code{\link{require}} and \code{\link{install.packages}}.
#' Specifically, this will first try to load the package(s) and if not found
#' it will install then load and attach the packages. Additionally, if the
#' \code{update=TRUE} parameter is specified it will check the currently
#' installed package version with what is available on CRAN (or mirror) and
#' install the newer version.
#'
#' The function was originally created by Jason Bryer
#' \href{https://www.r-bloggers.com/function-to-simplify-loading-and-installing-packages/}{here}
#' and the source is available \href{https://gist.github.com/jbryer/9112634}{here}.
#' Note: I renamed the function to \code{dependencies} and adapted it to attach
#' instead of only load the packages.
#'
#' @param pkges a character vector with the names of the packages to load.
#' @param install if TRUE (default), any packages not already installed will be.
#' @param update if TRUE, this function will install a newer version of the
#'        package if available.
#' @param quiet if TRUE (default), package startup messages will be suppressed.
#' @param verbose if TRUE (default), diagnostic messages will be printed.
#' @param ... other parameters passed to \code{\link{require}},
#'            \code{\link{install.packages}}, and
#'            \code{\link{available.packages}}.
#' @return a data frame with four columns and rownames corresponding to the
#'         packages to be loaded. The four columns are: loaded (logical
#'         indicating whether the package was successfully loaded), installed
#'         (logical indicating that the package was installed or updated),
#'         loaded.version (the version string of the installed package), and
#'         available.version (the version string of the package currently
#'         available on CRAN). Note that this only reflects packages listed in
#'         the \code{pkges} parameter. Other packages may be loaded and/or
#'         installed as necessary by \code{install.packages} and \code{require}.
#'         If \code{verbose=FALSE} the data frame will be returned using
#'         \code{\link{invisible}}.
#' @export
#' @examples
#' \dontrun{
#' dependencies(c('devtools','lattice','ggplot2','psych'))
#' }
dependencies <-
  function(pkges,
           install = TRUE,
           update  = FALSE,
           quiet   = TRUE,
           verbose = FALSE,
           ...) {
    myrequire <- function(package, ...) {
      result <- FALSE
      if (quiet) {
        suppressMessages(suppressWarnings(result <- requireNamespace(package, ...)))
      } else {
        result <- suppressWarnings(requireNamespace(package, ...))
      }
      return(result)
    }
    mymessage <- function(msg) {
      if (verbose) {
        message(msg)
      }
    }

    installedpkgs <- installed.packages()
    availpkgs <- available.packages()[, c('Package', 'Version')]
    if (nrow(availpkgs) == 0) {
      warning(
        paste0(
          'There appear to be no packages available from the ',
          'repositories. Perhaps you are not connected to the ',
          'Internet?'
        )
      )
    }
    # It appears that hyphens (-) will be replaced with dots (.) in version
    # numbers by the packageVersion function
    availpkgs[, 'Version'] <- gsub('-', '.', availpkgs[, 'Version'])
    results <- data.frame(
      loaded = rep(FALSE, length(pkges)),
      installed = rep(FALSE, length(pkges)),
      loaded.version = rep(as.character(NA), length(pkges)),
      available.version = rep(as.character(NA), length(pkges)),
      stringsAsFactors = FALSE
    )
    row.names(results) <- pkges
    for (i in pkges) {
      loadedPkgs <- search()
      needInstall <- FALSE
      if (i %in% row.names(installedpkgs)) {
        v <- as.character(packageVersion(i))
        if (i %in% row.names(availpkgs)) {
          if (v != availpkgs[i, 'Version']) {
            if (!update) {
              mymessage(
                paste0(
                  'A different version of ',
                  i,
                  ' is available ',
                  '(current=',
                  v,
                  '; available=',
                  availpkgs[i, 'Version'],
                  ')'
                )
              )
            }
            needInstall <- update
          }
          results[i, ]$available.version <- availpkgs[i, 'Version']
        } else {
          mymessage(paste0(i, ' is not available on the repositories.'))
        }
      } else {
        if (i %in% row.names(availpkgs)) {
          needInstall <- TRUE & install
          results[i, ]$available.version <- availpkgs[i, 'Version']
        } else {
          warning(paste0(
            i,
            ' is not available on the repositories and ',
            'is not installed locally'
          ))
        }
      }
      if (needInstall | !myrequire(i)) {
        install.packages(pkgs = i, quiet = quiet)
        if (!myrequire(i, ...)) {
          warning(paste0('Error loading package: ', i))
        } else {
          results[i, ]$installed <- TRUE
          results[i, ]$loaded <- TRUE
          results[i, ]$loaded.version <- as.character(packageVersion(i))
        }
      } else {
        results[i, ]$loaded <- TRUE
        results[i, ]$loaded.version <- as.character(packageVersion(i))
      }
      loadedPkgs2 <- search()
      for (j in loadedPkgs2[!loadedPkgs2 %in% loadedPkgs]) {
        try(detach(j, character.only = TRUE), silent = TRUE)
      }
      library(i, character.only = TRUE)
    }
    # library(pkges, character.only	= TRUE)
    if (verbose) {
      return(results)
    } else {
      invisible(results)
    }
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

#' Copy all columns of dt_i in dt_x
#' @export
absorb_dt <- function(dt_x, dt_i, on = ".NATURAL", exclude_col = NULL) {
  stopifnot(is.data.table(dt_x), is.data.table(dt_i), is.character(on))
  nam_i <- names(dt_i)
  nam_x <- names(dt_x)
  if (length(on) == 1 && on == ".NATURAL")
    on <- setdiff(intersect(nam_x, nam_i), exclude_col)
  colnam_replaced <-
    intersect(setdiff(nam_x, on), setdiff(nam_i, on))
  if (length(colnam_replaced) > 0)
    message(
      paste(
        "\ncolumn",
        colnam_replaced,
        "in dt_x has been replaced by the identically named column in dt_i",
        collapse = ","
      )
    )
  colnam <- setdiff(nam_i, on)
  ncol <- length(colnam)

  colnam_tmp <- paste0("COL", seq_len(ncol), "____")
  setnames(dt_i, colnam, colnam_tmp)
  # dt_x[dt_i, (colnam) := .(i.COL1____, i.COL2____), on = on]
  colnam_tmp2 <- paste0("i.", colnam_tmp)
  argum <- paste0("dt_x[dt_i, (colnam) := .(", paste(colnam_tmp2, collapse = ", "), "), on = on]")
  eval(parse(text = argum))
  setnames(dt_i, colnam_tmp, colnam)
  invisible(dt_x)
}

# dt_x <- data.table(a = 1:5, b = 1:5, c = 5:9, d = 5:1, e = 1:5)
# dt_i <- data.table(a = 1:5, b = 1:5, c = 1:5, d = 1:5)
# dt_x[dt_i, on = c("a", "b")]
# absorb_dt(dt_x, dt_i, c("a", "b"))[]
#
# dt_x <- data.table(a = 1:5, b = 1:5, c = 5:9, d = 5:1, e = 1:5)
# dt_i <- data.table(a = 3:5, b = 3:5, c = 3:5, d = 3:5)
# dt_x[dt_i, on = c("a", "b")]
# absorb_dt(dt_x, dt_i, c("a", "b"))[]
#
# dt_x <- data.table(a = 1:5, b = 1:5, c = 5:9, d = 5:1, e = 1:5)
# dt_i <- data.table(a = 3:5, b = 3:5, c = 1:5, d = 1:5)
# dt_x[dt_i, on = c("a", "b")]
# absorb_dt(dt_x, dt_i, c("a", "b"))[]
#
# dt_x <- data.table(a = 1:5, b = 1:5, c = 5:9, d = 5:1, e = 1:5)
# dt_i <- data.table(a = 1:10, b = 1:10, c = 1:10, d = 1:10)
# dt_x[dt_i, on = c("a", "b")]
# absorb_dt(dt_x, dt_i, c("a", "b"))[]

.onUnload <- function(libpath) {
  library.dynam.unload("CKutils", libpath)
}

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
  if (var >= (mu * (1 - mu))) var  <- mu * (1 - mu) * 0.9999
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

#' # delete output files
#' #' `delete_output_files` deletes the output files in the directory location specified in parameter
#' #' @param x The output directory containing the output files to be deleted. Should be a path relative to the directory
#' #' @export
#' #' @examples
#' #' delete_output_files(x = "C:/path/to/output/directory") # deletes all the files in the specified directory
#' delete_output_files <- function(x = output_dir()) {
#'   file.remove(list.files(
#'     path = x,
#'     full.names = TRUE,
#'     recursive = TRUE,
#'     all.files = TRUE
#'   ))
#' }


#' @export
del_dt_rows <- function(dt, indx_to_del, dt_env = .GlobalEnv) {
  stopifnot(is.data.table(dt), (is.integer(indx_to_del) | is.logical(indx_to_del)))
  if (is.integer(indx_to_del)) keep <- -indx_to_del
  if (is.logical(indx_to_del)) keep <- !indx_to_del

  name_of_dt <- deparse(substitute(dt))
  # dt_env <- pryr::where(name_of_dt) # to get dt envirnment
  dt_names <- copy(names(dt))
  dt_new <- dt[keep, dt_names[1L], with = F]
  set(dt, i = NULL, j = 1L, value = NULL)

  for (j in seq_len(ncol(dt))) {
    set(dt_new,
        i = NULL,
        j = dt_names[1L + j],
        value = dt[[1L]][keep])
    set(dt,
        i = NULL,
        j = 1L,
        value = NULL)
  }
  assign(name_of_dt, value = dt_new, envir = dt_env)
}

#' @export
identical_elements <-
  function(x, tol = .Machine$double.eps ^ 0.5) {
    stopifnot(is.numeric(x))
    fequal(x, tol)
  }


# normalise a vector to 0,1 range
#' @export
normalise <-
  function(x, ...) {
    stopifnot(is.numeric(x))
    if (identical_elements(x))
      return(1)
    else
      return(fnormalise(x))
  }
