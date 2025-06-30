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

#' Clone a data.table
#'
#' `clone_dt` clones a data.table and binds the copies at the bottom of
#' the original data.table. It also creates an column named \code{`.id`}
#' to identify each iteration. The keys of the input data.table are retained.
#' @param dtb A data.table to be cloned
#' @param times The number of cloning iterations
#' @param idcol = TRUE Creates a new column containing the id of each data.table iteration
#' @return A data.table binding the original one and the new iterations with a column for the id of each iteration, invisibly
#' @export
#' @examples
#' library(data.table)
#' library(CKutils)
#' x <- c(1, 5, 3, 6, 4, 2, 9, 8, 7)
#' dtb <- data.table(x)
#' clone_dt(dtb, 3, idcol = TRUE)
clone_dt <-
    function(dtb, times, idcol = TRUE) {
        # TODO early escape for times == 1
        xx <- key(dtb)
        l <- rep(list(dtb), times)
        out <- setkeyv(rbindlist(l, idcol = idcol), xx)
        return(invisible(out))
    }


#' Absorb Columns from One data.table into Another
#'
#' This function copies columns from a secondary data.table (`dt_i`) into a primary data.table (`dt_x`) based on matching key columns. If the join key is set to ".NATURAL" (the default), the function automatically determines the keys as the intersection of column names from `dt_x` and `dt_i`, excluding any specified in `exclude_col`. Any non-key columns in `dt_x` that share names with columns in `dt_i` are replaced by the corresponding columns from `dt_i`. This is achieved by temporarily renaming the columns in `dt_i`, performing the join, and then restoring the original column names.
#'
#' @param dt_x A data.table that will have its columns updated.
#' @param dt_i A data.table from which columns are to be copied.
#' @param on A character vector specifying the join keys. If set to ".NATURAL", the keys are automatically determined as the common columns between `dt_x` and `dt_i`, excluding any specified in `exclude_col`.
#' @param exclude_col An optional character vector of column names to exclude from the automatically determined join keys.
#' @param verbose Logical. If TRUE, outputs messages indicating which columns in `dt_x` have been replaced by columns from `dt_i`.
#'
#' @return Returns `dt_x` invisibly after modifying it in place by copying columns from `dt_i`.
#'
#' @export
#'
#' @examples
#' library(data.table)
#' dt_x <- data.table(a = 1:5, b = 1:5, c = 5:9, d = 5:1, e = 1:5)
#' dt_i <- data.table(a = 1:5, b = 1:5, c = 1:5, d = 1:5)
#' absorb_dt(dt_x, dt_i, on = c("a", "b"))
absorb_dt <- function(
    dt_x,
    dt_i,
    on = ".NATURAL",
    exclude_col = NULL,
    verbose = FALSE
) {
    stopifnot(is.data.table(dt_x), is.data.table(dt_i), is.character(on))
    nam_i <- names(dt_i)
    nam_x <- names(dt_x)
    if (length(on) == 1 && on == ".NATURAL") {
        on <- setdiff(intersect(nam_x, nam_i), exclude_col)
    }
    colnam_replaced <-
        intersect(setdiff(nam_x, on), setdiff(nam_i, on))
    if (verbose && length(colnam_replaced) > 0) {
        message(
            paste(
                "\ncolumn",
                colnam_replaced,
                "in dt_x has been replaced by the identically named column in dt_i",
                collapse = ","
            )
        )
    }
    colnam <- setdiff(nam_i, on)
    ncol <- length(colnam)

    colnam_tmp <- paste0("COL", seq_len(ncol), "____")
    setnames(dt_i, colnam, colnam_tmp)
    # dt_x[dt_i, (colnam) := .(i.COL1____, i.COL2____), on = on]
    colnam_tmp2 <- paste0("i.", colnam_tmp)
    argum <- paste0(
        "dt_x[dt_i, (colnam) := .(",
        paste(colnam_tmp2, collapse = ", "),
        "), on = on]"
    )
    eval(str2lang(argum))
    setnames(dt_i, colnam_tmp, colnam)
    invisible(dt_x)
}


#' Delete specified rows from a data.table
#'
#' This function removes rows from a data.table based on specified indices. It creates a new data.table containing only the rows that are retained, preserves the original key, and then reassigns the updated data.table back to its original variable in the specified environment.
#'
#' @param dtb A data.table from which rows will be deleted.
#' @param indx_to_del An integer vector indicating row positions to delete, or a logical vector where TRUE indicates rows to delete.
#' @param dt_env The environment where the data.table is stored; default is .GlobalEnv.
#'
#' @return Invisibly returns the updated data.table with the specified rows removed.
#'
#' @examples
#' library(data.table)
#' dtb <- data.table(a = 1:5, b = letters[1:5])
#' # Delete the 2nd and 4th rows
#' del_dt_rows(dtb, c(2L, 4L))
#' # Alternatively, using a logical vector to delete rows where 'a' is even
#' del_dt_rows(dtb, dtb$a %% 2 == 0)
#' @export
del_dt_rows <- function(dtb, indx_to_del, dt_env = .GlobalEnv) {
    stopifnot(
        is.data.table(dtb),
        (is.integer(indx_to_del) | is.logical(indx_to_del))
    )

    dt_keys <- key(dtb)
    if (is.integer(indx_to_del)) {
        keep <- -indx_to_del
    }
    if (is.logical(indx_to_del)) {
        keep <- !indx_to_del
    }

    name_of_dt <- deparse(substitute(dtb))
    # dt_env <- pryr::where(name_of_dt) # to get dt envirnment
    dt_names <- copy(names(dtb))
    dt_new <- dtb[keep, dt_names[1L], with = FALSE]
    set(dtb, i = NULL, j = 1L, value = NULL)

    for (j in seq_len(ncol(dtb))) {
        set(dt_new, i = NULL, j = dt_names[1L + j], value = dtb[[1L]][keep])
        set(dtb, i = NULL, j = 1L, value = NULL)
    }

    setkeyv(dt_new, dt_keys)
    assign(name_of_dt, value = dt_new, envir = dt_env)
}


#' Perform operations on selected columns in a data.table
#'
#' This function performs arithmetic or other operations on a subset of columns in a data.table.
#' It wraps the specified column names in backticks, concatenates them using the provided operator,
#' and optionally applies a function to the resulting expression.
#'
#' @param dtb A data.table.
#' @param cols_to_add A character vector of column names to be operated on.
#' @param symbol A character string specifying the operator to use between the columns. Defaults to ',' (if a vector is provided, the first element is used). Other examples include '+', '-', '*', or '/'.
#' @param fn An optional function name as a string. If provided, the operator expression is wrapped inside this function call.
#'
#' @return A data.table with one column containing the result of the evaluated expression.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' dtb <- data.table(a = 1:5, b = 6:10)
#' # Sum columns a and b
#' do_cols_dt(dtb, c("a", "b"), symbol = "+")
#'
#' # Multiply columns a and b
#' do_cols_dt(dtb, c("a", "b"), symbol = "*")
#'
#' # Apply the sum function to the columns
#' do_cols_dt(dtb, c("a", "b"), symbol = "+", fn = "sum")
#' }
#' @export
do_cols_dt <- function(
    dtb,
    cols_to_add,
    symbol = c(",", "+", "-", "*", "/"),
    fn = NULL
) {
    cols_to_add <- paste0("`", cols_to_add, "`")
    argum <- paste(cols_to_add, collapse = symbol)
    if (!is.null(fn)) {
        argum <- paste0(fn, "(", argum, ")")
    }
    dtb[, .(eval(str2lang(argum)))]
}
