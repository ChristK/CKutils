% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{del_dt_rows}
\alias{del_dt_rows}
\title{delete output files

#' \code{delete_output_files} deletes the output files in the directory location specified in parameter
#' @param x The output directory containing the output files to be deleted. Should be a path relative to the directory
#' @export
#' @examples
#' delete_output_files(x = "C:/path/to/output/directory") # deletes all the files in the specified directory
delete_output_files <- function(x = output_dir()) {
file.remove(list.files(
path = x,
full.names = TRUE,
recursive = TRUE,
all.files = TRUE
))
}}
\usage{
del_dt_rows(DT, indx_to_del, DT_env)
}
\description{
delete output files

#' \code{delete_output_files} deletes the output files in the directory location specified in parameter
#' @param x The output directory containing the output files to be deleted. Should be a path relative to the directory
#' @export
#' @examples
#' delete_output_files(x = "C:/path/to/output/directory") # deletes all the files in the specified directory
delete_output_files <- function(x = output_dir()) {
file.remove(list.files(
path = x,
full.names = TRUE,
recursive = TRUE,
all.files = TRUE
))
}
}