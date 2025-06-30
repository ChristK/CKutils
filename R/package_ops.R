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

#' @title Detach a Loaded Package
#'
#' @description
#' Detaches a loaded R package cleanly from the current session. This is useful
#' when reinstalling or updating a package during development.
#'
#' @param pkg Character string of package name. The name of the package to detach.
#'
#' @return Invisibly returns \code{TRUE} if the package was detached, or \code{FALSE} if it was not attached.
#'
#' @details
#' If the specified package is attached multiple times (e.g., via multiple `library()` calls),
#' it will be fully detached. A message is displayed indicating whether the detachment occurred.
#'
#' @examples
#' \dontrun{
#' detach_package("ggplot2")
#' }
#'
#' @seealso \code{\link{library}}, \code{\link{detach}}
#'
#' @export
detach_package <- function(pkg) {
  if (!is.character(pkg)) {
    stop("pkg must be a character string")
  }

  search_item <- paste0("package:", pkg)
  detached <- FALSE

  while (search_item %in% search()) {
    try(
      {
        detach(search_item, unload = TRUE, character.only = TRUE)
        library.dynam.unload(pkg, system.file(package = pkg))
        unloadNamespace(pkg)
      },
      silent = TRUE
    )
    message(sprintf("Detached package: %s", pkg))
    detached <- TRUE
  }

  if (!detached) {
    message(sprintf("Package '%s' was not attached.", pkg))
  }

  invisible(detached)
}

#' @title Install or Reinstall a Local R Package
#'
#' @description
#' Installs or reinstalls a local R package from the specified directory. Useful during
#' development to ensure the package is rebuilt with updated source code and documentation.
#'
#' @param sPackageDirPath Character string. Path to the local package directory.
#'
#' @return Invisibly returns \code{NULL}. Prints installation progress and success/failure messages.
#'
#' @details
#' The function performs the following steps:
#' - Ensures 'remotes' and 'roxygen2' are installed.
#' - Generates updated documentation using \code{roxygen2::roxygenise()}.
#' - Detaches the package if already loaded.
#' - Deletes compiled files such as .o, .so, or .dll for a clean rebuild.
#' - Installs the package using \code{remotes::install_local()}.
#'
#' @examples
#' \dontrun{
#' installLocalPackage()
#' installLocalPackage("/path/to/custom/package_dir")
#' }
#'
#' @seealso \code{\link[remotes]{install_local}}, \code{\link[roxygen2]{roxygenise}}
#'
#' @export
installLocalPackage <- function(sPackageDirPath) {
  # Ensure necessary tools are installed
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }

  if (!requireNamespace("roxygen2", quietly = TRUE)) {
    install.packages("roxygen2")
  }
  roxygen2::roxygenise(sPackageDirPath, clean = TRUE)

  # Detach package if already loaded
  # Read package name from DESCRIPTION file
  desc_path <- file.path(sPackageDirPath, "DESCRIPTION")
  pkg_name <- if (file.exists(desc_path)) {
    read.dcf(desc_path, fields = "Package")[1]
  } else {
    stop("DESCRIPTION file not found in: ", sPackageDirPath)
  }
  detach_package(pkg_name)

  # Remove any compiled files to ensure clean install
  compiled_files <- list.files(
    path = sPackageDirPath,
    pattern = "\\.o$|\\.dll$|\\.so$",
    recursive = TRUE,
    full.names = TRUE
  )
  file.remove(compiled_files)

  # Install the package from local directory
  tryCatch(
    {
      remotes::install_local(
        sPackageDirPath,
        build_vignettes = TRUE,
        force = TRUE,
        upgrade = "never"
      )
      message(paste0(pkg_name, " package installed successfully."))
    },
    error = function(e) {
      message(paste0("Failed to install ", pkg_name, " package: "), e$message)
    }
  )
}

#' @title Install Local Package If Changed
#'
#' @description
#' Reinstalls a local R package if it is not already installed or if files in the
#' package directory have changed since the last snapshot. Useful for automating
#' package installation during development.
#'
#' @param pkg_path Character string. Path to the local R package directory.
#' @param snapshot_path Character string. Path to the snapshot file used for change detection.
#'
#' @return Invisibly returns \code{NULL}. Installs the package and updates the snapshot if needed.
#'
#' @details
#' The function checks whether the specified package is installed and whether any
#' files in the directory have changed. If needed, the package is reinstalled and the
#' snapshot is updated.
#'
#' @examples
#' \dontrun{
#' installLocalPackageIfChanged(
#'   pkg_path = "./Rpackage/IMPACTncd_Engl_model_pkg/",
#'   snapshot_path = "./Rpackage/.IMPACTncd_Engl_model_pkg_snapshot.rds"
#' )
#' }
#'
#' @seealso \code{\link{installLocalPackage}}, \code{\link{saveRDS}}
#'
#' @export
installLocalPackageIfChanged <- function(pkg_path, snapshot_path) {
  snapshot <- if (file.exists(snapshot_path)) {
    changedFiles(readRDS(snapshot_path))
  } else {
    NULL
  }

  desc_path <- file.path(pkg_path, "DESCRIPTION")
  pkg_name <- if (file.exists(desc_path)) {
    read.dcf(desc_path, fields = "Package")[1]
  } else {
    stop("DESCRIPTION file not found in: ", pkg_path)
  }

  needs_install <- !requireNamespace(pkg_name, quietly = TRUE) ||
    is.null(snapshot) ||
    any(nzchar(unlist(snapshot[c("added", "deleted", "changed")])))

  if (needs_install) {
    installLocalPackage(pkg_path)

    if (!is.null(snapshot)) {
      file.remove(snapshot_path)
    }

    snapshot <- fileSnapshot(
      pkg_path,
      timestamp = NULL,
      md5sum = TRUE,
      recursive = TRUE
    )
    saveRDS(snapshot, snapshot_path)
  }

  invisible(NULL)
}
