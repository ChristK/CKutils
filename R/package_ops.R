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
#' @description Detaches a loaded R package cleanly from the current session.
#' This is useful when reinstalling or updating a package during development.
#'
#' @param pkg Character string of package name. The name of the package to
#' detach.
#'
#' @return Invisibly returns \code{TRUE} if the package was detached, or
#' \code{FALSE} if it was not attached.
#'
#' @details If the specified package is attached multiple times (e.g., via
#' multiple `library()` calls), it will be fully detached. A message is
#' displayed indicating whether the detachment occurred. The function
#' \code{pkgload::unload()} can also be used for this purpose and it is actually
#' more sophisticated.
#'
#' Note that R's dynamic library loader can load a shared object once per
#' session, but unloading and reloading the same library in the same process is
#' not guaranteed to work. In some cases, attempting to reload a package after
#' detachment may result in errors or unexpected behavior. For more details, see
#' the R documentation on dynamic libraries:
#' https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Dynamic-loading
#'
#' @examples \dontrun{ detach_package("ggplot2") }
#'
#' @seealso \code{\link{library}}, \code{\link{detach}}, \code{\link[pkgload]{unload}}
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

# Detaching my other packages is unsafe. I.e. the following always segfaults
# after a couple of iterations. pkgload::unload also crashes.
# i <- 0
# print(i)
# while (i < 10) {
#   i <- i + 1
#   library(IMPACTncdJapan)
#   CKutils::detach_package("IMPACTncdJapan")
# }




#' @title Install or Reinstall a Local R Package
#'
#' @description
#' Installs or reinstalls a local R package from the specified directory. Useful during
#' development to ensure the package is rebuilt with updated source code and documentation.
#'
#' @param pkg_path Character string. Path to the local package directory.
#'
#' @return Invisibly returns \code{NULL}. Prints installation progress and success/failure messages.
#'
#' @details
#' The function performs the following steps:
#' - Ensures 'remotes' and 'roxygen2' are installed.
#' - Generates updated documentation using \code{roxygen2::roxygenise()}.
#' - Detaches the package if already loaded.
#' - Deletes compiled files such as .o, .so, or .dll for a clean rebuild.
#' - Installs the package using \code{callr::rcmd("INSTALL", ...)}.
#'
#' @examples
#' \dontrun{
#' installLocalPackage()
#' installLocalPackage("/path/to/custom/package_dir")
#' }
#'
#' @seealso \code{\link[roxygen2]{roxygenise}}
#'
#' @export
installLocalPackage <- function(pkg_path) {
  # Detach package if already loaded
  # Read package name from DESCRIPTION file
  desc_path <- file.path(pkg_path, "DESCRIPTION")
  pkg_name <- if (file.exists(desc_path)) {
    read.dcf(desc_path, fields = "Package")[1]
  } else {
    stop("DESCRIPTION file not found in: ", pkg_path)
  }
  detach_package(pkg_name)

  rscript(
    # callr
    c(
      "-e",
      sprintf(
        "if (!require('roxygen2', quietly=TRUE)) install.packages('roxygen2'); roxygen2::roxygenise('%s', clean=TRUE)",
        pkg_path   
      )
    ),
    echo = TRUE
  )

  # Remove any compiled files to ensure clean install
  compiled_files <- list.files(
    path = pkg_path,
    pattern = "\\.o$|\\.dll$|\\.so$",
    recursive = TRUE,
    full.names = TRUE
  )
  file.remove(compiled_files)

  # Install the package from local directory
  tryCatch(
    {
      callr::rcmd(
        "INSTALL",
        c("--preclean", pkg_path),
        echo = TRUE
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
#' \href{https://www.r-bloggers.com/2014/02/function-to-simplify-loading-and-installing-packages/}{here}
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
  function(
    pkges,
    install = TRUE,
    update = FALSE,
    quiet = TRUE,
    verbose = FALSE,
    ...
  ) {
    myrequire <- function(package, ...) {
      result <- FALSE
      if (quiet) {
        suppressMessages(suppressWarnings(
          result <- requireNamespace(package, ...)
        ))
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
        if (verbose) {
          install.packages(pkgs = i, quiet = quiet)
        } else {
          suppressMessages(install.packages(pkgs = i, quiet = quiet))
        }
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