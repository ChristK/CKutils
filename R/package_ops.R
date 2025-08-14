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
    # Input validation
    if (missing(pkges)) {
      stop("Argument 'pkges' is missing, with no default")
    }
    if (!is.character(pkges) || length(pkges) == 0) {
      stop("'pkges' must be a non-empty character vector")
    }
    
    # Remove duplicates while preserving order
    pkges <- unique(pkges)
    
    myrequire <- function(package, ...) {
      if (quiet) {
        suppressMessages(suppressWarnings(
          requireNamespace(package, ...)
        ))
      } else {
        suppressWarnings(requireNamespace(package, ...))
      }
    }
    
    mymessage <- function(msg) {
      if (verbose) {
        message(msg)
      }
    }

    # Get package information once
    installedpkgs <- installed.packages()
    all_available <- available.packages()
    
    # Handle case where no packages are available
    if (nrow(all_available) == 0) {
      warning(
        "There appear to be no packages available from the repositories. ",
        "Perhaps you are not connected to the Internet?"
      )
      availpkgs <- data.frame(
        Package = character(0), 
        Version = character(0),
        stringsAsFactors = FALSE
      )
    } else {
      availpkgs <- all_available[, c('Package', 'Version'), drop = FALSE]
      # Normalize version numbers (hyphens to dots for packageVersion compatibility)
      availpkgs[, 'Version'] <- gsub('-', '.', availpkgs[, 'Version'])
    }
    # Initialize results data frame
    results <- data.frame(
      loaded = logical(length(pkges)),
      installed = logical(length(pkges)),
      loaded.version = character(length(pkges)),
      available.version = character(length(pkges)),
      stringsAsFactors = FALSE
    )
    rownames(results) <- pkges
    
    # Process each package
    for (pkg in pkges) {
      initial_search <- search()
      needs_install <- FALSE
      
      # Check if package is already installed
      if (pkg %in% rownames(installedpkgs)) {
        current_version <- as.character(packageVersion(pkg))
        
        # Check if available on repositories
        if (pkg %in% rownames(availpkgs)) {
          available_version <- availpkgs[pkg, 'Version']
          results[pkg, 'available.version'] <- available_version
          
          # Check for version differences
          if (current_version != available_version) {
            if (!update) {
              mymessage(sprintf(
                "A different version of %s is available (current=%s; available=%s)",
                pkg, current_version, available_version
              ))
            }
            needs_install <- update
          }
        } else {
          mymessage(sprintf("%s is not available on the repositories.", pkg))
        }
      } else {
        # Package not installed - check if available for installation
        if (pkg %in% rownames(availpkgs)) {
          needs_install <- isTRUE(install)
          results[pkg, 'available.version'] <- availpkgs[pkg, 'Version']
        } else {
          warning(sprintf(
            "%s is not available on the repositories and is not installed locally",
            pkg
          ))
        }
      }
      
      # Install and/or load package
      if (needs_install || !myrequire(pkg)) {
        # Install package
        tryCatch({
          if (verbose) {
            install.packages(pkgs = pkg, quiet = quiet, ...)
          } else {
            suppressMessages(install.packages(pkgs = pkg, quiet = quiet, ...))
          }
          
          # Verify installation and loading
          if (myrequire(pkg)) {
            results[pkg, 'installed'] <- TRUE
            results[pkg, 'loaded'] <- TRUE
            results[pkg, 'loaded.version'] <- as.character(packageVersion(pkg))
          } else {
            warning(sprintf("Error loading package: %s", pkg))
          }
        }, error = function(e) {
          warning(sprintf("Failed to install package %s: %s", pkg, e$message))
        })
      } else {
        # Package loaded successfully without installation
        results[pkg, 'loaded'] <- TRUE
        results[pkg, 'loaded.version'] <- as.character(packageVersion(pkg))
      }
      
      # Clean up any side-effect package attachments and attach the target package
      final_search <- search()
      side_effect_packages <- setdiff(final_search, initial_search)
      
      for (side_pkg in side_effect_packages) {
        try(detach(side_pkg, character.only = TRUE), silent = TRUE)
      }
      
      # Attach the package we actually want
      if (quiet) {
        suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      } else {
        library(pkg, character.only = TRUE)
      }
    }
    
    # Return results
    if (verbose) {
      return(results)
    } else {
      invisible(results)
    }
  }