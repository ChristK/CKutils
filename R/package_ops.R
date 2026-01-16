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
#' @param debug Logical. If TRUE (default), compiles with debug flags (-O0 -g -UNDEBUG).
#'   If FALSE, uses production/release flags from Makevars (typically -O2).
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
#' When debug = FALSE, the function sets R_COMPILE_AND_INSTALL_PACKAGES environment
#' variable to prevent debug compilation flags from overriding Makevars settings.
#'
#' @examples
#' \dontrun{
#' installLocalPackage()
#' installLocalPackage("/path/to/custom/package_dir")
#' installLocalPackage("/path/to/package", debug = FALSE)  # Production build
#' }
#'
#' @seealso \code{\link[roxygen2]{roxygenise}}
#'
#' @export
installLocalPackage <- function(pkg_path, debug = TRUE) {
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
      # Set build mode based on debug parameter
      build_msg <- if (debug) "(debug build)" else "(production build)"
      message(paste0("Installing ", pkg_name, " ", build_msg))
      
      # For production builds, disable debug compilation flags
      if (!debug) {
        # Save current environment
        old_env <- Sys.getenv("R_COMPILE_AND_INSTALL_PACKAGES", unset = NA)
        Sys.setenv(R_COMPILE_AND_INSTALL_PACKAGES = "never")
        
        # Ensure cleanup on exit
        on.exit({
          if (is.na(old_env)) {
            Sys.unsetenv("R_COMPILE_AND_INSTALL_PACKAGES")
          } else {
            Sys.setenv(R_COMPILE_AND_INSTALL_PACKAGES = old_env)
          }
        }, add = TRUE)
      }
      
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
#' @param debug Logical. If TRUE (default), compiles with debug flags (-O0 -g -UNDEBUG).
#'   If FALSE, uses production/release flags from Makevars (typically -O2).
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
#' # Production build
#' installLocalPackageIfChanged(
#'   pkg_path = "./Rpackage/IMPACTncd_Engl_model_pkg/",
#'   snapshot_path = "./Rpackage/.IMPACTncd_Engl_model_pkg_snapshot.rds",
#'   debug = FALSE
#' )
#' }
#'
#' @seealso \code{\link{installLocalPackage}}, \code{\link{saveRDS}}
#'
#' @export
installLocalPackageIfChanged <- function(pkg_path, snapshot_path, debug = TRUE) {
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
    installLocalPackage(pkg_path, debug = debug)

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
#' @param ... Package names (quoted or unquoted) or a character vector of package names.
#'            When providing unquoted names, separate them with commas.
#'            When providing a character vector, it should be the first argument.
#' @param pkges (Deprecated) Use \code{...} instead. A character vector with the names of the packages to load.
#' @param install Logical. If TRUE (default), any packages not already installed will be installed.
#' @param update Logical. If TRUE, this function will install a newer version of the
#'        package if available. Default is FALSE.
#' @param quiet Logical. If TRUE (default), package startup messages will be suppressed.
#' @param verbose Logical. If TRUE, diagnostic messages will be printed. Default is FALSE.
#' @return a data frame with four columns and rownames corresponding to the
#'         packages to be loaded. The four columns are: loaded (logical
#'         indicating whether the package was successfully loaded), installed
#'         (logical indicating that the package was installed or updated),
#'         loaded.version (the version string of the installed package), and
#'         available.version (the version string of the package currently
#'         available on CRAN). Note that this only reflects packages listed in
#'         the \code{...} parameter. Other packages may be loaded and/or
#'         installed as necessary by \code{install.packages} and \code{require}.
#'         If \code{verbose=FALSE} the data frame will be returned using
#'         \code{\link{invisible}}.
#' @export
#' @examples
#' \dontrun{
#' # Using unquoted package names
#' dependencies(data.table, MASS, ggplot2)
#' 
#' # Using quoted package names (traditional way)
#' dependencies(c('data.table','MASS'))
#' 
#' # Mixed usage with options
#' dependencies(data.table, MASS, install = TRUE, quiet = FALSE)
#' }
dependencies <-
  function(
    ...,
    pkges = NULL,
    install = TRUE,
    update = FALSE,
    quiet = TRUE,
    verbose = FALSE
  ) {
    # Handle different input methods
    dots <- substitute(list(...))[-1]  # Remove the 'list' part
    
    if (length(dots) == 0 && is.null(pkges)) {
      stop("No packages specified. Provide package names as arguments or use 'pkges' parameter.")
    }
    
    # Process package names from ... (unquoted or quoted)
    if (length(dots) > 0) {
      # Check if first argument is a character vector (traditional usage)
      first_arg <- tryCatch(eval(dots[[1]], parent.frame()), error = function(e) NULL)
      
      if (length(dots) == 1 && is.character(first_arg) && length(first_arg) > 1) {
        # Traditional usage: dependencies(c("pkg1", "pkg2"))
        pkg_names <- first_arg
      } else {
        # New usage: dependencies(pkg1, pkg2) or dependencies("pkg1", "pkg2")
        pkg_names <- character(length(dots))
        for (i in seq_along(dots)) {
          arg <- dots[[i]]
          if (is.character(arg)) {
            # Already quoted
            pkg_names[i] <- arg
          } else {
            # Unquoted - convert to character
            pkg_names[i] <- as.character(arg)
          }
        }
      }
    } else {
      # Fallback to pkges parameter (backward compatibility)
      pkg_names <- pkges
    }
    
    # Input validation
    if (is.null(pkg_names) || length(pkg_names) == 0) {
      stop("No packages specified")
    }
    if (!is.character(pkg_names)) {
      stop("Package names must be character strings")
    }
    
    # Remove duplicates while preserving order
    pkges <- unique(pkg_names)
    
    myrequire <- function(package) {
      if (quiet) {
        suppressMessages(suppressWarnings(
          requireNamespace(package, quietly = TRUE)
        ))
      } else {
        suppressWarnings(requireNamespace(package, quietly = TRUE))
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
            install.packages(pkgs = pkg, quiet = quiet)
          } else {
            suppressMessages(install.packages(pkgs = pkg, quiet = quiet))
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