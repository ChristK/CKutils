# Generic Distribution Benchmarking Function for CKutils vs gamlss.dist
# This script provides a flexible benchmarking framework for any distribution

# Load required packages
if (!requireNamespace("microbenchmark", quietly = TRUE)) {
  install.packages("microbenchmark")
}
if (!requireNamespace("gamlss.dist", quietly = TRUE)) {
  install.packages("gamlss.dist")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(microbenchmark)
library(gamlss.dist)
library(ggplot2)
library(CKutils)

#' Benchmark Distribution Functions Between CKutils and gamlss.dist
#'
#' This function provides comprehensive benchmarking of distribution functions
#' between CKutils and gamlss.dist packages, including accuracy verification
#' and performance comparison.
#'
#' @param distribution Character string specifying the distribution name (e.g., "DPO", "DEL", "BCPEo", "BCT", "BNB")
#' @param n_obs Vector of sample sizes to test. Default: c(1000, 10000)
#' @param n_iterations Number of benchmark iterations per test. Default: 20
#' @param test_quantiles Logical, whether to test quantile functions. Default: TRUE
#' @param save_plot Logical, whether to create and save performance plot. Set to TRUE to enable plot generation. Default: FALSE
#' @param plot_width Plot width in inches (only used if save_plot = TRUE). Default: 10
#' @param plot_height Plot height in inches (only used if save_plot = TRUE). Default: 6
#' @param verbose Logical, whether to print detailed output. If FALSE, only shows speedup ratios and accuracy tests. Default: FALSE
#' @param seed Random seed for reproducibility. Default: 123
#'
#' @return List containing benchmark results, speedup ratios, and accuracy metrics
#'
#' @examples
#' # Benchmark DPO distribution with default settings (no plot)
#' results_dpo <- benchmark_distribution("DPO")
#'
#' # Benchmark DEL distribution with custom parameters
#' results_del <- benchmark_distribution("DEL", n_obs = c(5000, 50000), n_iterations = 10)
#'
#' # Benchmark BCPEo distribution without quantile testing
#' results_bcpeo <- benchmark_distribution("BCPEo", test_quantiles = FALSE)
#'
#' # Benchmark BCT distribution with plot generation enabled
#' results_bct <- benchmark_distribution("BCT", save_plot = TRUE)
#'
#' # Benchmark with verbose output enabled
#' results_verbose <- benchmark_distribution("DPO", verbose = TRUE)
#'
#' @export
benchmark_distribution <- function(distribution,
                                   n_obs = 1e3, # c(100, 1000),
                                   n_iterations = 20,
                                   test_quantiles = TRUE,
                                   save_plot = FALSE,
                                   plot_width = 10,
                                   plot_height = 6,
                                   verbose = FALSE,
                                   seed = 123) {
  
  # Validate inputs
  if (!is.character(distribution) || length(distribution) != 1) {
    stop("distribution must be a single character string")
  }
  
  # distribution <- toupper(distribution)
  
  # First, check if the distribution exists in gamlss.dist
  gamlss_d_name <- paste0("d", distribution)
  gamlss_p_name <- paste0("p", distribution)  
  gamlss_q_name <- paste0("q", distribution)
  
  if (!exists(gamlss_d_name, envir = asNamespace("gamlss.dist"))) {
    stop(paste("Distribution", distribution, "not found in gamlss.dist package"))
  }
  
  # Check if CKutils implementations exist
  ckutils_d_name <- paste0("fd", distribution)
  ckutils_p_name <- paste0("fp", distribution)
  ckutils_q_name <- paste0("fq", distribution)
  
  has_ckutils_density <- exists(ckutils_d_name, envir = asNamespace("CKutils"))
  has_ckutils_cdf <- exists(ckutils_p_name, envir = asNamespace("CKutils"))
  has_ckutils_quantile <- exists(ckutils_q_name, envir = asNamespace("CKutils"))
  
  if (!has_ckutils_density && !has_ckutils_cdf && !has_ckutils_quantile) {
    stop(paste("No CKutils implementations found for distribution", distribution, 
               "\nLooked for:", ckutils_d_name, ",", ckutils_p_name, ",", ckutils_q_name))
  }
  
  # Warn about partial implementations
  if (!has_ckutils_density || !has_ckutils_cdf || (!has_ckutils_quantile && test_quantiles)) {
    if (verbose) {
      cat("WARNING: Partial CKutils implementation for", distribution, "distribution:\n")
      cat("  Density function", ckutils_d_name, ":", ifelse(has_ckutils_density, "Available", "Missing"), "\n")
      cat("  CDF function", ckutils_p_name, ":", ifelse(has_ckutils_cdf, "Available", "Missing"), "\n")
      if (test_quantiles) {
        cat("  Quantile function", ckutils_q_name, ":", ifelse(has_ckutils_quantile, "Available", "Missing"), "\n")
      }
      cat("  Proceeding with available functions only...\n\n")
    }
  }
  
  # Auto-detect parameters by inspecting the gamlss.dist function
  gamlss_d_func <- get(gamlss_d_name, envir = asNamespace("gamlss.dist"))
  func_args <- names(formals(gamlss_d_func))
  
  # Standard parameter names to look for (excluding x, q, p and control parameters)
  standard_params <- c("mu", "sigma", "nu", "tau")
  control_params <- c("x", "q", "p", "log", "log.p", "log_", "lower.tail", "lower_tail")
  
  detected_params <- intersect(func_args, standard_params)
  
  if (length(detected_params) == 0) {
    stop(paste("Could not detect distribution parameters for", distribution, 
               "\nFunction arguments:", paste(func_args, collapse = ", ")))
  }
  
  if (verbose) {
    cat("Auto-detected parameters for", distribution, "distribution:", paste(detected_params, collapse = ", "), "\n")
  }
  
  # Auto-generate parameter configuration
  config <- list(
    params = detected_params,
    ranges = list(),
    data_lambda = ifelse(length(detected_params) <= 2, 10, 5),
    type = "unknown"  # Will be determined by testing
  )
  
  # Set default parameter ranges
  for (param in detected_params) {
    config$ranges[[param]] <- switch(param,
      "mu" = c(1, 5),
      "sigma" = c(0.1, 2),
      "nu" = c(-1, 1),
      "tau" = c(1, 5),
      c(0.1, 2)  # default range
    )
  }
  
  # Apply distribution-specific parameter constraints
  if (distribution == "BNB") {
    # Beta Negative Binomial: nu must be > 0
    if ("nu" %in% detected_params) {
      config$ranges[["nu"]] <- c(0.1, 2)
    }
  } else if (distribution == "DEL") {
    # Delaporte: nu must be between 0 and 1
    if ("nu" %in% detected_params) {
      config$ranges[["nu"]] <- c(0.01, 0.99)
    }
  } else if (distribution %in% c("BCT", "BCTo")) {
    # Box-Cox t: nu and tau have specific ranges
    if ("nu" %in% detected_params) {
      config$ranges[["nu"]] <- c(-2, 2)
    }
    if ("tau" %in% detected_params) {
      config$ranges[["tau"]] <- c(0.1, 10)
    }
  } else if (distribution %in% c("BCPE", "BCPEo")) {
    # Box-Cox Power Exponential: tau must be > 0
    if ("tau" %in% detected_params) {
      config$ranges[["tau"]] <- c(0.1, 5)
    }
  } else if (distribution == "ZINB") {
    # Zero Inflated Negative Binomial: nu must be > 0
    if ("nu" %in% detected_params) {
      config$ranges[["nu"]] <- c(0.1, 2)
    }
  } else if (distribution == "NBI") {
    # Negative Binomial Type I: sigma must be > 0
    if ("sigma" %in% detected_params) {
      config$ranges[["sigma"]] <- c(0.1, 2)
    }
  } else if (distribution == "NBII") {
    # Negative Binomial Type II: sigma must be > 0
    if ("sigma" %in% detected_params) {
      config$ranges[["sigma"]] <- c(0.1, 2)
    }
  } else if (distribution %in% c("GA", "GG", "IG", "LG")) {
    # Gamma family: sigma must be > 0
    if ("sigma" %in% detected_params) {
      config$ranges[["sigma"]] <- c(0.1, 2)
    }
  } else if (distribution %in% c("WEI", "WEI2", "WEI3")) {
    # Weibull family: sigma must be > 0
    if ("sigma" %in% detected_params) {
      config$ranges[["sigma"]] <- c(0.1, 2)
    }
  } else if (distribution == "ZANBI") {
    # Zero-Altered Negative Binomial Type I: nu must be between 0 and 1, sigma > 0
    if ("nu" %in% detected_params) {
      config$ranges[["nu"]] <- c(0.01, 0.99)
    }
    if ("sigma" %in% detected_params) {
      config$ranges[["sigma"]] <- c(0.1, 2)
    }
  } else if (distribution == "MN4") {
    # Multinomial 4-parameter: nu must be > 0, sigma > 0
    if ("nu" %in% detected_params) {
      config$ranges[["nu"]] <- c(0.1, 5)
    }
    if ("sigma" %in% detected_params) {
      config$ranges[["sigma"]] <- c(0.1, 2)
    }
  }
  
  # Try to determine if distribution is discrete or continuous
  # First check if it's a known discrete distribution
  discrete_distributions <- c("DPO", "PO", "NBI", "NBII", "BNB", "DEL", "ZINB", "ZIP", "ZIB", "BB", "BI", "GEO", "YULE", "SICHEL", "LG", "ZANBI", "MN4")
  continuous_distributions <- c("BCT", "BCPE", "BCPEo", "BCTo", "GA", "GG", "IG", "LO", "NO", "WEI", "WEI2", "WEI3", "EXP", "LOGNO", "TF")
  
  if (distribution %in% discrete_distributions) {
    config$type <- "discrete"
  } else if (distribution %in% continuous_distributions) {
    config$type <- "continuous"
  } else {
    # Test with a small sample for unknown distributions
    if (distribution == "MN4") {
      test_x_discrete <- 1:4  # MN4 specific values
    } else {
      test_x_discrete <- 1:5
    }
    test_x_continuous <- c(0.1, 0.5, 1.0, 2.0, 5.0)
    test_params <- lapply(config$ranges, function(r) rep(mean(r), 5))
    
    try_discrete <- try({
      test_args_discrete <- c(list(test_x_discrete), test_params)
      do.call(gamlss_d_func, test_args_discrete)
      TRUE
    }, silent = TRUE)
    
    try_continuous <- try({
      test_args_continuous <- c(list(test_x_continuous), test_params)
      do.call(gamlss_d_func, test_args_continuous)
      TRUE
    }, silent = TRUE)
    
    if (!inherits(try_discrete, "try-error") && !inherits(try_continuous, "try-error")) {
      config$type <- "continuous"  # Assume continuous if both work
    } else if (!inherits(try_discrete, "try-error")) {
      config$type <- "discrete"
    } else if (!inherits(try_continuous, "try-error")) {
      config$type <- "continuous"
    } else {
      warning("Could not determine if distribution is discrete or continuous. Assuming continuous.")
      config$type <- "continuous"
    }
  }
  
  if (verbose) {
    cat("Detected distribution type:", config$type, "\n\n")
  }
  
  # Function name mapping with existence checking
  get_function_names <- function(dist, prefix) {
    # For CKutils, the actual function names are:
    # - Density: fdXXX (e.g., fdBCT, fdDPO)
    # - CDF: fpXXX (e.g., fpBCT, fpDPO) 
    # - Quantile: fqXXX (e.g., fqBCT, fqDPO)
    ckutils_name <- switch(prefix,
                          "f" = paste0("fd", dist),    # density
                          "fp" = paste0("fp", dist),   # CDF
                          "fq" = paste0("fq", dist))   # quantile
    
    # Handle gamlss.dist naming (standard d/p/q prefixes)
    gamlss_prefix <- switch(prefix,
                           "f" = "d",      # density
                           "fp" = "p",     # CDF  
                           "fq" = "q")     # quantile
    
    gamlss_name <- paste0(gamlss_prefix, dist)
    
    # Check if CKutils function exists
    ckutils_exists <- exists(ckutils_name, envir = asNamespace("CKutils"))
    
    return(list(ckutils = ckutils_name, gamlss = gamlss_name, ckutils_exists = ckutils_exists))
  }
  
  # Generate test data function
  generate_test_data <- function(N, config, seed_val) {
    set.seed(seed_val)
    
    if (config$type == "discrete") {
      if (distribution == "MN4") {
        # MN4 (Multinomial 4) only accepts values 1, 2, 3, 4
        x <- sample(1:4, N, replace = TRUE)
        q <- sample(1:4, N, replace = TRUE)
      } else {
        x <- rpois(N, lambda = config$data_lambda)
        q <- rpois(N, lambda = config$data_lambda)
      }
    } else {
      if (distribution == "MN4") {
        # MN4 is actually discrete despite being in continuous list
        x <- sample(1:4, N, replace = TRUE)
        q <- sample(1:4, N, replace = TRUE)
      } else {
        x <- runif(N, 0.1, 20)  # Positive continuous values
        q <- runif(N, 0.1, 20)
      }
    }
    
    p <- runif(N, 0.01, 0.99)
    
    # Generate parameters
    param_values <- list()
    for (param in config$params) {
      range_vals <- config$ranges[[param]]
      param_values[[param]] <- runif(N, range_vals[1], range_vals[2])
    }
    
    return(list(x = x, q = q, p = p, params = param_values))
  }
  
  # Parameter handling for function calls
  build_param_list <- function(param_values, indices = NULL) {
    if (is.null(indices)) {
      return(param_values)
    } else {
      return(lapply(param_values, function(x) x[indices]))
    }
  }
  
  if (verbose) {
    cat("=== ", distribution, " Distribution Functions Speed Benchmark ===\n\n")
  }
  
  results_summary <- list()
  
  for (N in n_obs) {
    if (verbose) {
      cat("Testing with N =", format(N, big.mark = ","), "observations\n")
      cat("----------------------------------------\n")
    }
    
    # Generate test data
    test_data <- generate_test_data(N, config, seed)
    
    # Verify accuracy (small sample)
    if (N <= max(n_obs) && N >= 1000) {
      small_indices <- 1:min(10, N)
      small_data <- list(
        x = test_data$x[small_indices],
        q = test_data$q[small_indices], 
        p = test_data$p[small_indices],
        params = build_param_list(test_data$params, small_indices)
      )
      
      cat("Accuracy verification (max absolute difference):\n")
      
      # Get function names
      d_funcs <- get_function_names(distribution, "f")
      p_funcs <- get_function_names(distribution, "fp")
      
      # Density accuracy check (only if CKutils function exists)
      if (d_funcs$ckutils_exists) {
        ckutils_d_func <- get(d_funcs$ckutils)
        gamlss_d_func <- get(d_funcs$gamlss, envir = asNamespace("gamlss.dist"))
        
        ckutils_d_args <- c(list(small_data$x), small_data$params)
        gamlss_d_args <- c(list(small_data$x), small_data$params)
        
        our_d <- do.call(ckutils_d_func, ckutils_d_args)
        gamlss_d <- do.call(gamlss_d_func, gamlss_d_args)
        max_diff_d <- max(abs(our_d - gamlss_d))
        cat(sprintf("  Density:  %.2e\n", max_diff_d))
      } else {
        cat("  Density:  (CKutils function not available)\n")
      }
      
      # CDF accuracy check (only if CKutils function exists)
      if (p_funcs$ckutils_exists) {
        ckutils_p_func <- get(p_funcs$ckutils)
        gamlss_p_func <- get(p_funcs$gamlss, envir = asNamespace("gamlss.dist"))
        
        ckutils_p_args <- c(list(small_data$q), small_data$params)
        gamlss_p_args <- c(list(small_data$q), small_data$params)
        
        our_p <- do.call(ckutils_p_func, ckutils_p_args)
        gamlss_p <- do.call(gamlss_p_func, gamlss_p_args)
        max_diff_p <- max(abs(our_p - gamlss_p))
        cat(sprintf("  CDF:      %.2e\n", max_diff_p))
      } else {
        cat("  CDF:      (CKutils function not available)\n")
      }
      
      # Quantile accuracy check if enabled (only if CKutils function exists)
      if (test_quantiles) {
        q_funcs <- get_function_names(distribution, "fq")
        
        if (q_funcs$ckutils_exists) {
          ckutils_q_func <- get(q_funcs$ckutils)
          gamlss_q_func <- get(q_funcs$gamlss, envir = asNamespace("gamlss.dist"))
          
          ckutils_q_args <- c(list(small_data$p), small_data$params)
          gamlss_q_args <- c(list(small_data$p), small_data$params)
          
          our_q <- do.call(ckutils_q_func, ckutils_q_args)
          gamlss_q <- do.call(gamlss_q_func, gamlss_q_args)
          max_diff_q <- max(abs(our_q - gamlss_q))
          cat(sprintf("  Quantile: %.2e\n", max_diff_q))
        } else {
          cat("  Quantile: (CKutils function not available)\n")
        }
      }
      cat("\n")
    }
    
    # Prepare benchmark arguments
    density_args_ck <- c(list(test_data$x), test_data$params)
    density_args_gm <- c(list(test_data$x), test_data$params)
    
    cdf_args_ck <- c(list(test_data$q), test_data$params)
    cdf_args_gm <- c(list(test_data$q), test_data$params)
    
    # Benchmark density functions (only if CKutils function exists)
    mb_density <- NULL
    d_funcs <- get_function_names(distribution, "f")
    
    if (d_funcs$ckutils_exists) {
      if (verbose) {
        cat("Benchmarking density functions...\n")
      }
      mb_density <- microbenchmark(
        CKutils = do.call(get(d_funcs$ckutils), density_args_ck),
        gamlss.dist = do.call(get(d_funcs$gamlss, envir = asNamespace("gamlss.dist")), density_args_gm),
        times = n_iterations,
        unit = "ms"
      )
    } else {
      if (verbose) {
        cat("Skipping density function benchmark (CKutils function not available)\n")
      }
    }
    
    # Benchmark CDF functions (only if CKutils function exists)
    mb_cdf <- NULL  
    p_funcs <- get_function_names(distribution, "fp")
    
    if (p_funcs$ckutils_exists) {
      if (verbose) {
        cat("Benchmarking CDF functions...\n")
      }
      mb_cdf <- microbenchmark(
        CKutils = do.call(get(p_funcs$ckutils), cdf_args_ck),
        gamlss.dist = do.call(get(p_funcs$gamlss, envir = asNamespace("gamlss.dist")), cdf_args_gm),
        times = n_iterations,
        unit = "ms"
      )
    } else {
      if (verbose) {
        cat("Skipping CDF function benchmark (CKutils function not available)\n")
      }
    }
    
    # Benchmark quantile functions if enabled (only if CKutils function exists)
    mb_quantile <- NULL
    if (test_quantiles) {
      q_funcs <- get_function_names(distribution, "fq")
      
      if (q_funcs$ckutils_exists) {
        if (verbose) {
          cat("Benchmarking quantile functions...\n")
        }
        quantile_args_ck <- c(list(test_data$p), test_data$params)
        quantile_args_gm <- c(list(test_data$p), test_data$params)
        
        mb_quantile <- microbenchmark(
          CKutils = do.call(get(q_funcs$ckutils), quantile_args_ck),
          gamlss.dist = do.call(get(q_funcs$gamlss, envir = asNamespace("gamlss.dist")), quantile_args_gm),
          times = n_iterations,
          unit = "ms"
        )
      } else {
        if (verbose) {
          cat("Skipping quantile function benchmark (CKutils function not available)\n")
        }
      }
    }
    
    # Print results
    if (verbose) {
      if (!is.null(mb_density)) {
        cat("\nDensity function results:\n")
        print(summary(mb_density))
      }
      
      if (!is.null(mb_cdf)) {
        cat("\nCDF function results:\n")
        print(summary(mb_cdf))
      }
      
      if (!is.null(mb_quantile)) {
        cat("\nQuantile function results:\n")
        print(summary(mb_quantile))
      }
    }
    
    # Calculate speedup ratios
    speedup_d <- NA
    speedup_p <- NA
    speedup_q <- NA
    
    if (!is.null(mb_density)) {
      d_ckutils <- median(mb_density[mb_density$expr == "CKutils", "time"])
      d_gamlss <- median(mb_density[mb_density$expr == "gamlss.dist", "time"])
      speedup_d <- d_gamlss / d_ckutils
    }
    
    if (!is.null(mb_cdf)) {
      p_ckutils <- median(mb_cdf[mb_cdf$expr == "CKutils", "time"])
      p_gamlss <- median(mb_cdf[mb_cdf$expr == "gamlss.dist", "time"])
      speedup_p <- p_gamlss / p_ckutils
    }
    
    if (!is.null(mb_quantile)) {
      q_ckutils <- median(mb_quantile[mb_quantile$expr == "CKutils", "time"])
      q_gamlss <- median(mb_quantile[mb_quantile$expr == "gamlss.dist", "time"])
      speedup_q <- q_gamlss / q_ckutils
    }
    
    # Store results
    result_entry <- list(
      N = N,
      speedup_density = speedup_d,
      speedup_cdf = speedup_p,
      speedup_quantile = speedup_q,
      mb_density = mb_density,
      mb_cdf = mb_cdf,
      mb_quantile = mb_quantile
    )
    results_summary[[paste0("N_", N)]] <- result_entry
    
    # Print concise results
    cat("N =", format(N, big.mark = ","), "- Speedup ratios (CKutils vs gamlss.dist):\n")
    
    if (!is.na(speedup_d)) {
      cat(sprintf(" Density: %.2fx faster\n", speedup_d))
    }
    
    if (!is.na(speedup_p)) {
      cat(sprintf(" CDF: %.2fx faster\n", speedup_p))
    }
    
    if (test_quantiles && !is.na(speedup_q)) {
      cat(sprintf(" Quantile: %.2fx faster\n", speedup_q))
    }
    
    # Calculate average speedup for available functions only
    available_speedups <- c(speedup_d, speedup_p, speedup_q)
    available_speedups <- available_speedups[!is.na(available_speedups)]
    
    if (length(available_speedups) > 0) {
      avg_speedup <- mean(available_speedups)
      cat(sprintf(" Average: %.2fx faster\n", avg_speedup))
    }
    cat("\n")
    
    if (verbose) {
      cat("\nSpeedup ratios (CKutils vs gamlss.dist):\n")
      
      if (!is.na(speedup_d)) {
        cat(sprintf("  Density:  %.2fx faster\n", speedup_d))
      } else {
        cat("  Density:  (not benchmarked)\n")
      }
      
      if (!is.na(speedup_p)) {
        cat(sprintf("  CDF:      %.2fx faster\n", speedup_p))
      } else {
        cat("  CDF:      (not benchmarked)\n")
      }
      
      if (test_quantiles) {
        if (!is.na(speedup_q)) {
          cat(sprintf("  Quantile: %.2fx faster\n", speedup_q))
        } else {
          cat("  Quantile: (not benchmarked)\n")
        }
      }
      
      if (length(available_speedups) > 0) {
        cat(sprintf("  Average:  %.2fx faster\n", avg_speedup))
      } else {
        cat("  Average:  (no benchmarks available)\n")
      }
      
      cat("\n", rep("=", 60), "\n\n")
    }
  }
  
  # Create visualization for the largest dataset
  if (save_plot && length(n_obs) > 0) {
    if (verbose) {
      cat("Creating performance visualization...\n")
    }
    
    N_viz <- max(n_obs)
    viz_data <- generate_test_data(N_viz, config, seed)
    
    # Build benchmark expressions
    d_funcs <- get_function_names(distribution, "f")
    p_funcs <- get_function_names(distribution, "fp")
    
    # Create list of expressions for benchmarking
    viz_exprs <- list()
    
    # Density functions (only if CKutils function exists)
    if (d_funcs$ckutils_exists) {
      viz_exprs[[paste0("f", distribution, "_CKutils")]] <- substitute(
        do.call(f_ck, c(list(x_data), param_data)),
        list(f_ck = as.symbol(d_funcs$ckutils), x_data = viz_data$x, param_data = viz_data$params)
      )
      viz_exprs[[paste0("d", distribution, "_gamlss")]] <- substitute(
        do.call(f_gm, c(list(x_data), param_data)),
        list(f_gm = get(d_funcs$gamlss, envir = asNamespace("gamlss.dist")), x_data = viz_data$x, param_data = viz_data$params)
      )
    }
    
    # CDF functions (only if CKutils function exists)
    if (p_funcs$ckutils_exists) {
      viz_exprs[[paste0("fp", distribution, "_CKutils")]] <- substitute(
        do.call(f_ck, c(list(q_data), param_data)),
        list(f_ck = as.symbol(p_funcs$ckutils), q_data = viz_data$q, param_data = viz_data$params)
      )
      viz_exprs[[paste0("p", distribution, "_gamlss")]] <- substitute(
        do.call(f_gm, c(list(q_data), param_data)),
        list(f_gm = get(p_funcs$gamlss, envir = asNamespace("gamlss.dist")), q_data = viz_data$q, param_data = viz_data$params)
      )
    }
    
    if (test_quantiles) {
      q_funcs <- get_function_names(distribution, "fq")
      if (q_funcs$ckutils_exists) {
        viz_exprs[[paste0("fq", distribution, "_CKutils")]] <- substitute(
          do.call(f_ck, c(list(p_data), param_data)),
          list(f_ck = as.symbol(q_funcs$ckutils), p_data = viz_data$p, param_data = viz_data$params)
        )
        viz_exprs[[paste0("q", distribution, "_gamlss")]] <- substitute(
          do.call(f_gm, c(list(p_data), param_data)),
          list(f_gm = get(q_funcs$gamlss, envir = asNamespace("gamlss.dist")), p_data = viz_data$p, param_data = viz_data$params)
        )
      }
    }
    
    # Check if we have any expressions to benchmark
    if (length(viz_exprs) == 0) {
      if (verbose) {
        cat("No CKutils functions available for plotting comparison\n")
      }
    } else {
      # Run combined benchmark
      mb_all <- microbenchmark(list = viz_exprs, times = max(5, n_iterations %/% 4), unit = "ms")
      
      # Process results for plotting
      mb_df <- data.frame(mb_all)
      mb_df$Package <- ifelse(grepl("CKutils", mb_df$expr), "CKutils", "gamlss.dist")
      
      pattern_density <- paste0("f", distribution, "|d", distribution)
      pattern_cdf <- paste0("fp", distribution, "|p", distribution)
      pattern_quantile <- paste0("fq", distribution, "|q", distribution)
      
      mb_df$FunctionType <- ifelse(
        grepl(pattern_density, mb_df$expr), "Density",
        ifelse(grepl(pattern_cdf, mb_df$expr), "CDF",
               ifelse(grepl(pattern_quantile, mb_df$expr), "Quantile", "Other"))
      )
      
      # Create plot
      p <- ggplot(mb_df, aes(x = FunctionType, y = time/1e6, fill = Package)) +
        geom_boxplot(alpha = 0.7) +
        scale_fill_manual(values = c("CKutils" = "#2E86AB", "gamlss.dist" = "#A23B72")) +
        labs(
          title = paste(distribution, "Distribution Functions Performance Comparison"),
          subtitle = paste("N =", format(N_viz, big.mark = ","), "observations"),
          x = "Function Type",
          y = "Time (milliseconds)",
          fill = "Package"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)
        ) +
        scale_y_log10()
      
      # Save plot
      plot_filename <- paste0(distribution, "_performance_comparison.png")
      ggsave(plot_filename, p, width = plot_width, height = plot_height, dpi = 300)
      if (verbose) {
        cat("Performance visualization saved as '", plot_filename, "'\n")
      }
      
      # Final summary
      if (verbose) {
        cat("\n=== FINAL SUMMARY ===\n")
        summary_stats <- aggregate(time ~ Package + FunctionType, mb_df, median)
        summary_stats$time_ms <- summary_stats$time / 1e6
        
        # Calculate overall speedup
        ckutils_times <- summary_stats[summary_stats$Package == "CKutils", "time"]
        gamlss_times <- summary_stats[summary_stats$Package == "gamlss.dist", "time"]
        
        if (length(ckutils_times) > 0 && length(gamlss_times) > 0) {
          overall_speedup <- mean(gamlss_times / ckutils_times, na.rm = TRUE)
          cat(sprintf("Overall speedup: %.2fx\n faster than gamlss.dist\n", overall_speedup))
          
          cat("Individual function speedups:\n")
          for (func in unique(summary_stats$FunctionType)) {
            ck_time <- summary_stats[summary_stats$Package == "CKutils" & summary_stats$FunctionType == func, "time"]
            gamlss_time <- summary_stats[summary_stats$Package == "gamlss.dist" & summary_stats$FunctionType == func, "time"]
            if (length(ck_time) > 0 && length(gamlss_time) > 0) {
              speedup <- gamlss_time / ck_time
              cat(sprintf("  %s: %.2fx\n faster\n", func, speedup))
            }
          }
        } else {
          cat("No performance comparison data available\n")
        }
      }
    }
  } else {
    if (!save_plot && verbose) {
      cat("Plot generation skipped (save_plot = FALSE)\n")
    }
  }
  
  if (verbose) {
    cat("\nBenchmark completed successfully!\n")
  }
  
  # Return comprehensive results
  if (verbose) {
  return(list(
    distribution = distribution,
    n_obs = n_obs,
    n_iterations = n_iterations,
    test_quantiles = test_quantiles,
    results_by_size = results_summary,
    config_used = config
  ))
  } else {
    return("")
  }

}

# Example usage and old benchmark code follows below for reference
cat("=== Generic Distribution Benchmarking Function Loaded ===\n")
cat("Usage examples:\n")
cat("  benchmark_distribution('DPO')\n")
cat("  benchmark_distribution('DEL', n_obs = c(5000, 50000), n_iterations = 10)\n")
cat("  benchmark_distribution('BCPEo', test_quantiles = FALSE)\n")
cat("  benchmark_distribution('BCT', save_plot = TRUE)  # Enable plot generation\n")
cat("  benchmark_distribution('DPO', verbose = TRUE)    # Enable verbose output\n\n")

# Example usage and old benchmark code follows below for reference
cat("=== Generic Distribution Benchmarking Function Loaded ===\n")
cat("Usage examples:\n")
cat("  benchmark_distribution('DPO')\n")
cat("  benchmark_distribution('DEL', n_obs = c(5000, 50000), n_iterations = 10)\n")
cat("  benchmark_distribution('BCPEo', test_quantiles = FALSE)\n")
cat("  benchmark_distribution('DPO', verbose = TRUE)    # Enable verbose output\n\n")

# Original DPO-specific benchmark code (commented out for reference)
# To run the original DPO benchmark, uncomment the following section:

# # Test different data sizes
# test_sizes <- c(1000, 10000)
# 
# for (N in test_sizes) {
#   cat("Testing with N =", N, "observations\n")
#   cat("----------------------------------------\n")
#   
#   # Generate test data
#   set.seed(123)
#   x <- rpois(N, lambda = 10)  # Integer values for discrete distribution
#   q <- rpois(N, lambda = 10)  # Integer values for discrete distribution
#   p <- runif(N, 0.01, 0.99)
#   mu <- runif(N, 1, 5)      # Mean parameter for DPO
#   sigma <- runif(N, 0.5, 2) # Dispersion parameter for DPO
#   
#   # Verify our functions match gamlss.dist (small sample)
#   if (N <= 1000) {
#     small_x <- x[1:10]
#     small_mu <- mu[1:10]
#     small_sigma <- sigma[1:10]
#     
#     our_d <- fdDPO(small_x, small_mu, small_sigma)
#     gamlss_d <- gamlss.dist::dDPO(small_x, small_mu, small_sigma)
#     max_diff_d <- max(abs(our_d - gamlss_d))
#     
#     our_p <- fpDPO(small_x, small_mu, small_sigma)
#     gamlss_p <- gamlss.dist::pDPO(small_x, small_mu, small_sigma)
#     max_diff_p <- max(abs(our_p - gamlss_p))
#     
#     small_p <- p[1:10]
#     our_q <- fqDPO(small_p, small_mu, small_sigma)
#     gamlss_q <- gamlss.dist::qDPO(small_p, small_mu, small_sigma)
#     max_diff_q <- max(abs(our_q - gamlss_q))
#     
#     cat("Accuracy verification (max absolute difference):\n")
#     cat(sprintf("  Density:  %.2e\n", max_diff_d))
#     cat(sprintf("  CDF:      %.2e\n", max_diff_p))
#     cat(sprintf("  Quantile: %.2e\n", max_diff_q))
#     cat("\n")
#   }
#   
#   # [Additional original benchmark code would continue here...]
# }
