# Test ZABNB distribution functions
# Using tinytest framework

# Skip tests if gamlss.dist is not available
if (!requireNamespace("gamlss.dist", quietly = TRUE)) {
  exit_file("gamlss.dist not available - skipping ZABNB validation tests")
}

# Load required libraries
suppressMessages(library(gamlss.dist))

# Set tolerance for floating point comparisons
tolerance <- sqrt(.Machine$double.eps)

# Test data generators
generate_test_data <- function(n = 100, seed = 123) {
  set.seed(seed)
  list(
    x = sample(0:20, n, replace = TRUE),
    q = sample(0:15, n, replace = TRUE),
    p = runif(n, 0.001, 0.999),
    mu = runif(n, 0.5, 5),
    sigma = runif(n, 0.1, 2),
    nu = runif(n, 0.1, 3)
  )
}

generate_edge_case_data <- function(seed = 456) {
  set.seed(seed)
  n <- 50
  list(
    x = sample(0:10, n, replace = TRUE),
    q = sample(0:8, n, replace = TRUE),
    p = runif(n, 0.001, 0.999),
    mu = runif(n, 0.5, 3),
    sigma = runif(n, 0.1, 1),
    nu = runif(n, 0.1, 2)
  )
}

# Generate test data
data <- generate_test_data()
edge_data <- generate_edge_case_data()

# Test 7: Quantile basic correctness (subset for speed)
test_indices <- sample(length(data$p), 20)
p_test <- data$p[test_indices]
mu_test <- data$mu[test_indices]
sigma_test <- data$sigma[test_indices]
nu_test <- data$nu[test_indices]


# =============================================================================
# ZERO INFLATED TESTS
# =============================================================================

# Generate tau parameter
tau_test <- runif(20, 0.01, 0.3)

# Test 10: ZABNB quantile
qtl_za_ck <- fqZABNB(p_test, mu_test, sigma_test, nu_test, tau_test)
qtl_za_ref <- gamlss.dist::qZABNB(p_test, mu_test, sigma_test, nu_test, tau_test)

expect_equal(
  qtl_za_ck,
  qtl_za_ref,
  info = "ZABNB: Quantile test"
)

# =============================================================================
# PARAMETER RECYCLING TESTS
# =============================================================================




# Test 14: Quantile parameter recycling with 5 parameters (ZABNB)
p_vec <- c(0.1, 0.3, 0.5, 0.7)
mu_vec <- c(1, 2)
sigma_single <- 0.5
nu_vec <- c(1, 1.5)
tau_single <- 0.1

qtl_recycled <- fqZABNB(p_vec, mu_vec, sigma_single, nu_vec, tau_single)
qtl_expected <- fqZABNB(p_vec, rep(mu_vec, length.out = 4), 
                        rep(sigma_single, 4), rep(nu_vec, length.out = 4),
                        rep(tau_single, 4))

expect_equal(
  qtl_recycled,
  qtl_expected,
  info = "ZABNB: Parameter recycling test"
)

# =============================================================================
# PERFORMANCE AND EDGE CASE TESTS
# =============================================================================



# cat("All ZABNB distribution tests completed successfully!\n")
