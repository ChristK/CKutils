# =============================================================================
# Tests for fscramble_trajectories
# =============================================================================

# Load dqrng for seeding
if (!requireNamespace("dqrng", quietly = TRUE)) {
  exit_file("dqrng package required for scramble trajectories tests")
}

# Test input validation
test_data_scramble <- c(0.1, 0.2, 0.3, 0.5, 0.6, 0.7)
test_pid_scramble <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)

# Test basic input validation
expect_error(
  fscramble_trajectories(test_data_scramble, test_pid_scramble, jumpiness = 0.0),
  info = "Zero jumpiness should raise error"
)

expect_error(
  fscramble_trajectories(test_data_scramble, test_pid_scramble, jumpiness = -0.5),
  info = "Negative jumpiness should raise error"
)

# Test length mismatch
expect_error(
  fscramble_trajectories(test_data_scramble, c(TRUE, FALSE), jumpiness = 1.0),
  info = "Length mismatch between x and pid should raise error"
)

# Test with valid parameters - in place = FALSE
dqrng::dqset.seed(42)
result_not_inplace <- fscramble_trajectories(test_data_scramble, test_pid_scramble, 
                                           jumpiness = 1.0, inplace = FALSE)

expect_true(is.numeric(result_not_inplace), info = "Function returns numeric vector when inplace = FALSE")
expect_equal(length(result_not_inplace), length(test_data_scramble), 
             info = "Output length matches input length")

# Test that trajectory starting points are preserved
trajectory_starts <- which(test_pid_scramble)
expect_equal(result_not_inplace[trajectory_starts], test_data_scramble[trajectory_starts],
             info = "Trajectory starting points should be preserved")

# Test with valid parameters - in place = TRUE
test_data_copy <- copy(test_data_scramble) # Create a copy to avoid modifying original data
dqrng::dqset.seed(42)
expect_null(fscramble_trajectories(test_data_copy, test_pid_scramble, jumpiness = 1.0, inplace = TRUE), 
 info = "Function returns NULL when inplace = TRUE")
expect_equal(test_data_copy[trajectory_starts], test_data_scramble[trajectory_starts],
             info = "Trajectory starting points preserved in-place")

# Test that values are scrambled (not identical)
expect_false(identical(test_data_copy, test_data_scramble), 
             info = "In-place modification should change the original vector")

# Test reproducibility with same seed
test_data1 <- test_data_scramble
test_data2 <- test_data_scramble
dqrng::dqset.seed(123)
result1 <- fscramble_trajectories(test_data1, test_pid_scramble, jumpiness = 0.5, inplace = FALSE)
dqrng::dqset.seed(123)
result2 <- fscramble_trajectories(test_data2, test_pid_scramble, jumpiness = 0.5, inplace = FALSE)

expect_equal(result1, result2, info = "Same seed should produce identical results")

# Test different jumpiness values
dqrng::dqset.seed(456)
result_low_jump <- fscramble_trajectories(test_data_scramble, test_pid_scramble, 
                                         jumpiness = 0.1, inplace = FALSE)
dqrng::dqset.seed(456)
result_high_jump <- fscramble_trajectories(test_data_scramble, test_pid_scramble, 
                                          jumpiness = 10.0, inplace = FALSE)

# Higher jumpiness should generally produce more variation (though this is stochastic)
var_low <- var(result_low_jump - test_data_scramble)
var_high <- var(result_high_jump - test_data_scramble)
expect_true(var_high >= var_low, info = "Higher jumpiness should produce more variation")

# Test with single trajectory
single_traj_data <- c(0.1, 0.2, 0.3, 0.4, 0.5)
single_traj_pid <- c(TRUE, FALSE, FALSE, FALSE, FALSE)

dqrng::dqset.seed(789)
single_result <- fscramble_trajectories(single_traj_data, single_traj_pid, 
                                       jumpiness = 1.0, inplace = FALSE)

expect_equal(single_result[1], single_traj_data[1], 
             info = "First value of single trajectory should be preserved")
expect_true(length(single_result) == length(single_traj_data),
            info = "Single trajectory output length should match input")

# Test with multiple trajectories
multi_data <- c(0.1, 0.2, 0.3, 0.5, 0.6, 0.7, 0.8, 0.9, 0.4, 0.3, 0.2)
multi_pid <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)

dqrng::dqset.seed(101)
multi_result <- fscramble_trajectories(multi_data, multi_pid, 
                                      jumpiness = 0.8, inplace = FALSE)

# Check that all trajectory starts are preserved
multi_starts <- which(multi_pid)
expect_equal(multi_result[multi_starts], multi_data[multi_starts],
             info = "All trajectory starting points should be preserved")

# Test edge case: single value trajectory
single_val_data <- c(0.5)
single_val_pid <- c(TRUE)

single_val_result <- fscramble_trajectories(single_val_data, single_val_pid, 
                                           jumpiness = 2.0, inplace = FALSE)
expect_equal(single_val_result, single_val_data, 
             info = "Single value trajectory should remain unchanged")

# Test edge case: all values are trajectory starts
all_starts_data <- c(0.1, 0.2, 0.3, 0.4)
all_starts_pid <- c(TRUE, TRUE, TRUE, TRUE)

all_starts_result <- fscramble_trajectories(all_starts_data, all_starts_pid, 
                                           jumpiness = 1.0, inplace = FALSE)
expect_equal(all_starts_result, all_starts_data,
             info = "When all values are trajectory starts, no scrambling should occur")

# Test boundary behavior (values near 0 and 1)
boundary_data <- c(0.01, 0.02, 0.99, 0.98, 0.5, 0.6)
boundary_pid <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)

dqrng::dqset.seed(202)
boundary_result <- fscramble_trajectories(boundary_data, boundary_pid, 
                                         jumpiness = 5.0, inplace = FALSE)

# Check that values stay within valid bounds (> 0 and < 1)
expect_true(all(boundary_result > 0.0), info = "All scrambled values should be > 0")
expect_true(all(boundary_result < 1.0), info = "All scrambled values should be < 1")

# Test with zero jumpiness edge case (should error)
expect_error(
  fscramble_trajectories(test_data_scramble, test_pid_scramble, jumpiness = 0.0),
  info = "Zero jumpiness should raise an error"
)

# Test type consistency
expect_true(is.numeric(boundary_result), info = "Output should be numeric")
expect_true(is.double(boundary_result), info = "Output should be double precision")

# Test that the function doesn't modify input when inplace = FALSE
original_data <- c(0.1, 0.2, 0.3, 0.4)
original_pid <- c(TRUE, FALSE, FALSE, FALSE)
data_copy <- original_data

result_copy_test <- fscramble_trajectories(data_copy, original_pid, 
                                          jumpiness = 1.0, inplace = FALSE)
expect_equal(data_copy, original_data, 
             info = "Original data should not be modified when inplace = FALSE")

# Test large vector performance (basic functionality check)
large_n <- 1000
large_data <- runif(large_n, 0.1, 0.9)
large_pid <- rep(FALSE, large_n)
large_pid[seq(1, large_n, by = 50)] <- TRUE  # Trajectory starts every 50 elements

dqrng::dqset.seed(303)
large_result <- fscramble_trajectories(large_data, large_pid, 
                                      jumpiness = 1.0, inplace = FALSE)

expect_equal(length(large_result), large_n, info = "Large vector should maintain length")
expect_true(all(large_result > 0.0), info = "Large vector results should be > 0")
expect_true(all(large_result < 1.0), info = "Large vector results should be < 1")

# Verify trajectory starts are preserved in large vector
large_starts <- which(large_pid)
expect_equal(large_result[large_starts], large_data[large_starts],
             info = "Trajectory starts should be preserved in large vector")

# cat("All fscramble_trajectories tests completed successfully!\n")
