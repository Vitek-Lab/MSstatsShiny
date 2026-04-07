# ============================================================================
# EXPDES HELPER FUNCTION TESTS
# ============================================================================

test_that("get_concentrations_from_matrix extracts dose_value column", {
  mat <- data.frame(
    GROUP = c("DMSO", "Drug_1nM", "Drug_10nM"),
    dose_value = c(0, 1, 10),
    dose_unit = c("", "nM", "nM"),
    drug = c("DMSO", "Drug", "Drug")
  )
  result <- MSstatsShiny:::.get_concentrations_from_matrix(mat)
  expect_equal(result, c(0, 1, 10))
})

test_that("get_concentrations_from_matrix falls back to _value columns", {
  mat <- data.frame(
    GROUP = c("A", "B", "C"),
    treatment_value = c(0, 5, 50)
  )
  result <- MSstatsShiny:::.get_concentrations_from_matrix(mat)
  expect_equal(result, c(0, 5, 50))
})

test_that("get_concentrations_from_matrix returns NULL when no value columns exist", {
  mat <- data.frame(
    GROUP = c("A", "B"),
    condition = c("ctrl", "treated")
  )
  result <- MSstatsShiny:::.get_concentrations_from_matrix(mat)
  expect_null(result)
})

test_that("get_concentrations_from_matrix returns sorted unique values", {
  mat <- data.frame(
    dose_value = c(100, 0, 10, 100, 0, 10)
  )
  result <- MSstatsShiny:::.get_concentrations_from_matrix(mat)
  expect_equal(result, c(0, 10, 100))
})

# ============================================================================
# EXPDES MODE BRANCHING TESTS
# ============================================================================

test_that("is_response_curve_mode returns TRUE for dose response mode", {
  mock_input <- list()
  mock_input[[NAMESPACE_STATMODEL$comparison_mode]] <-
    CONSTANTS_STATMODEL$comparison_mode_response_curve

  expect_true(MSstatsShiny:::.is_response_curve_mode(mock_input))
})

test_that("is_response_curve_mode returns FALSE for all standard comparison modes", {
  standard_modes <- c(
    CONSTANTS_STATMODEL$comparison_mode_all_pairwise,
    CONSTANTS_STATMODEL$comparison_mode_all_vs_one,
    CONSTANTS_STATMODEL$comparison_mode_custom_pairwise,
    CONSTANTS_STATMODEL$comparison_mode_custom_nonpairwise
  )

  for (mode in standard_modes) {
    mock_input <- list()
    mock_input[[NAMESPACE_STATMODEL$comparison_mode]] <- mode
    expect_false(MSstatsShiny:::.is_response_curve_mode(mock_input),
                 info = paste("Should be FALSE for mode:", mode))
  }
})

test_that("is_response_curve_mode returns FALSE for NULL input", {
  expect_false(MSstatsShiny:::.is_response_curve_mode(NULL))
})

test_that("is_response_curve_mode returns FALSE when comparison_mode is missing", {
  mock_input <- list(some_other_field = "value")
  expect_false(MSstatsShiny:::.is_response_curve_mode(mock_input))
})

test_that("is_response_curve_mode returns FALSE for empty list", {
  expect_false(MSstatsShiny:::.is_response_curve_mode(list()))
})