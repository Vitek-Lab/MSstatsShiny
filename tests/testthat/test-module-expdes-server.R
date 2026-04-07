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

test_that("is_response_curve correctly identifies dose response mode", {
  # Simulate statmodel input for dose response
  mock_statmodel_input <- list()
  mock_statmodel_input[[NAMESPACE_STATMODEL$comparison_mode]] <-
    CONSTANTS_STATMODEL$comparison_mode_response_curve

  is_rc <- !is.null(mock_statmodel_input) &&
    !is.null(mock_statmodel_input[[NAMESPACE_STATMODEL$comparison_mode]]) &&
    mock_statmodel_input[[NAMESPACE_STATMODEL$comparison_mode]] ==
      CONSTANTS_STATMODEL$comparison_mode_response_curve

  expect_true(is_rc)
})

test_that("is_response_curve returns FALSE for standard comparison modes", {
  standard_modes <- c(
    CONSTANTS_STATMODEL$comparison_mode_all_pairwise,
    CONSTANTS_STATMODEL$comparison_mode_all_vs_one,
    CONSTANTS_STATMODEL$comparison_mode_custom_pairwise
  )

  for (mode in standard_modes) {
    mock_statmodel_input <- list()
    mock_statmodel_input[[NAMESPACE_STATMODEL$comparison_mode]] <- mode

    is_rc <- !is.null(mock_statmodel_input) &&
      !is.null(mock_statmodel_input[[NAMESPACE_STATMODEL$comparison_mode]]) &&
      mock_statmodel_input[[NAMESPACE_STATMODEL$comparison_mode]] ==
        CONSTANTS_STATMODEL$comparison_mode_response_curve

    expect_false(is_rc, info = paste("Should not be response curve for mode:", mode))
  }
})

test_that("is_response_curve returns FALSE for NULL input", {
  mock_statmodel_input <- NULL

  is_rc <- !is.null(mock_statmodel_input) &&
    !is.null(mock_statmodel_input[[NAMESPACE_STATMODEL$comparison_mode]]) &&
    mock_statmodel_input[[NAMESPACE_STATMODEL$comparison_mode]] ==
      CONSTANTS_STATMODEL$comparison_mode_response_curve

  expect_false(is_rc)
})