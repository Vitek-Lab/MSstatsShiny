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