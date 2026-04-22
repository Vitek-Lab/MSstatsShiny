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
# EXCLUDE CONDITIONS TESTS
# ============================================================================

test_that("filter_excluded_conditions removes specified groups", {
  mat <- data.frame(
    GROUP = c("DMSO", "Drug_1nM", "Drug_10nM", "PDPD"),
    dose_value = c(0, 1, 10, 0),
    drug = c("DMSO", "Drug", "Drug", "PDPD")
  )
  result <- MSstatsShiny:::.filter_excluded_conditions(mat, c("PDPD"))
  expect_equal(nrow(result), 3)
  expect_false("PDPD" %in% result$GROUP)
})

test_that("filter_excluded_conditions returns unchanged matrix when nothing excluded", {
  mat <- data.frame(
    GROUP = c("DMSO", "Drug_1nM", "Drug_10nM"),
    dose_value = c(0, 1, 10)
  )
  result <- MSstatsShiny:::.filter_excluded_conditions(mat, NULL)
  expect_equal(nrow(result), 3)

  result2 <- MSstatsShiny:::.filter_excluded_conditions(mat, character(0))
  expect_equal(nrow(result2), 3)
})

test_that("filter_excluded_conditions handles multiple exclusions", {
  mat <- data.frame(
    GROUP = c("DMSO", "Drug_1nM", "Drug_10nM", "PDPD", "QC_Run"),
    dose_value = c(0, 1, 10, 0, 0)
  )
  result <- MSstatsShiny:::.filter_excluded_conditions(mat, c("PDPD", "QC_Run"))
  expect_equal(nrow(result), 3)
  expect_false(any(c("PDPD", "QC_Run") %in% result$GROUP))
})

# ============================================================================
# REPLICATE CHECK TESTS
# ============================================================================

test_that("check_replicates_per_dose counts correctly with multiple replicates", {
  data <- data.frame(
    protein = rep("P1", 9),
    drug = c("DMSO", "DMSO", "DMSO", "Drug1", "Drug1", "Drug1", "Drug1", "Drug1", "Drug1"),
    dose = c(0, 0, 0, 10, 10, 10, 100, 100, 100),
    response = rnorm(9)
  )
  result <- MSstatsShiny:::.check_replicates_per_dose(data, "P1")
  expect_equal(result$max_reps, 3)
  expect_equal(result$min_reps, 3)
})

test_that("check_replicates_per_dose detects single replicate", {
  data <- data.frame(
    protein = rep("P1", 4),
    drug = c("DMSO", "Drug1", "Drug1", "Drug1"),
    dose = c(0, 10, 100, 1000),
    response = rnorm(4)
  )
  result <- MSstatsShiny:::.check_replicates_per_dose(data, "P1")
  expect_equal(result$max_reps, 1)
})

test_that("check_replicates_per_dose returns zero for missing protein", {
  data <- data.frame(
    protein = "P1", drug = "Drug1", dose = 10, response = 1
  )
  result <- MSstatsShiny:::.check_replicates_per_dose(data, "P999")
  expect_equal(result$max_reps, 0)
})

test_that("check_replicates_per_dose handles uneven replicates", {
  data <- data.frame(
    protein = rep("P1", 5),
    drug = c("DMSO", "Drug1", "Drug1", "Drug1", "Drug1"),
    dose = c(0, 10, 10, 100, 100),
    response = rnorm(5)
  )
  # dose 10 has 2 reps, dose 100 has 2 reps (after excluding DMSO control which has drug="DMSO")
  result <- MSstatsShiny:::.check_replicates_per_dose(data, "P1")
  expect_equal(result$min_reps, 2)
  expect_equal(result$max_reps, 2)
})