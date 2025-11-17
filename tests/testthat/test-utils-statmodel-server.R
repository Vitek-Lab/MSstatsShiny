# ============================================================================
# Tests for get_experimental_conditions
# ============================================================================

test_that("get_experimental_conditions returns PTM conditions for TMT", {
  loadpage_input <- list(BIO = "PTM", DDA_DIA = "TMT", filetype = "other")
  preprocess_data <- list(
    PTM = list(
      ProteinLevelData = data.frame(
        Condition = factor(c("A", "B", "A", "B"))
      )
    )
  )
  
  result <- get_experimental_conditions(loadpage_input, preprocess_data)
  expect_equal(result, levels(factor(c("A", "B"))))
})

test_that("get_experimental_conditions returns PTM GROUP for non-TMT", {
  loadpage_input <- list(BIO = "PTM", DDA_DIA = "DDA", filetype = "other")
  preprocess_data <- list(
    PTM = list(
      ProteinLevelData = data.frame(
        GROUP = factor(c("Control", "Treatment"))
      )
    )
  )
  
  result <- get_experimental_conditions(loadpage_input, preprocess_data)
  expect_equal(result, levels(factor(c("Control", "Treatment"))))
})

test_that("get_experimental_conditions returns Condition for TMT non-PTM", {
  loadpage_input <- list(BIO = "Protein", DDA_DIA = "TMT", filetype = "other")
  preprocess_data <- list(
    ProteinLevelData = data.frame(
      Condition = factor(c("X", "Y", "Z"))
    )
  )
  
  result <- get_experimental_conditions(loadpage_input, preprocess_data)
  expect_equal(result, levels(factor(c("X", "Y", "Z"))))
})

test_that("get_experimental_conditions returns GROUP for standard analysis", {
  loadpage_input <- list(BIO = "Protein", DDA_DIA = "DDA", filetype = "other")
  preprocess_data <- list(
    ProteinLevelData = data.frame(
      GROUP = factor(c("Group1", "Group2", "Group3"))
    )
  )
  
  result <- get_experimental_conditions(loadpage_input, preprocess_data)
  expect_equal(result, levels(factor(c("Group1", "Group2", "Group3"))))
})

# ============================================================================
# Tests for build_custom_pairwise_contrast
# ============================================================================

test_that("build_custom_pairwise_contrast creates correct matrix", {
  input <- list(group1 = "A", group2 = "B")
  condition_list <- c("A", "B", "C")
  contrast <- list(matrix = NULL, row = NULL)
  comp_list <- list(dList = NULL)
  row <- c(0, 0, 0)
  
  result <- build_custom_pairwise_contrast(
    input, condition_list, contrast, comp_list, row
  )
  
  expect_equal(dim(result), c(1, 3))
  expect_equal(result[1, 1], 1)
  expect_equal(result[1, 2], -1)
  expect_equal(result[1, 3], 0)
  expect_equal(rownames(result), "A vs B")
  expect_equal(colnames(result), condition_list)
})

test_that("build_custom_pairwise_contrast handles multiple comparisons", {
  input <- list(group1 = "C", group2 = "A")
  condition_list <- c("A", "B", "C")
  existing_matrix <- matrix(c(1, -1, 0), nrow = 1)
  rownames(existing_matrix) <- "A vs B"
  colnames(existing_matrix) <- condition_list
  
  contrast <- list(matrix = existing_matrix, row = NULL)
  comp_list <- list(dList = c("A vs B"))
  row <- c(0, 0, 0)
  
  result <- build_custom_pairwise_contrast(
    input, condition_list, contrast, comp_list, row
  )
  
  expect_equal(nrow(result), 2)
  expect_equal(result[2, 1], -1)
  expect_equal(result[2, 3], 1)
})

test_that("build_custom_pairwise_contrast returns unchanged matrix for same groups", {
  input <- list(group1 = "A", group2 = "A")
  condition_list <- c("A", "B", "C")
  existing_matrix <- matrix(c(1, -1, 0), nrow = 1)
  
  contrast <- list(matrix = existing_matrix, row = NULL)
  comp_list <- list(dList = c("A vs B"))
  row <- c(0, 0, 0)
  
  result <- build_custom_pairwise_contrast(
    input, condition_list, contrast, comp_list, row
  )
  
  expect_equal(result, existing_matrix)
})

# ============================================================================
# Tests for build_custom_non_pairwise_contrast
# ============================================================================

test_that("build_custom_non_pairwise_contrast creates correct matrix", {
  input <- list(
    weight1 = 0.5,
    weight2 = 0.5,
    weight3 = -1,
    comp_name = "AB vs C"
  )
  condition_list <- c("A", "B", "C")
  contrast <- list(matrix = NULL, row = NULL)
  comp_list <- list(dList = NULL)
  row <- c(0, 0, 0)
  
  result <- build_custom_non_pairwise_contrast(
    input, condition_list, contrast, comp_list, row
  )
  
  expect_equal(dim(result), c(1, 3))
  expect_equal(result[1, 1], 0.5)
  expect_equal(result[1, 2], 0.5)
  expect_equal(result[1, 3], -1)
  expect_equal(rownames(result), "AB vs C")
})

test_that("build_custom_non_pairwise_contrast rejects non-zero sum", {
  input <- list(
    weight1 = 1,
    weight2 = 1,
    weight3 = 1,
    comp_name = "Invalid"
  )
  condition_list <- c("A", "B", "C")
  existing_matrix <- matrix(c(1, -1, 0), nrow = 1)
  
  contrast <- list(matrix = existing_matrix, row = NULL)
  comp_list <- list(dList = c("A vs B"))
  row <- c(0, 0, 0)
  
  result <- build_custom_non_pairwise_contrast(
    input, condition_list, contrast, comp_list, row
  )
  
  expect_equal(result, existing_matrix)
})

# ============================================================================
# Tests for build_all_against_one_contrast
# ============================================================================

test_that("build_all_against_one_contrast creates all comparisons", {
  input <- list(group3 = "Control")
  condition_list <- c("TreatA", "TreatB", "Control")
  contrast <- list(matrix = NULL, row = NULL)
  comp_list <- list(dList = NULL)
  row <- c(0, 0, 0)
  loadpage_input <- list(BIO = "Protein", DDA_DIA = "DDA")
  
  result <- build_all_against_one_contrast(
    input, condition_list, contrast, comp_list, row, loadpage_input
  )
  
  expect_equal(nrow(result), 2)
  expect_equal(result[1, 1], 1)
  expect_equal(result[1, 3], -1)
  expect_equal(result[2, 2], 1)
  expect_equal(result[2, 3], -1)
  expect_true("TreatA vs Control" %in% rownames(result))
  expect_true("TreatB vs Control" %in% rownames(result))
})

test_that("build_all_against_one_contrast handles single comparison", {
  input <- list(group3 = "B")
  condition_list <- c("A", "B")
  contrast <- list(matrix = NULL, row = NULL)
  comp_list <- list(dList = NULL)
  row <- c(0, 0)
  loadpage_input <- list(BIO = "Protein", DDA_DIA = "DDA")
  
  result <- build_all_against_one_contrast(
    input, condition_list, contrast, comp_list, row, loadpage_input
  )
  
  expect_equal(nrow(result), 1)
  expect_equal(result[1, 1], 1)
  expect_equal(result[1, 2], -1)
})

# ============================================================================
# Tests for build_all_pair_contrast
# ============================================================================

test_that("build_all_pair_contrast creates all pairwise comparisons", {
  input <- list()
  condition_list <- c("A", "B", "C")
  contrast <- list(matrix = NULL, row = NULL)
  comp_list <- list(dList = NULL)
  row <- c(0, 0, 0)
  loadpage_input <- list(BIO = "Protein", DDA_DIA = "DDA")
  
  result <- build_all_pair_contrast(
    input, condition_list, contrast, comp_list, row, loadpage_input
  )
  
  expect_equal(nrow(result), 3)
  expect_true("A vs B" %in% rownames(result))
  expect_true("A vs C" %in% rownames(result))
  expect_true("B vs C" %in% rownames(result))
})

test_that("build_all_pair_contrast creates correct contrast values", {
  input <- list()
  condition_list <- c("X", "Y")
  contrast <- list(matrix = NULL, row = NULL)
  comp_list <- list(dList = NULL)
  row <- c(0, 0)
  loadpage_input <- list(BIO = "Protein", DDA_DIA = "DDA")
  
  result <- build_all_pair_contrast(
    input, condition_list, contrast, comp_list, row, loadpage_input
  )
  
  expect_equal(nrow(result), 1)
  expect_equal(result[1, 1], 1)
  expect_equal(result[1, 2], -1)
  expect_equal(rownames(result), "X vs Y")
})

test_that("build_all_pair_contrast handles four groups correctly", {
  input <- list()
  condition_list <- c("A", "B", "C", "D")
  contrast <- list(matrix = NULL, row = NULL)
  comp_list <- list(dList = NULL)
  row <- c(0, 0, 0, 0)
  loadpage_input <- list(BIO = "Protein", DDA_DIA = "DDA")
  
  result <- build_all_pair_contrast(
    input, condition_list, contrast, comp_list, row, loadpage_input
  )
  
  # Should have 6 pairwise comparisons (4 choose 2)
  expect_equal(nrow(result), 6)
})

# ============================================================================
# Tests for extract_significant_proteins
# ============================================================================

test_that("extract_significant_proteins filters PTM data correctly", {
  data_comp <- list(
    PTM.Model = data.frame(
      protein = c("P1", "P2", "P3"),
      adj.pvalue = c(0.001, 0.1, 0.03)
    ),
    PROTEIN.Model = data.frame(
      protein = c("P1", "P2"),
      adj.pvalue = c(0.02, 0.2)
    ),
    ADJUSTED.Model = data.frame(
      protein = c("P1", "P2"),
      adj.pvalue = c(0.04, 0.3)
    )
  )
  loadpage_input <- list(BIO = "PTM")
  
  result <- extract_significant_proteins(data_comp, loadpage_input, 0.05)
  expect_equal(nrow(result), 1)
})

test_that("extract_significant_proteins filters TMT data correctly", {
  data_comp <- list(
    ComparisonResult = data.frame(
      protein = c("P1", "P2", "P3", "P4"),
      adj.pvalue = c(0.001, 0.06, 0.03, 0.2)
    )
  )
  loadpage_input <- list(BIO = "Protein", DDA_DIA = "TMT")
  
  result <- extract_significant_proteins(data_comp, loadpage_input, 0.05)
  
  expect_equal(nrow(result), 2)
  expect_true(all(result$adj.pvalue < 0.05))
})

test_that("extract_significant_proteins filters standard data correctly", {
  data_comp <- list(
    ComparisonResult = data.frame(
      protein = c("P1", "P2", "P3"),
      adj.pvalue = c(0.01, 0.2, 0.04)
    )
  )
  loadpage_input <- list(BIO = "Protein", DDA_DIA = "DDA")
  
  result <- extract_significant_proteins(data_comp, loadpage_input, 0.05)
  
  expect_equal(nrow(result), 2)
  expect_equal(result$protein, c("P1", "P3"))
})

test_that("extract_significant_proteins returns empty for no significant results", {
  data_comp <- list(
    ComparisonResult = data.frame(
      protein = c("P1", "P2"),
      adj.pvalue = c(0.1, 0.2)
    )
  )
  loadpage_input <- list(BIO = "Protein", DDA_DIA = "DDA")
  
  result <- extract_significant_proteins(data_comp, loadpage_input, 0.05)
  
  expect_equal(nrow(result), 0)
})

test_that("creates response matrix when contrast$matrix is NULL", {
  input <- list(
    group3 = "Control",
    response_curve_xaxis = "Dose",
    response_curve_amount = 0
  )
  
  contrast <- list(matrix = NULL)
  result <- build_response_curve_matrix(input, contrast)
  
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 3)
  expect_equal(result$Condition, "Control")
  expect_equal(result$X_axis, "Dose")
  expect_equal(result$Amount, 0)
})

test_that("appends row to existing response matrix", {
  input <- list(
    group3 = "T5",
    response_curve_xaxis = "Time",
    response_curve_amount = 5
  )
  
  contrast <- list(
    matrix = data.frame(
      Condition = "Control",
      X_axis = "Time",
      Amount = 0,
      stringsAsFactors = FALSE
    )
  )
  
  result <- build_response_curve_matrix(input, contrast)
  
  expect_equal(nrow(result), 2)
  expect_equal(result$Condition[2], "T5")
  expect_equal(result$X_axis[2], "Time")
  expect_equal(result$Amount[2], 5)
})

test_that("removes duplicate conditions in response matrix, keep first occurrence", {
  input <- list(
    group3 = "Control",
    response_curve_xaxis = "Time",
    response_curve_amount = 1
  )
  
  contrast <- list(
    matrix = data.frame(
      Condition = c("Control", "T15"),
      X_axis = c("Time", "Time"),
      Amount = c(0, 15),
      stringsAsFactors = FALSE
    )
  )
  
  result <- build_response_curve_matrix(input, contrast)
  
  expect_equal(nrow(result), 2)
  control_row <- result[result$Condition == "Control", ]
  expect_equal(control_row$X_axis, "Time")
  expect_equal(control_row$Amount, 0)
})

test_that("response matrix handles multiple unique x-axes correctly", {
  input <- list(
    group3 = "Group_C",
    response_curve_xaxis = "pH",
    response_curve_amount = 7.5
  )
  
  contrast <- list(
    matrix = data.frame(
      Condition = c("Group_A", "Group_B"),
      X_axis = c("Temperature", "Pressure"),
      Amount = c(25, 100),
      stringsAsFactors = FALSE
    )
  )
  
  result <- build_response_curve_matrix(input, contrast)
  
  expect_equal(nrow(result), 3)
  expect_true(all(c("Group_A", "Group_B", "Group_C") %in% result$Condition))
})
