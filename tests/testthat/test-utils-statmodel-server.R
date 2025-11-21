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
  input <- list()
  input[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice1]] = "A"
  input[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice2]] = "B"
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
  input <- list()
  input[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice1]] = "C"
  input[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice2]] = "A"
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
  input <- list()
  input[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice1]] = "A"
  input[[NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice2]] = "A"
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
  input <- list()
  input[[paste0(NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights, 1)]] = 0.5
  input[[paste0(NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights, 2)]] = 0.5
  input[[paste0(NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights, 3)]] = -1
  input[[NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_name]] = "AB vs C"
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
  input <- list()
  input[[paste0(NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights, 1)]] = 1
  input[[paste0(NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights, 2)]] = 1
  input[[paste0(NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights, 3)]] = 1
  input[[NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_name]] = "invalid"
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
  input <- list()
  input[[NAMESPACE_STATMODEL$comparisons_all_vs_one_choice]] = "Control"
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
  input <- list()
  input[[NAMESPACE_STATMODEL$comparisons_all_vs_one_choice]] = "B"
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


test_that("get_contrast_panel_ui returns correct UI for each mode", {
  ns <- function(id) paste0("statmodel-", id)
  ui_custom <- get_contrast_panel_ui(
    CONSTANTS_STATMODEL$comparison_mode_custom_pairwise, ns)
  html <- as.character(ui_custom)
  expect_true(grepl(
    NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice1, html, fixed = TRUE))
  expect_true(grepl(
    NAMESPACE_STATMODEL$comparisons_custom_pairwise_choice2, html, fixed = TRUE))
  expect_true(grepl(
    NAMESPACE_STATMODEL$comparisons_custom_pairwise_submit, html, fixed = TRUE))
  expect_true(grepl(
    NAMESPACE_STATMODEL$comparisons_custom_pairwise_clear, html, fixed = TRUE))

  ui_all_one <- get_contrast_panel_ui(
    CONSTANTS_STATMODEL$comparison_mode_all_vs_one, ns)
  html <- as.character(ui_all_one)
  expect_true(grepl(
    NAMESPACE_STATMODEL$comparisons_all_vs_one_submit, html, fixed = TRUE))
  expect_true(grepl(
    NAMESPACE_STATMODEL$comparisons_all_vs_one_choice, html, fixed = TRUE))

  ui_all_pair <- get_contrast_panel_ui(
    CONSTANTS_STATMODEL$comparison_mode_all_pairwise, ns)
  html <- as.character(ui_all_pair)
  expect_true(grepl(
    NAMESPACE_STATMODEL$comparisons_all_pairwise_submit, html, fixed = TRUE))

  ui_custom_np <- get_contrast_panel_ui(
    CONSTANTS_STATMODEL$comparison_mode_custom_nonpairwise, ns)
  html <- as.character(ui_custom_np)
  expect_true(grepl(
    NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_name, html, fixed = TRUE))
  expect_true(grepl(
    NAMESPACE_STATMODEL$comparisons_custom_nonpairwise_weights, html, fixed = TRUE))
  
  expect_null(get_contrast_panel_ui(NULL, ns))
  expect_null(get_contrast_panel_ui(character(0), ns))
  expect_null(get_contrast_panel_ui("invalid", ns))
})