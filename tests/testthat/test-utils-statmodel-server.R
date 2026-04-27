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
  expect_equal(nrow(result$ADJUSTED.Model), 1)
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
    NAMESPACE_STATMODEL$comparisons_submit, html, fixed = TRUE))
  expect_true(grepl(
    NAMESPACE_STATMODEL$comparisons_clear, html, fixed = TRUE))

  ui_all_one <- get_contrast_panel_ui(
    CONSTANTS_STATMODEL$comparison_mode_all_vs_one, ns)
  html <- as.character(ui_all_one)
  expect_true(grepl(
    NAMESPACE_STATMODEL$comparisons_submit, html, fixed = TRUE))
  expect_true(grepl(
    NAMESPACE_STATMODEL$comparisons_all_vs_one_choice, html, fixed = TRUE))

  ui_all_pair <- get_contrast_panel_ui(
    CONSTANTS_STATMODEL$comparison_mode_all_pairwise, ns)
  html <- as.character(ui_all_pair)
  expect_true(grepl(
    NAMESPACE_STATMODEL$comparisons_submit, html, fixed = TRUE))

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


# ============================================================================
# Tests for update_matrix_from_edit
# ============================================================================

test_that("update_matrix_from_edit updates a numeric matrix correctly", {
  mat <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  info <- list(row = 1, col = 2, value = "99")
  
  result <- update_matrix_from_edit(mat, info)
  
  expect_equal(result[1, 2], 99)
  expect_true(is.numeric(result))
})

test_that("update_matrix_from_edit updates a data.frame with mixed types", {
  df <- data.frame(
    label = c("A", "B"),
    value = c(10, 20),
    stringsAsFactors = FALSE
  )
  
  # Update numeric column
  info_numeric <- list(row = 1, col = 2, value = "100.5")
  result_numeric <- update_matrix_from_edit(df, info_numeric)
  
  expect_equal(result_numeric[1, 2], 100.5)
  expect_true(is.numeric(result_numeric$value))
  
  # Update character column
  info_char <- list(row = 2, col = 1, value = "Updated")
  result_char <- update_matrix_from_edit(result_numeric, info_char)
  
  expect_equal(result_char[2, 1], "Updated")
  expect_true(is.character(result_char$label))
})

# ============================================================================
# Tests for generate_analysis_code
# ============================================================================

test_that("generate_analysis_code produces MSstatsResponse code for dose response curves", {
  mockery::stub(generate_analysis_code, "preprocessDataCode", "# preprocess\n")

  comp_mat = data.frame(
    GROUP = c("Drug_1nM", "Drug_10nM", "DMSO"),
    drug = c("Drug", "Drug", "DMSO"),
    dose_value = c(1, 10, 0),
    dose_unit = c("nM", "nM", "nM")
  )

  mock_input = list()
  mock_input[[NAMESPACE_STATMODEL$comparison_mode]] = CONSTANTS_STATMODEL$comparison_mode_response_curve
  mock_input[[NAMESPACE_STATMODEL$modeling_response_curve_increasing_trend]] = FALSE
  mock_input[[NAMESPACE_STATMODEL$modeling_response_curve_log_xaxis]] = TRUE
  mock_input[[NAMESPACE_STATMODEL$visualization_response_curve_ratio_scale]] = TRUE

  result = generate_analysis_code(list(), list(), comp_mat, mock_input)

  expect_true(grepl("library\\(MSstatsResponse\\)", result))
  expect_true(grepl("data\\.frame", result))
  expect_true(grepl("protein_level_data\\$protein", result))
  expect_true(grepl("protein_level_data\\$response", result))
  expect_true(grepl("protein_level_data\\$dose", result))
  expect_false(grepl("setup_metadata", result))
  expect_false(grepl("prepare_dose_response_fit", result))
  expect_true(grepl("doseResponseFit", result))
  expect_true(grepl("visualizeResponseProtein", result))
  # doseResponseFit always hardcodes ratio_response = FALSE regardless of checkbox
  expect_true(grepl("doseResponseFit[^)]*ratio_response = FALSE", result))
  # visualizeResponseProtein should use the checkbox value (TRUE here)
  expect_true(grepl("visualizeResponseProtein[^)]*ratio_response = TRUE", result))
  # Should NOT contain standard group comparison code
  expect_false(grepl("groupComparison\\b", result))
  expect_false(grepl("contrast.matrix", result, fixed = TRUE))
})

test_that("generate_analysis_code passes response curve parameters correctly", {
  mockery::stub(generate_analysis_code, "preprocessDataCode", "# preprocess\n")

  comp_mat = data.frame(
    GROUP = c("A", "B"), drug = c("X", "X"),
    dose_value = c(1, 2), dose_unit = c("nM", "nM")
  )

  mock_input = list()
  mock_input[[NAMESPACE_STATMODEL$comparison_mode]] = CONSTANTS_STATMODEL$comparison_mode_response_curve
  mock_input[[NAMESPACE_STATMODEL$modeling_response_curve_increasing_trend]] = TRUE
  mock_input[[NAMESPACE_STATMODEL$visualization_response_curve_ratio_scale]] = FALSE

  result = generate_analysis_code(list(), list(), comp_mat, mock_input)

  expect_true(grepl("increasing = TRUE", result))
  expect_true(grepl("transform_dose = TRUE", result))
  # doseResponseFit always hardcodes ratio_response = FALSE
  expect_true(grepl("doseResponseFit[^)]*ratio_response = FALSE", result))
  # visualizeResponseProtein uses the checkbox value (FALSE in this case)
  expect_true(grepl("visualizeResponseProtein[^)]*ratio_response = FALSE", result))
})

test_that("generate_analysis_code serializes condition names from contrast matrix", {
  mockery::stub(generate_analysis_code, "preprocessDataCode", "# preprocess\n")

  comp_mat = data.frame(
    GROUP = c("Dasatinib_001nM", "Dasatinib_010nM", "DMSO"),
    drug = c("Dasatinib", "Dasatinib", "DMSO"),
    dose_value = c(0.001, 0.01, 0), dose_unit = c("nM", "nM", "nM")
  )

  mock_input = list()
  mock_input[[NAMESPACE_STATMODEL$comparison_mode]] = CONSTANTS_STATMODEL$comparison_mode_response_curve
  mock_input[[NAMESPACE_STATMODEL$modeling_response_curve_increasing_trend]] = FALSE
  mock_input[[NAMESPACE_STATMODEL$modeling_response_curve_log_xaxis]] = TRUE
  mock_input[[NAMESPACE_STATMODEL$visualization_response_curve_ratio_scale]] = TRUE

  result = generate_analysis_code(list(), list(), comp_mat, mock_input)

  expect_true(grepl("Dasatinib_001nM", result))
  expect_true(grepl("Dasatinib_010nM", result))
  expect_true(grepl("DMSO", result))
})

test_that("generate_analysis_code produces groupComparison for standard DDA", {
  mockery::stub(generate_analysis_code, "preprocessDataCode", "# preprocess\n")

  comp_mat = matrix(c(1, -1, 0), nrow = 1)
  rownames(comp_mat) = "A vs B"
  colnames(comp_mat) = c("A", "B", "C")

  mock_input = list()
  mock_input[[NAMESPACE_STATMODEL$comparison_mode]] = CONSTANTS_STATMODEL$comparison_mode_all_pairwise

  result = generate_analysis_code(list(), list(DDA_DIA = "DDA", BIO = "Protein"), comp_mat, mock_input)

  expect_true(grepl("MSstats::groupComparison", result))
  expect_true(grepl("contrast.matrix", result, fixed = TRUE))
  expect_true(grepl("groupComparisonPlots", result))
  expect_false(grepl("doseResponseFit", result))
})

test_that("generate_analysis_code produces groupComparisonPTM for PTM", {
  mockery::stub(generate_analysis_code, "preprocessDataCode", "# preprocess\n")

  comp_mat = matrix(c(1, -1), nrow = 1)
  rownames(comp_mat) = "A vs B"
  colnames(comp_mat) = c("A", "B")

  mock_input = list()
  mock_input[[NAMESPACE_STATMODEL$comparison_mode]] = CONSTANTS_STATMODEL$comparison_mode_all_pairwise

  result = generate_analysis_code(
    list(), list(DDA_DIA = "DDA", BIO = "PTM", filetype = "maxq"), comp_mat, mock_input
  )

  expect_true(grepl("groupComparisonPTM", result))
  expect_true(grepl("groupComparisonPlotsPTM", result))
  expect_false(grepl("doseResponseFit", result))
})

test_that("generate_analysis_code produces groupComparisonTMT for TMT", {
  mockery::stub(generate_analysis_code, "preprocessDataCode", "# preprocess\n")

  comp_mat = matrix(c(1, -1), nrow = 1)
  rownames(comp_mat) = "A vs B"
  colnames(comp_mat) = c("A", "B")

  mock_input = list()
  mock_input[[NAMESPACE_STATMODEL$comparison_mode]] = CONSTANTS_STATMODEL$comparison_mode_all_pairwise
  mock_input[[NAMESPACE_STATMODEL$modeling_tmt_moderation]] = TRUE

  result = generate_analysis_code(
    list(), list(DDA_DIA = "TMT", BIO = "Protein"), comp_mat, mock_input
  )

  expect_true(grepl("groupComparisonTMT", result))
  expect_false(grepl("doseResponseFit", result))
})

# ============================================================================
# MODELING SECTION HEADER TESTS
# ============================================================================

test_that("get_modeling_section_header returns dose-response heading for response curve mode", {
  result <- MSstatsShiny:::get_modeling_section_header(
    CONSTANTS_STATMODEL$comparison_mode_response_curve
  )
  html <- as.character(result)
  expect_true(grepl("Dose-response analysis", html),
              info = "Should show dose-response heading")
  expect_true(grepl("configure the mapping", html),
              info = "Should show dose-response description")
  expect_false(grepl("Group comparison", html),
               info = "Should not show group comparison heading")
})

test_that("get_modeling_section_header returns group comparison heading for other modes", {
  for (mode in c(
    CONSTANTS_STATMODEL$comparison_mode_all_pairwise,
    CONSTANTS_STATMODEL$comparison_mode_all_vs_one,
    CONSTANTS_STATMODEL$comparison_mode_custom_pairwise,
    CONSTANTS_STATMODEL$comparison_mode_custom_nonpairwise
  )) {
    result <- MSstatsShiny:::get_modeling_section_header(mode)
    html <- as.character(result)
    expect_true(grepl("Group comparison", html),
                info = paste("Should show group comparison for mode:", mode))
    expect_false(grepl("Dose response", html),
                 info = paste("Should not show dose response for mode:", mode))
  }
})

test_that("get_modeling_section_header handles NULL mode", {
  result <- MSstatsShiny:::get_modeling_section_header(NULL)
  html <- as.character(result)
  expect_true(grepl("Group comparison", html),
              info = "NULL mode should default to group comparison")
})