# ============================================================================
# Tests for protein turnover functionality
# ============================================================================

# ============================================================================
# Tests for TEMPLATES and TEMPLATE_LABELS constants
# ============================================================================

test_that("TEMPLATES includes protein_turnover entry", {
  expect_true("protein_turnover" %in% names(TEMPLATES),
              info = "TEMPLATES should have a protein_turnover key")
  expect_equal(TEMPLATES$protein_turnover, "protein_turnover")
})

test_that("TEMPLATE_LABELS includes protein_turnover entry with correct label", {
  expect_true("protein_turnover" %in% names(TEMPLATE_LABELS),
              info = "TEMPLATE_LABELS should have a protein_turnover key")
  expect_equal(TEMPLATE_LABELS$protein_turnover, "Protein Turnover")
})

# ============================================================================
# Tests for prepare_turnover_for_dose_response
# ============================================================================

test_that("prepare_turnover_for_dose_response returns correct columns", {
  ratios <- data.frame(
    Protein  = c("ProtA", "ProtB"),
    TimeVal  = c(1, 2),
    H_frac   = c(0.3, 0.7),
    stringsAsFactors = FALSE
  )

  result <- MSstatsShiny:::prepare_turnover_for_dose_response(ratios)

  expect_true(all(c("protein", "drug", "dose", "response") %in% colnames(result)),
              info = "Result should contain required columns")
  expect_equal(ncol(result), 4,
               info = "Result should have exactly 4 columns when BaseSequence is absent")
})

test_that("prepare_turnover_for_dose_response maps columns correctly", {
  ratios <- data.frame(
    Protein  = c("ProtA", "ProtB"),
    TimeVal  = c(4, 8),
    H_frac   = c(0.25, 0.75),
    stringsAsFactors = FALSE
  )

  result <- MSstatsShiny:::prepare_turnover_for_dose_response(ratios)

  expect_equal(result$protein, c("ProtA", "ProtB"))
  expect_equal(result$drug, c("time", "time"))
  expect_equal(result$dose, c(4, 8))
  expect_equal(result$response, c(0.25, 0.75))
})

test_that("prepare_turnover_for_dose_response imputes NA H_frac to 0", {
  ratios <- data.frame(
    Protein = c("ProtA", "ProtB", "ProtC"),
    TimeVal = c(1, 2, 4),
    H_frac  = c(NA, 0.5, NA),
    stringsAsFactors = FALSE
  )

  result <- MSstatsShiny:::prepare_turnover_for_dose_response(ratios)

  expect_equal(result$response[1], 0,
               info = "NA H_frac should be imputed to 0")
  expect_equal(result$response[2], 0.5,
               info = "Non-NA H_frac should be preserved")
  expect_equal(result$response[3], 0,
               info = "NA H_frac should be imputed to 0")
  expect_false(any(is.na(result$response)),
               info = "Result response column should contain no NAs")
})

test_that("prepare_turnover_for_dose_response includes BaseSequence when present", {
  ratios <- data.frame(
    Protein      = c("ProtA", "ProtB"),
    TimeVal      = c(1, 2),
    H_frac       = c(0.3, 0.6),
    BaseSequence = c("PEPTIDEK", "SEQUENCER"),
    stringsAsFactors = FALSE
  )

  result <- MSstatsShiny:::prepare_turnover_for_dose_response(ratios)

  expect_true("BaseSequence" %in% colnames(result),
              info = "BaseSequence should be included when present in input")
  expect_equal(result$BaseSequence, c("PEPTIDEK", "SEQUENCER"))
  expect_equal(ncol(result), 5,
               info = "Result should have 5 columns when BaseSequence is present")
})

test_that("prepare_turnover_for_dose_response excludes BaseSequence when absent", {
  ratios <- data.frame(
    Protein = c("ProtA"),
    TimeVal = c(1),
    H_frac  = c(0.4),
    stringsAsFactors = FALSE
  )

  result <- MSstatsShiny:::prepare_turnover_for_dose_response(ratios)

  expect_false("BaseSequence" %in% colnames(result),
               info = "BaseSequence should not appear when absent from input")
})

test_that("prepare_turnover_for_dose_response coerces Protein to character", {
  ratios <- data.frame(
    Protein = factor(c("ProtA", "ProtB")),
    TimeVal = c(1, 2),
    H_frac  = c(0.3, 0.6),
    stringsAsFactors = FALSE
  )

  result <- MSstatsShiny:::prepare_turnover_for_dose_response(ratios)

  expect_type(result$protein, "character",
              info = "protein column should be character type")
})

test_that("prepare_turnover_for_dose_response coerces TimeVal to numeric dose", {
  ratios <- data.frame(
    Protein = c("ProtA"),
    TimeVal = c("4"),  # character input
    H_frac  = c(0.5),
    stringsAsFactors = FALSE
  )

  result <- MSstatsShiny:::prepare_turnover_for_dose_response(ratios)

  expect_type(result$dose, "double",
              info = "dose column should be numeric type")
  expect_equal(result$dose, 4)
})

# ============================================================================
# Tests for get_modeling_section_header with protein_turnover template
# ============================================================================

test_that("get_modeling_section_header returns turnover heading for protein_turnover template", {
  result <- MSstatsShiny:::get_modeling_section_header(
    CONSTANTS_STATMODEL$comparison_mode_response_curve,
    template = TEMPLATES$protein_turnover
  )
  html <- as.character(result)

  expect_true(grepl("Turnover analysis", html),
              info = "Should show turnover analysis heading for protein_turnover template")
  expect_true(grepl("time points", html),
              info = "Should mention time points in description for turnover template")
  expect_false(grepl("Dose-response", html),
               info = "Should not show dose-response heading for protein_turnover template")
  expect_false(grepl("Group comparison", html),
               info = "Should not show group comparison for response curve mode")
})

test_that("get_modeling_section_header returns dose-response heading for chemoproteomics template", {
  result <- MSstatsShiny:::get_modeling_section_header(
    CONSTANTS_STATMODEL$comparison_mode_response_curve,
    template = TEMPLATES$chemoproteomics
  )
  html <- as.character(result)

  expect_true(grepl("Dose-response analysis", html),
              info = "Should show dose-response heading for chemoproteomics template")
  expect_false(grepl("Turnover analysis", html),
               info = "Should not show turnover heading for chemoproteomics template")
})

test_that("get_modeling_section_header returns dose-response heading for default template", {
  result <- MSstatsShiny:::get_modeling_section_header(
    CONSTANTS_STATMODEL$comparison_mode_response_curve,
    template = TEMPLATES$default
  )
  html <- as.character(result)

  expect_true(grepl("Dose-response analysis", html),
              info = "Should show dose-response heading for default template")
  expect_false(grepl("Turnover analysis", html),
               info = "Should not show turnover heading for default template")
})

# ============================================================================
# Tests for create_response_curve_options with protein_turnover template
# ============================================================================

test_that("create_response_curve_options hides ratio scale checkbox for protein_turnover", {
  ns <- NS("test_module")
  result <- MSstatsShiny:::create_response_curve_options(ns, template = TEMPLATES$protein_turnover)
  ui_html <- htmltools::renderTags(result)$html

  expect_false(grepl(NAMESPACE_STATMODEL$visualization_response_curve_ratio_scale, ui_html),
               info = "Ratio scale checkbox should NOT be present for protein_turnover template")
})

test_that("create_response_curve_options shows ratio scale checkbox for default template", {
  ns <- NS("test_module")
  result <- MSstatsShiny:::create_response_curve_options(ns, template = TEMPLATES$default)
  ui_html <- htmltools::renderTags(result)$html

  expect_true(grepl(NAMESPACE_STATMODEL$visualization_response_curve_ratio_scale, ui_html),
              info = "Ratio scale checkbox should be present for default template")
})

test_that("create_response_curve_options shows ratio scale checkbox for chemoproteomics template", {
  ns <- NS("test_module")
  result <- MSstatsShiny:::create_response_curve_options(ns, template = TEMPLATES$chemoproteomics)
  ui_html <- htmltools::renderTags(result)$html

  expect_true(grepl(NAMESPACE_STATMODEL$visualization_response_curve_ratio_scale, ui_html),
              info = "Ratio scale checkbox should be present for chemoproteomics template")
})

test_that("create_response_curve_options shows ratio scale checkbox when template is NULL", {
  ns <- NS("test_module")
  result <- MSstatsShiny:::create_response_curve_options(ns, template = NULL)
  ui_html <- htmltools::renderTags(result)$html

  expect_true(grepl(NAMESPACE_STATMODEL$visualization_response_curve_ratio_scale, ui_html),
              info = "Ratio scale checkbox should be present when template is NULL")
})

test_that("create_response_curve_options always shows protein selector", {
  ns <- NS("test_module")

  for (tmpl in list(TEMPLATES$protein_turnover, TEMPLATES$default, TEMPLATES$chemoproteomics, NULL)) {
    result <- MSstatsShiny:::create_response_curve_options(ns, template = tmpl)
    ui_html <- htmltools::renderTags(result)$html
    expect_true(grepl(NAMESPACE_STATMODEL$visualization_which_protein, ui_html),
                info = paste("Protein selector should always be present; template =", tmpl))
  }
})

# ============================================================================
# Tests for create_response_curve_increasing_trend_checkbox with protein_turnover
# ============================================================================

test_that("create_response_curve_increasing_trend_checkbox uses turnover label for protein_turnover", {
  ns <- NS("test_module")
  result <- MSstatsShiny:::create_response_curve_increasing_trend_checkbox(
    ns, value = TRUE, template = TEMPLATES$protein_turnover
  )
  ui_html <- htmltools::renderTags(result)$html

  expect_true(grepl("heavy-isotope", ui_html),
              info = "Should use heavy-isotope label text for protein_turnover template")
  expect_false(grepl("dose-response", ui_html),
               info = "Should not use dose-response label text for protein_turnover template")
})

test_that("create_response_curve_increasing_trend_checkbox uses dose-response label for default", {
  ns <- NS("test_module")
  result <- MSstatsShiny:::create_response_curve_increasing_trend_checkbox(
    ns, value = FALSE, template = TEMPLATES$default
  )
  ui_html <- htmltools::renderTags(result)$html

  expect_true(grepl("dose-response", ui_html),
              info = "Should use dose-response label text for default template")
  expect_false(grepl("heavy-isotope", ui_html),
               info = "Should not use heavy-isotope label text for default template")
})

test_that("create_response_curve_increasing_trend_checkbox uses turnover tooltip for protein_turnover", {
  ns <- NS("test_module")
  result <- MSstatsShiny:::create_response_curve_increasing_trend_checkbox(
    ns, value = TRUE, template = TEMPLATES$protein_turnover
  )
  ui_html <- htmltools::renderTags(result)$html

  expect_true(grepl("pulse-chase", ui_html),
              info = "Turnover tooltip should mention pulse-chase experiments")
})

# ============================================================================
# Tests for generate_analysis_code with protein_turnover template
# ============================================================================

test_that("generate_analysis_code produces turnover-specific code for protein_turnover template", {
  mockery::stub(generate_analysis_code, "preprocessDataCode", "# preprocess\n")

  comp_mat <- data.frame(
    GROUP   = c("T0h", "T4h", "T8h"),
    TimeVal = c(0, 4, 8),
    stringsAsFactors = FALSE
  )

  mock_input <- list()
  mock_input[[NAMESPACE_STATMODEL$comparison_mode]] <- CONSTANTS_STATMODEL$comparison_mode_response_curve
  mock_input[[NAMESPACE_STATMODEL$modeling_response_curve_increasing_trend]] <- TRUE
  mock_input[[NAMESPACE_STATMODEL$visualization_response_curve_ratio_scale]] <- FALSE

  result <- generate_analysis_code(
    list(), list(), comp_mat, mock_input,
    app_template = TEMPLATES$protein_turnover
  )

  expect_true(grepl('drug_name = "time"', result),
              info = "Turnover code should use drug_name = \"time\"")
  expect_true(grepl("precalculated_ratios = TRUE", result),
              info = "Turnover code should set precalculated_ratios = TRUE")
  expect_true(grepl('color_by = "BaseSequence"', result),
              info = "Turnover code should set color_by = \"BaseSequence\"")
  expect_true(grepl("target_response = 0.5", result),
              info = "Turnover code should set target_response = 0.5")
  expect_true(grepl("visualizeResponseProtein", result),
              info = "Turnover code should call visualizeResponseProtein")
})

test_that("generate_analysis_code does not set precalculated_ratios for chemoproteomics template", {
  mockery::stub(generate_analysis_code, "preprocessDataCode", "# preprocess\n")

  comp_mat <- data.frame(
    GROUP      = c("Drug_1nM", "DMSO"),
    drug       = c("Drug", "DMSO"),
    dose_value = c(1, 0),
    dose_unit  = c("nM", "nM"),
    stringsAsFactors = FALSE
  )

  mock_input <- list()
  mock_input[[NAMESPACE_STATMODEL$comparison_mode]] <- CONSTANTS_STATMODEL$comparison_mode_response_curve
  mock_input[[NAMESPACE_STATMODEL$modeling_response_curve_increasing_trend]] <- FALSE
  mock_input[[NAMESPACE_STATMODEL$visualization_response_curve_ratio_scale]] <- TRUE

  result <- generate_analysis_code(
    list(), list(), comp_mat, mock_input,
    app_template = TEMPLATES$chemoproteomics
  )

  expect_false(grepl("precalculated_ratios", result),
               info = "Chemoproteomics code should not include precalculated_ratios")
  expect_false(grepl('drug_name = "time"', result),
               info = "Chemoproteomics code should not use drug_name = \"time\"")
  expect_true(grepl("visualizeResponseProtein", result),
              info = "Chemoproteomics code should still call visualizeResponseProtein")
})

test_that("generate_analysis_code uses placeholder drug_name for non-turnover template", {
  mockery::stub(generate_analysis_code, "preprocessDataCode", "# preprocess\n")

  comp_mat <- data.frame(
    GROUP      = c("A", "B"),
    drug       = c("X", "X"),
    dose_value = c(1, 2),
    dose_unit  = c("nM", "nM"),
    stringsAsFactors = FALSE
  )

  mock_input <- list()
  mock_input[[NAMESPACE_STATMODEL$comparison_mode]] <- CONSTANTS_STATMODEL$comparison_mode_response_curve
  mock_input[[NAMESPACE_STATMODEL$modeling_response_curve_increasing_trend]] <- FALSE
  mock_input[[NAMESPACE_STATMODEL$visualization_response_curve_ratio_scale]] <- FALSE

  result <- generate_analysis_code(
    list(), list(), comp_mat, mock_input,
    app_template = TEMPLATES$default
  )

  expect_true(grepl("Enter drug name here", result),
              info = "Non-turnover code should use a placeholder drug name")
})