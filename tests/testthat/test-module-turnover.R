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

test_that("prepare_turnover_for_dose_response drops NA H_frac", {
  ratios <- data.frame(
    Protein = c("ProtA", "ProtB", "ProtC"),
    TimeVal = c(1, 2, 4),
    H_frac  = c(NA, 0.5, NA),
    stringsAsFactors = FALSE
  )

  result <- MSstatsShiny:::prepare_turnover_for_dose_response(ratios)

  expect_equal(nrow(result), 1,
               info = "Non-NA rows preserved")
  expect_equal(result$response[1], 0.5,
               info = "Non-NA H_frac value is preserved")
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

  expect_type(result$protein, "character")
})

test_that("prepare_turnover_for_dose_response coerces TimeVal to numeric dose", {
  ratios <- data.frame(
    Protein = c("ProtA"),
    TimeVal = c("4"),  # character input
    H_frac  = c(0.5),
    stringsAsFactors = FALSE
  )

  result <- MSstatsShiny:::prepare_turnover_for_dose_response(ratios)

  expect_type(result$dose, "double")
  expect_equal(result$dose, 4)
})

test_that("prepare_turnover_for_dose_response selects H_frac when increasing = TRUE (synthesis)", {
  ratios <- data.frame(
    Protein = c("ProtA", "ProtB"),
    TimeVal = c(1, 2),
    H_frac  = c(0.3, 0.7),
    L_frac  = c(0.7, 0.3),
    stringsAsFactors = FALSE
  )

  result <- MSstatsShiny:::prepare_turnover_for_dose_response(ratios, increasing = TRUE)

  expect_equal(result$response, c(0.3, 0.7),
               info = "increasing = TRUE should map response to H_frac")
})

test_that("prepare_turnover_for_dose_response selects L_frac when increasing = FALSE (degradation)", {
  ratios <- data.frame(
    Protein = c("ProtA", "ProtB"),
    TimeVal = c(1, 2),
    H_frac  = c(0.3, 0.7),
    L_frac  = c(0.7, 0.3),
    stringsAsFactors = FALSE
  )

  result <- MSstatsShiny:::prepare_turnover_for_dose_response(ratios, increasing = FALSE)

  expect_equal(result$response, c(0.7, 0.3),
               info = "increasing = FALSE should map response to L_frac (degradation)")
})

test_that("prepare_turnover_for_dose_response defaults to H_frac (synthesis) when increasing is unset", {
  ratios <- data.frame(
    Protein = c("ProtA"),
    TimeVal = c(1),
    H_frac  = c(0.4),
    L_frac  = c(0.6),
    stringsAsFactors = FALSE
  )

  result <- MSstatsShiny:::prepare_turnover_for_dose_response(ratios)

  expect_equal(result$response, 0.4,
               info = "default behavior should remain H_frac for backward compatibility")
})

test_that("prepare_turnover_for_dose_response zero timepoint is 0 for synthesis", {
  ratios <- data.frame(
    Protein = c("ProtA", "ProtA"),
    TimeVal = c(2, 4),
    H_frac  = c(0.3, 0.6),
    L_frac  = c(0.7, 0.4),
    stringsAsFactors = FALSE
  )

  result <- MSstatsShiny:::prepare_turnover_for_dose_response(
    ratios, add_zero_timepoint = TRUE, increasing = TRUE
  )

  zero_row <- result[result$dose == 0, ]
  expect_equal(nrow(zero_row), 1,
               info = "exactly one synthetic zero-timepoint row added")
  expect_equal(zero_row$response, 0,
               info = "synthesis: zero timepoint response is 0 (no heavy incorporated yet)")
})

test_that("prepare_turnover_for_dose_response zero timepoint is 1 for degradation", {
  ratios <- data.frame(
    Protein = c("ProtA", "ProtA"),
    TimeVal = c(2, 4),
    H_frac  = c(0.3, 0.6),
    L_frac  = c(0.7, 0.4),
    stringsAsFactors = FALSE
  )

  result <- MSstatsShiny:::prepare_turnover_for_dose_response(
    ratios, add_zero_timepoint = TRUE, increasing = FALSE
  )

  zero_row <- result[result$dose == 0, ]
  expect_equal(nrow(zero_row), 1,
               info = "exactly one synthetic zero-timepoint row added")
  expect_equal(zero_row$response, 1,
               info = "degradation: zero timepoint response is 1 (pre-existing light pool intact)")
})

test_that("prepare_turnover_for_dose_response drops NA on the selected fraction column", {
  ratios <- data.frame(
    Protein = c("ProtA", "ProtB", "ProtC"),
    TimeVal = c(1, 2, 4),
    H_frac  = c(0.3, NA,  0.8),
    L_frac  = c(NA,  0.5, 0.2),
    stringsAsFactors = FALSE
  )

  syn <- MSstatsShiny:::prepare_turnover_for_dose_response(ratios, increasing = TRUE)
  deg <- MSstatsShiny:::prepare_turnover_for_dose_response(ratios, increasing = FALSE)

  expect_equal(syn$response, c(0.3, 0.8),
               info = "synthesis: drops the row with NA H_frac")
  expect_equal(deg$response, c(0.5, 0.2),
               info = "degradation: drops the row with NA L_frac")
})

# ============================================================================
# Tests for per-peptide weight passthrough (calculatePeptideWeights -> fit)
# ============================================================================

test_that("prepare_turnover_for_dose_response carries the weight column when present", {
  ratios <- data.frame(
    Protein = c("ProtA", "ProtA"),
    TimeVal = c(1, 2),
    H_frac  = c(0.3, 0.6),
    L_frac  = c(0.7, 0.4),
    weight  = c(0.5, 0.9),
    stringsAsFactors = FALSE
  )

  result <- MSstatsShiny:::prepare_turnover_for_dose_response(ratios)

  expect_true("weight" %in% colnames(result),
              info = "weight column should be preserved for the fit's weights argument")
  expect_equal(result$weight, c(0.5, 0.9),
               info = "weights should stay row-aligned with the prepared data")
})

test_that("prepare_turnover_for_dose_response omits weight column when absent", {
  ratios <- data.frame(
    Protein = c("ProtA", "ProtA"),
    TimeVal = c(1, 2),
    H_frac  = c(0.3, 0.6),
    stringsAsFactors = FALSE
  )

  result <- MSstatsShiny:::prepare_turnover_for_dose_response(ratios)

  expect_false("weight" %in% colnames(result),
               info = "no weight column should appear when the input has none")
})

test_that("prepare_turnover_for_dose_response assigns weight 1 to synthetic zero rows", {
  ratios <- data.frame(
    Protein = c("ProtA", "ProtA"),
    TimeVal = c(2, 4),
    H_frac  = c(0.3, 0.6),
    L_frac  = c(0.7, 0.4),
    weight  = c(0.5, 0.9),
    stringsAsFactors = FALSE
  )

  result <- MSstatsShiny:::prepare_turnover_for_dose_response(
    ratios, add_zero_timepoint = TRUE, increasing = TRUE
  )

  expect_false(any(is.na(result$weight)),
               info = "synthetic zero rows must not leave NA weights that misalign the vector")
  expect_equal(result$weight[result$dose == 0], 1,
               info = "synthetic anchor points are fully trusted (weight 1)")
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
  expect_true(grepl("calculateTurnoverRatios", result, fixed = TRUE),
              info = "Turnover code should recompute turnover ratios")
})

# ============================================================================
# Tests for build_turnover_analysis_code (weighted reproducible script)
# ============================================================================

test_that("build_turnover_analysis_code includes weights when the checkbox is enabled", {
  comp_mat <- data.frame(GROUP = c("T0h", "T4h"), TimeVal = c(0, 4),
                         stringsAsFactors = FALSE)
  qc_input <- list(assign_feature_weights = TRUE)
  qc_input[[paste0("tracer_", make.names("T0h"))]] <- 1.0
  qc_input[[paste0("tracer_", make.names("T4h"))]] <- 0.9

  code <- MSstatsShiny:::build_turnover_analysis_code(qc_input, comp_mat, increasing = TRUE)
  has <- function(p) grepl(p, code, fixed = TRUE)

  expect_true(has("calculatePeptideWeights(turnover_ratios)"),
              info = "weighted script must add the calculatePeptideWeights step")
  expect_true(has("weights = prepared_data$weight"),
              info = "weighted script must pass weights to doseResponseFit / visualizeResponseProtein")
  expect_true(has("show_weights = TRUE"),
              info = "weighted script should scale plot points by weight")
  expect_true(has("\"weight\")"),
              info = "weighted script must retain the weight column in prepared_data")
  expect_true(has("\"T4h\" = 0.9"),
              info = "tracer constants must be serialized from qc_input, keyed by condition")
})

test_that("build_turnover_analysis_code omits weighting when the checkbox is disabled", {
  comp_mat <- data.frame(GROUP = c("T0h", "T4h"), TimeVal = c(0, 4),
                         stringsAsFactors = FALSE)
  qc_input <- list(assign_feature_weights = FALSE)

  code <- MSstatsShiny:::build_turnover_analysis_code(qc_input, comp_mat, increasing = FALSE)

  expect_false(grepl("calculatePeptideWeights", code, fixed = TRUE),
               info = "unweighted script must not compute peptide weights")
  expect_false(grepl("weights = prepared_data", code, fixed = TRUE),
               info = "unweighted script must not pass a weights argument")
  expect_true(grepl("frac_col = \"L_frac\"", code, fixed = TRUE),
              info = "increasing = FALSE selects the degradation (L_frac) response")
})

test_that("build_turnover_analysis_code emits syntactically valid R", {
  comp_mat <- data.frame(GROUP = c("T0h", "T4h", "T8h"), TimeVal = c(0, 4, 8),
                         stringsAsFactors = FALSE)
  for (flag in c(TRUE, FALSE)) {
    code <- MSstatsShiny:::build_turnover_analysis_code(
      list(assign_feature_weights = flag), comp_mat, increasing = TRUE
    )
    expect_silent(parse(text = code))
  }
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