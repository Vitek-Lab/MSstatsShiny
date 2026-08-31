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

# A tracer_constants snapshot in the shape register_qc_turnover records it.
tracer_snapshot <- function(values, source = CONSTANTS_QC$tracer_source_upload,
                            file = "tracer.csv") {
  list(values = values, source = source, file = file)
}

test_that("build_turnover_analysis_code includes weights when the checkbox is enabled", {
  comp_mat <- data.frame(GROUP = c("T0h", "T4h"), TimeVal = c(0, 4),
                         stringsAsFactors = FALSE)
  qc_input <- list(assign_feature_weights = TRUE)

  code <- MSstatsShiny:::build_turnover_analysis_code(
    qc_input, comp_mat, increasing = TRUE,
    tracer_constants = tracer_snapshot(c("T0h" = 1.0, "T4h" = 0.9)))
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
              info = "tracer constants must be serialized from the resolved snapshot, keyed by condition")
})

test_that("build_turnover_analysis_code omits weighting when the checkbox is disabled", {
  comp_mat <- data.frame(GROUP = c("T0h", "T4h"), TimeVal = c(0, 4),
                         stringsAsFactors = FALSE)
  qc_input <- list(assign_feature_weights = FALSE)

  code <- MSstatsShiny:::build_turnover_analysis_code(
    qc_input, comp_mat, increasing = FALSE,
    tracer_constants = tracer_snapshot(c("T0h" = 1, "T4h" = 1),
                                       source = CONSTANTS_QC$tracer_source_none,
                                       file = NULL))

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
      list(assign_feature_weights = flag), comp_mat, increasing = TRUE,
      tracer_constants = tracer_snapshot(c("T0h" = 1, "T4h" = 0.9, "T8h" = 0.8))
    )
    expect_silent(parse(text = code))
  }
})

# ---------------------------------------------------------------------------
# The emitted vector must round-trip to the vector the app divided by.
# Evaluating the emitted source rather than grepl()-ing a substring out of it
# catches names, values, order and precision at once.
# ---------------------------------------------------------------------------

# Pulls the `tracer_constants = c(...)` assignment out of the generated script
# and evaluates it, so the assertion is against what R would bind.
eval_emitted_tracer_constants <- function(code) {
  env <- new.env(parent = baseenv())
  found <- FALSE
  for (expr in as.list(parse(text = code))) {
    is_assignment <- is.call(expr) && length(expr) == 3L &&
      as.character(expr[[1L]])[1L] %in% c("=", "<-") &&
      is.name(expr[[2L]]) && identical(as.character(expr[[2L]]), "tracer_constants")
    if (is_assignment) {
      eval(expr, env)
      found <- TRUE
    }
  }
  if (!found) stop("the generated script contains no tracer_constants assignment")
  get("tracer_constants", envir = env)
}

test_that("the emitted tracer_constants vector is identical to the resolved one", {
  conditions <- c("0h", "6h", "24h")
  # 1/3 is the point of the test: paste0() renders it to 15 significant digits,
  # which parses back to a DIFFERENT double.
  uploaded <- stats::setNames(c(1/3, 0.9, 1), conditions)
  resolved <- MSstatsShiny:::qc_resolve_tracer_constants(conditions, uploaded)

  code <- MSstatsShiny:::build_turnover_analysis_code(
    list(assign_feature_weights = FALSE),
    data.frame(GROUP = conditions, TimeVal = c(0, 6, 24), stringsAsFactors = FALSE),
    increasing = TRUE, tracer_constants = tracer_snapshot(resolved))

  expect_identical(eval_emitted_tracer_constants(code), resolved)
})

test_that("the emitted vector follows the contrast matrix order, not the file order", {
  conditions <- c("0h", "6h", "24h")
  resolved <- stats::setNames(c(1, 0.9, 0.8), conditions)
  # Same condition set, reversed: calculateTurnoverRatios keys by name, but an
  # order-blind assertion would not notice a wrong-value-per-name pairing.
  comp_mat <- data.frame(GROUP = rev(conditions), TimeVal = c(24, 6, 0),
                         stringsAsFactors = FALSE)

  emitted <- eval_emitted_tracer_constants(
    MSstatsShiny:::build_turnover_analysis_code(
      list(assign_feature_weights = FALSE), comp_mat, increasing = TRUE,
      tracer_constants = tracer_snapshot(resolved)))

  expect_identical(names(emitted), rev(conditions))
  expect_identical(unname(emitted[conditions]), unname(resolved))
})

test_that("build_turnover_analysis_code escapes quotes in condition names", {
  # Keying by the raw condition string exposes this; make.names() previously
  # sanitized the names away.
  conditions <- c('0h "baseline"', "6h\\late")
  resolved <- stats::setNames(c(1, 0.9), conditions)
  comp_mat <- data.frame(GROUP = conditions, TimeVal = c(0, 6),
                         stringsAsFactors = FALSE)

  code <- MSstatsShiny:::build_turnover_analysis_code(
    list(assign_feature_weights = FALSE), comp_mat, increasing = TRUE,
    tracer_constants = tracer_snapshot(resolved))

  expect_silent(parse(text = code))
  expect_identical(eval_emitted_tracer_constants(code), resolved)
})

test_that("build_turnover_analysis_code refuses to emit constants it cannot vouch for", {
  comp_mat <- data.frame(GROUP = c("0h", "6h"), TimeVal = c(0, 6),
                         stringsAsFactors = FALSE)
  build <- function(values) MSstatsShiny:::build_turnover_analysis_code(
    list(assign_feature_weights = FALSE), comp_mat, increasing = TRUE,
    tracer_constants = tracer_snapshot(values))

  # An uncovered condition would emit NA and fit nothing, silently.
  expect_error(build(c("0h" = 1)), "do not cover condition\\(s\\): 6h")
  expect_error(build(NULL), "no tracer constants are recorded")
  expect_error(build(c(1, 1)), "no tracer constants are recorded")
  expect_error(build(c("0h" = 1, "6h" = NA_real_)), "is not a finite number")
  # Inf passes an anyNA check and emits a literal `Inf`, where H_frac / Inf is 0
  # and the script reports total turnover for every peptide.
  expect_error(build(c("0h" = 1, "6h" = Inf)), "is not a finite number")
  expect_error(build(c("0h" = 1, "6h" = -Inf)), "is not a finite number")
  expect_error(build(c("0h" = 1, "6h" = NaN)), "is not a finite number")
  # Trimming can collapse two distinct keys onto one, and match() would then
  # give BOTH conditions the first value.
  expect_error(
    MSstatsShiny:::build_turnover_analysis_code(
      list(assign_feature_weights = FALSE),
      data.frame(GROUP = c("0h", "0h "), TimeVal = c(0, 0), stringsAsFactors = FALSE),
      increasing = TRUE,
      tracer_constants = tracer_snapshot(stats::setNames(c(0.5, 0.9), c("0h", "0h ")))),
    "ambiguous for condition")
  # ...but a key repeated with the SAME value cannot mispair, and the all-1s
  # default legitimately produces one.
  expect_silent(
    MSstatsShiny:::build_turnover_analysis_code(
      list(assign_feature_weights = FALSE),
      data.frame(GROUP = c("0h", "0h"), TimeVal = c(0, 0), stringsAsFactors = FALSE),
      increasing = TRUE,
      tracer_constants = tracer_snapshot(stats::setNames(c(1, 1), c("0h", "0h")))))
  # No default: forgetting the argument must fail, not fall back to all-1s.
  expect_error(
    MSstatsShiny:::build_turnover_analysis_code(
      list(assign_feature_weights = FALSE), comp_mat, increasing = TRUE),
    "tracer_constants")
})

test_that("build_turnover_analysis_code refuses a blank condition name", {
  # `c("" = 1)` is a PARSE error, so a blank condition name produces a script
  # that will not even load. Reachable without an upload: a blank Condition cell
  # takes the all-1s default path, which runs none of the upload name checks.
  for (blank in list("", "   ", NA_character_)) {
    comp_mat <- data.frame(GROUP = c(blank, "6h"), TimeVal = c(0, 6),
                           stringsAsFactors = FALSE)
    expect_error(
      MSstatsShiny:::build_turnover_analysis_code(
        list(assign_feature_weights = FALSE), comp_mat, increasing = TRUE,
        tracer_constants = tracer_snapshot(
          stats::setNames(c(1, 1), c(blank, "6h")))),
      "blank name",
      info = paste("must refuse condition name:", deparse(blank)))
  }
})

test_that("a blank condition name cannot reach the script via the all-1s default", {
  # The same hole one hop up: generate_analysis_code builds the default vector
  # from comp_mat$GROUP, so the guard has to be the thing that stops it.
  mockery::stub(generate_analysis_code, "preprocessDataCode", "# preprocess\n")
  mock_input <- list()
  mock_input[[NAMESPACE_STATMODEL$comparison_mode]] <- CONSTANTS_STATMODEL$comparison_mode_response_curve
  mock_input[[NAMESPACE_STATMODEL$modeling_response_curve_increasing_trend]] <- TRUE

  expect_error(
    generate_analysis_code(
      list(), list(), data.frame(GROUP = c("", "6h"), TimeVal = c(0, 6),
                                 stringsAsFactors = FALSE),
      mock_input, app_template = TEMPLATES$protein_turnover),
    "blank name")
})

test_that("build_turnover_analysis_code matches conditions ignoring surrounding whitespace", {
  # An Excel-sourced "0h " is not editable in the metadata table, so a strict
  # match would be an unfixable dead end.
  comp_mat <- data.frame(GROUP = c("0h ", "6h"), TimeVal = c(0, 6),
                         stringsAsFactors = FALSE)
  code <- MSstatsShiny:::build_turnover_analysis_code(
    list(assign_feature_weights = FALSE), comp_mat, increasing = TRUE,
    tracer_constants = tracer_snapshot(c("0h" = 0.5, "6h" = 0.9)))

  expect_identical(unname(eval_emitted_tracer_constants(code)), c(0.5, 0.9))
})

test_that("exactly one tracer provenance comment is stamped into the script", {
  comp_mat <- data.frame(GROUP = c("0h", "6h"), TimeVal = c(0, 6),
                         stringsAsFactors = FALSE)
  # Each marker is the phrase that DISTINGUISHES its state, deliberately shorter
  # than the full comment so it can catch an overlap between them.
  markers <- c(upload  = "uploaded on the data-processing page",
               none    = "no file was supplied on the data-processing page",
               not_run = "the data-processing page was not run in this session")

  cases <- list(
    upload  = tracer_snapshot(c("0h" = 1, "6h" = 0.9),
                              source = CONSTANTS_QC$tracer_source_upload,
                              file = "constants.csv"),
    none    = tracer_snapshot(c("0h" = 1, "6h" = 1),
                              source = CONSTANTS_QC$tracer_source_none, file = NULL),
    not_run = tracer_snapshot(c("0h" = 1, "6h" = 1),
                              source = CONSTANTS_QC$tracer_source_not_run, file = NULL)
  )

  for (state in names(cases)) {
    code <- MSstatsShiny:::build_turnover_analysis_code(
      list(assign_feature_weights = FALSE), comp_mat, increasing = TRUE,
      tracer_constants = cases[[state]])
    hits <- vapply(markers, function(m) grepl(m, code, fixed = TRUE), logical(1))
    expect_identical(names(markers)[hits], state,
                     info = paste("provenance comments must be mutually exclusive:", state))
  }

  # The markers must not be substrings of one another's comments: an earlier
  # wording had "uploaded on the data-processing page" appear inside the
  # NO-upload comment, so a grep for the upload state matched both.
  comments <- vapply(cases, MSstatsShiny:::build_tracer_provenance_comment,
                     character(1))
  for (owner in names(markers)) {
    for (other in setdiff(names(markers), owner)) {
      expect_false(grepl(markers[[owner]], comments[[other]], fixed = TRUE),
                   info = paste0("the ", owner, " marker must not appear in the ",
                                 other, " comment"))
    }
  }

  # The file name is quoted so it survives into the comment verbatim.
  expect_true(grepl('from "constants.csv"',
                    MSstatsShiny:::build_turnover_analysis_code(
                      list(assign_feature_weights = FALSE), comp_mat, increasing = TRUE,
                      tracer_constants = cases$upload), fixed = TRUE))
})

test_that("a newline in the uploaded file name cannot break the provenance comment", {
  comp_mat <- data.frame(GROUP = "0h", TimeVal = 0, stringsAsFactors = FALSE)
  code <- MSstatsShiny:::build_turnover_analysis_code(
    list(assign_feature_weights = FALSE), comp_mat, increasing = TRUE,
    tracer_constants = tracer_snapshot(c("0h" = 1), file = "bad\nname.csv"))

  expect_silent(parse(text = code))
})

test_that("format_r_double emits the shortest form that reads back identically", {
  for (value in c(1, 0.9, 0.01, 0.5, 1/3, 2/7, 0.123, 0.4567)) {
    text <- MSstatsShiny:::format_r_double(value)
    expect_identical(as.numeric(text), value,
                     info = paste("must round-trip:", value))
  }
  # Readability is not sacrificed for the common case.
  expect_identical(MSstatsShiny:::format_r_double(0.9), "0.9")
  expect_identical(MSstatsShiny:::format_r_double(1), "1")
})

# ---------------------------------------------------------------------------
# Per-hop forwarding: the round-trip tests above prove the generator is correct
# given the right input; these prove the right input reaches it.
# ---------------------------------------------------------------------------

test_that("generate_analysis_code forwards the tracer snapshot into the turnover script", {
  mockery::stub(generate_analysis_code, "preprocessDataCode", "# preprocess\n")

  comp_mat <- data.frame(GROUP = c("0h", "6h"), TimeVal = c(0, 6),
                         stringsAsFactors = FALSE)
  mock_input <- list()
  mock_input[[NAMESPACE_STATMODEL$comparison_mode]] <- CONSTANTS_STATMODEL$comparison_mode_response_curve
  mock_input[[NAMESPACE_STATMODEL$modeling_response_curve_increasing_trend]] <- TRUE
  mock_input[[NAMESPACE_STATMODEL$visualization_response_curve_ratio_scale]] <- FALSE

  resolved <- stats::setNames(c(0.98, 0.93), c("0h", "6h"))
  result <- generate_analysis_code(
    list(), list(), comp_mat, mock_input,
    app_template = TEMPLATES$protein_turnover,
    tracer_constants = list(values = resolved,
                            source = CONSTANTS_QC$tracer_source_upload,
                            file = "constants.csv"))

  expect_identical(eval_emitted_tracer_constants(result), resolved)
  expect_true(grepl("# Tracer constants: uploaded on the data-processing page",
                    result, fixed = TRUE))
})

test_that("generate_analysis_code labels an unrun QC page as such rather than as no upload", {
  # Reachable, not a defect: the Download-code button only requires a contrast
  # matrix, and the upload-summarized-abundances flow never renders the tracer
  # panel at all.
  mockery::stub(generate_analysis_code, "preprocessDataCode", "# preprocess\n")

  comp_mat <- data.frame(GROUP = c("0h", "6h"), TimeVal = c(0, 6),
                         stringsAsFactors = FALSE)
  mock_input <- list()
  mock_input[[NAMESPACE_STATMODEL$comparison_mode]] <- CONSTANTS_STATMODEL$comparison_mode_response_curve
  mock_input[[NAMESPACE_STATMODEL$modeling_response_curve_increasing_trend]] <- TRUE

  result <- generate_analysis_code(
    list(), list(), comp_mat, mock_input,
    app_template = TEMPLATES$protein_turnover)

  expect_identical(eval_emitted_tracer_constants(result),
                   stats::setNames(c(1, 1), c("0h", "6h")))
  expect_true(grepl("the data-processing page was not run in this session",
                    result, fixed = TRUE))
  expect_false(grepl("no file was supplied", result, fixed = TRUE))
})

test_that("generate_analysis_code resolves the NULL snapshot only inside the turnover branch", {
  # On the group-comparison path comp_mat is a bare MATRIX, where `$GROUP`
  # throws "$ operator is invalid for atomic vectors". Resolving the NULL
  # snapshot at the top of the function rather than inside the turnover branch
  # would therefore take the Download-code button out on every template.
  mockery::stub(generate_analysis_code, "preprocessDataCode", "# preprocess\n")

  comp_mat <- matrix(c(1, -1), nrow = 1, dimnames = list("C2-C1", c("C1", "C2")))
  mock_input <- list()
  mock_input[[NAMESPACE_STATMODEL$visualization_plot_type]] <- "VolcanoPlot"

  for (template in c(TEMPLATES$default, TEMPLATES$chemoproteomics)) {
    result <- generate_analysis_code(
      list(), list(DDA_DIA = "DDA", BIO = "Protein"), comp_mat, mock_input,
      app_template = template, tracer_constants = NULL)
    expect_true(grepl("groupComparison", result, fixed = TRUE),
                info = paste("group comparison path must still build on", template))
    expect_false(grepl("tracer_constants", result, fixed = TRUE),
                 info = paste("no tracer constants belong in the", template, "script"))
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

# ============================================================================
# Tests for the Turnover Ratios panel (register_qc_turnover)
# ============================================================================

library(shiny)

# Minimal FeatureLevelData with paired heavy/light measurements at three
# timepoints, enough for calculateTurnoverRatios to return rows.
create_mock_turnover_summary <- function() {
  groups <- c("0hr", "4hr", "24hr")
  feature <- expand.grid(GROUP = groups, LABEL = c("H", "L"),
                         PEPTIDE = c("PEPTIDEK_2", "PEPTIDER_2"),
                         stringsAsFactors = FALSE)
  feature$PROTEIN <- "ProtA"
  feature$RUN <- match(feature$GROUP, groups)
  feature$INTENSITY <- seq_len(nrow(feature)) * 1000
  # One run per condition, so the panel takes the FeatureLevelData branch.
  protein <- data.frame(RUN = seq_along(groups), Protein = "ProtA", LABEL = "L",
                        LogIntensities = c(10, 11, 12), GROUP = groups,
                        stringsAsFactors = FALSE)
  list(FeatureLevelData = feature, ProteinLevelData = protein)
}

turnover_panel_server <- function(summary_data) {
  function(input, output, session) {
    register_qc_turnover(
      input, output, session,
      app_template = reactive(TEMPLATES$protein_turnover),
      get_data = reactive(summary_data$FeatureLevelData),
      get_condition_metadata = reactive(
        data.frame(Condition = unique(summary_data$ProteinLevelData$GROUP),
                   stringsAsFactors = FALSE)),
      preprocess_data = reactive(summary_data)
    )
  }
}

test_that("Download Ratios is disabled before summarization is run", {
  testServer(turnover_panel_server(create_mock_turnover_summary()), {
    panel <- as.character(output$turnover_ratios_panel$html)
    expect_true(grepl("download_turnover_ratios", panel, fixed = TRUE))
    # shinyjs marks its own disabling with the shinyjs-disabled class; the
    # plain "disabled" class is on every downloadButton until the client binds
    # it, so it cannot distinguish the two states.
    expect_true(grepl("shinyjs-disabled", panel, fixed = TRUE),
                info = "Button should render disabled until ratios exist")
  })
})

test_that("Download Ratios is enabled once ratios are calculated", {
  testServer(turnover_panel_server(create_mock_turnover_summary()), {
    session$setInputs(run = 1)
    panel <- as.character(output$turnover_ratios_panel$html)
    expect_true(grepl("download_turnover_ratios", panel, fixed = TRUE))
    # Rendered from the ratios state, so it survives a re-render of the panel
    # (outputs in an inactive tabPanel are suspended and re-render on
    # activation, which a one-off shinyjs::enable() message does not).
    expect_false(grepl("shinyjs-disabled", panel, fixed = TRUE))
  })
})

# ============================================================================
# Tests for the tracer-constants upload observer (register_qc_turnover)
# ============================================================================

# The eight conditions of the working turnover dataset. Every fixture in
# data/tracer/ is keyed to these.
turnover_conditions <- c("0hr", "1hr", "4hr", "12hrs", "24hrs",
                         "48hrs", "96hrs", "168hrs")

# register_qc_turnover's return is captured through an environment rather than
# session$getReturned(), which is not available for a bare server function.
tracer_upload_server <- function(capture,
                                 conditions = reactive(
                                   data.frame(Condition = turnover_conditions,
                                              stringsAsFactors = FALSE)),
                                 template = reactive(TEMPLATES$protein_turnover)) {
  function(input, output, session) {
    capture$returned <- register_qc_turnover(
      input, output, session,
      app_template = template,
      get_data = reactive(data.frame(x = 1)),
      get_condition_metadata = conditions,
      preprocess_data = reactive(NULL))
  }
}

# Shiny delivers a fileInput's value as a one-row data frame.
tracer_file_input <- function(path) {
  data.frame(name = basename(path), datapath = path, stringsAsFactors = FALSE)
}

tracer_upload_state <- function(fixture) {
  capture <- new.env()
  state <- NULL
  testServer(tracer_upload_server(capture), {
    session$setInputs(tracer_constants_file =
                        tracer_file_input(test_path("data/tracer", fixture)))
    state <<- capture$returned$tracer_upload()
  })
  state
}

test_that("a well-formed tracer file is accepted with one value per condition", {
  for (fixture in c("tracer_good.csv", "tracer_floor.csv")) {
    state <- tracer_upload_state(fixture)
    expect_identical(state$state, "valid", info = fixture)
    # Names are the raw condition strings: calculateTurnoverRatios re-keys the
    # vector through parse_timepoint(names(x)), so they must survive intact.
    expect_identical(names(state$values), turnover_conditions, info = fixture)
  }
  # tracer_floor.csv sits on the inclusive 0.01 boundary; a ">" written where
  # ">=" was meant passes every other fixture and is caught only here.
  expect_equal(unname(tracer_upload_state("tracer_floor.csv")$values[["12hrs"]]),
               CONSTANTS_QC$tracer_min)
})

test_that("every malformed tracer file is rejected rather than silently defaulted", {
  # Rejected, not absent: absent means "the user declined the correction" and
  # runs with all 1s.
  rejected <- c("tracer_missing_col.csv", "tracer_nonnumeric.csv",
                "tracer_zero.csv", "tracer_tiny.csv", "tracer_over.csv",
                "tracer_unknown.csv", "tracer_partial.csv", "tracer_dup.csv",
                "tracer_dupcol.csv", "tracer_empty.csv",
                "tracer_headeronly.csv", "tracer_quote.csv")
  for (fixture in rejected) {
    state <- tracer_upload_state(fixture)
    expect_identical(state$state, "rejected", info = fixture)
    expect_null(state$values, info = fixture)
  }
})

test_that("a row fread silently discards rejects the file", {
  # fread drops an unparseable row with a warning, not an error. A dropped row
  # for a condition another row already covers passes the coverage check, so
  # the file looks complete while the value the user typed is gone.
  path <- file.path(tempdir(), "tracer_footer.csv")
  writeLines(c("GROUP,TracerConstant",
               paste0(turnover_conditions, ",",
                      c(0.98, 0.97, 0.96, 0.95, 0.94, 0.93, 0.92, 0.91)),
               "0hr,0,42"),
             path)
  capture <- new.env()
  testServer(tracer_upload_server(capture), {
    session$setInputs(tracer_constants_file = tracer_file_input(path))
    expect_identical(capture$returned$tracer_upload()$state, "rejected")
  })
})

test_that("a rejected file does not leave the previous good file's values in play", {
  capture <- new.env()
  testServer(tracer_upload_server(capture), {
    session$setInputs(tracer_constants_file =
                        tracer_file_input(test_path("data/tracer/tracer_good.csv")))
    expect_identical(capture$returned$tracer_upload()$state, "valid")

    session$setInputs(tracer_constants_file =
                        tracer_file_input(test_path("data/tracer/tracer_unknown.csv")))
    state <- capture$returned$tracer_upload()
    expect_identical(state$state, "rejected")
    expect_null(state$values)
  })
})

test_that("a NULL condition-metadata argument does not error", {
  # qcServer defaults get_condition_metadata to NULL and documents it as
  # "reactive (or NULL)". An unguarded get_condition_metadata() is NULL(),
  # which throws on the first flush of the session, on every template.
  capture <- new.env()
  expect_no_error(
    testServer(tracer_upload_server(capture, conditions = NULL,
                                    template = reactive(TEMPLATES$default)), {
      session$setInputs(run = 1)
    })
  )
})

test_that("switching away from protein turnover clears a blocking tracer state", {
  # The tracer panel, and with it the Clear button, is gated on the turnover
  # template. A rejected state surviving a template switch would keep the Run
  # button disabled with no on-screen way to re-enable it.
  capture <- new.env()
  template <- reactiveVal(TEMPLATES$protein_turnover)
  testServer(tracer_upload_server(capture, template = template), {
    session$setInputs(tracer_constants_file =
                        tracer_file_input(test_path("data/tracer/tracer_zero.csv")))
    expect_identical(capture$returned$tracer_upload()$state, "rejected")

    template(TEMPLATES$default)
    session$flushReact()
    expect_identical(capture$returned$tracer_upload()$state, "absent")
  })
})

test_that("a valid upload survives a metadata rewrite but not a change of conditions", {
  # condition_metadata is rewritten wholesale by ordinary actions -- re-clicking
  # the load page's proceed button, uploading a GROUP mapping -- so dropping the
  # upload on every rewrite would be its own bug. Only a changed condition SET
  # invalidates it.
  capture <- new.env()
  metadata <- reactiveVal(data.frame(Condition = turnover_conditions,
                                     stringsAsFactors = FALSE))
  testServer(tracer_upload_server(capture, conditions = metadata), {
    session$setInputs(tracer_constants_file =
                        tracer_file_input(test_path("data/tracer/tracer_good.csv")))
    expect_identical(capture$returned$tracer_upload()$state, "valid")

    metadata(data.frame(Condition = turnover_conditions, stringsAsFactors = FALSE))
    session$flushReact()
    expect_identical(capture$returned$tracer_upload()$state, "valid")

    metadata(data.frame(Condition = c(turnover_conditions, "336hrs"),
                        stringsAsFactors = FALSE))
    session$flushReact()
    expect_identical(capture$returned$tracer_upload()$state, "rejected")
  })
})

test_that("clearing the upload returns to the neutral all-ones state", {
  capture <- new.env()
  testServer(tracer_upload_server(capture), {
    session$setInputs(tracer_constants_file =
                        tracer_file_input(test_path("data/tracer/tracer_good.csv")))
    expect_identical(capture$returned$tracer_upload()$state, "valid")

    session$setInputs(tracer_constants_clear = 1)
    state <- capture$returned$tracer_upload()
    expect_identical(state$state, "absent")
    expect_null(state$values)
  })
})

# ============================================================================
# The constants resolved at Run are the ones the fit actually receives
# ============================================================================

# The upload observer tests above stop at the reactiveVal; these carry the value
# the rest of the way. calculateTurnoverRatios is stubbed so the assertion is on
# the tracer_constants argument itself rather than on downstream ratios, which
# would depend on MSstatsResponse's arithmetic and on turnover fixture data this
# repo does not have.

# Two distinct runs per condition, so use_protein_level is TRUE and the
# ProteinLevelData branch is taken.
turnover_pld <- function() {
  pld <- data.frame(
    GROUP = rep(turnover_conditions, each = 2),
    Protein = "P1", LogIntensities = 1, LABEL = "H",
    stringsAsFactors = FALSE)
  pld$RUN <- seq_len(nrow(pld))
  pld
}

# One run per condition, so use_protein_level is FALSE and the FeatureLevelData
# branch is taken instead. Pairs with turnover_fld() below.
turnover_pld_single <- function() {
  pld <- data.frame(
    GROUP = turnover_conditions,
    Protein = "P1", LogIntensities = 1, LABEL = "H",
    stringsAsFactors = FALSE)
  pld$RUN <- seq_len(nrow(pld))
  pld
}

# Deliberately uses the FeatureLevelData column names (PEPTIDE/PROTEIN/
# INTENSITY) rather than the ProteinLevelData ones, so a test can tell which of
# the two branches actually called the fit.
turnover_fld <- function() {
  fld <- data.frame(
    GROUP = turnover_conditions,
    PROTEIN = "P1", PEPTIDE = "PEP1", INTENSITY = 1, LABEL = "H",
    stringsAsFactors = FALSE)
  fld$RUN <- seq_len(nrow(fld))
  fld
}

# template and preprocess are injectable so the regressions below can switch the
# app template mid-session and make a second Run bail out early.
tracer_run_server <- function(capture,
                              conditions = reactive(
                                data.frame(Condition = turnover_conditions,
                                           stringsAsFactors = FALSE)),
                              template = reactive(TEMPLATES$protein_turnover),
                              preprocess = reactive(
                                list(ProteinLevelData = turnover_pld()))) {
  fn <- function(input, output, session) {
    capture$returned <- register_qc_turnover(
      input, output, session,
      app_template = template,
      get_data = reactive(data.frame(x = 1)),
      get_condition_metadata = conditions,
      preprocess_data = preprocess)
  }
  mockery::stub(fn, "register_qc_turnover", register_qc_turnover)
  fn
}

# Captures the tracer_constants argument handed to the fit, and which of the
# two branches called it -- the branch choice is otherwise invisible to the
# tests, because preprocess_data()$FeatureLevelData is NULL in this harness.
with_stubbed_fit <- function() {
  capture <- new.env()
  capture$constants_seen <- NULL
  capture$fit_calls <- 0L
  capture$data_seen <- NULL
  fake_fit <- function(data, ...) {
    args <- list(...)
    capture$fit_calls <- capture$fit_calls + 1L
    capture$constants_seen <- args$tracer_constants
    capture$data_seen <- data
    data.frame(Protein = "P1", TimeVal = 0, H_frac = 0.5)
  }
  list(capture = capture, fit = fake_fit)
}

test_that("a valid upload reaches the fit; no upload sends all 1s", {
  for (case in list(list(fixture = NULL, source = "none"),
                    list(fixture = "tracer_good.csv", source = "upload"))) {
    ctx <- with_stubbed_fit()
    capture <- ctx$capture
    server <- tracer_run_server(capture)
    mockery::stub(server, "calculateTurnoverRatios", ctx$fit, depth = 2)

    testServer(server, {
      if (!is.null(case$fixture)) {
        session$setInputs(tracer_constants_file =
                            tracer_file_input(test_path("data/tracer", case$fixture)))
      }
      session$setInputs(run = 1)
      capture$snapshot <- capture$returned$tracer_constants()
    })

    label <- case$source
    expected <- if (is.null(case$fixture)) {
      stats::setNames(rep(1, length(turnover_conditions)), turnover_conditions)
    } else {
      stats::setNames(c(0.98, 0.97, 0.96, 0.95, 0.94, 0.93, 0.92, 0.91),
                      turnover_conditions)
    }

    # Names AND values AND order: calculateTurnoverRatios re-keys by name, so a
    # correct-values/wrong-names vector silently mis-assigns every constant.
    expect_equal(capture$constants_seen, expected, info = label)
    expect_equal(capture$snapshot$values, expected, info = label)
    expect_identical(capture$snapshot$source, case$source, info = label)
    # The ProteinLevelData branch, not the FeatureLevelData one: the harness
    # supplies two runs per condition, and the fake fit otherwise makes the two
    # branches indistinguishable.
    expect_identical(capture$fit_calls, 1L, info = label)
    expect_identical(nrow(capture$data_seen), 16L, info = label)
    # Provenance is three-state, so "which file" has to survive alongside "from
    # a file at all".
    if (is.null(case$fixture)) {
      expect_null(capture$snapshot$file, info = label)
    } else {
      expect_identical(capture$snapshot$file, case$fixture, info = label)
    }
  }
})

test_that("the FeatureLevelData branch also receives the tracer constants", {
  # Every other fit test supplies two runs per condition and so only exercises
  # the ProteinLevelData branch; without this one, dropping tracer_constants =
  # from the single-replicate branch would leave the suite green.
  ctx <- with_stubbed_fit()
  capture <- ctx$capture
  server <- tracer_run_server(
    capture,
    preprocess = reactive(list(ProteinLevelData = turnover_pld_single(),
                               FeatureLevelData = turnover_fld())))
  mockery::stub(server, "calculateTurnoverRatios", ctx$fit, depth = 2)

  testServer(server, {
    session$setInputs(tracer_constants_file =
                        tracer_file_input(test_path("data/tracer", "tracer_good.csv")))
    session$setInputs(run = 1)
  })

  expected <- stats::setNames(c(0.98, 0.97, 0.96, 0.95, 0.94, 0.93, 0.92, 0.91),
                              turnover_conditions)

  # Which branch ran: the fake fit is shared, so the data's column names are the
  # only witness. INTENSITY means FeatureLevelData; LogIntensities would mean the
  # harness fell back to the ProteinLevelData branch.
  expect_identical(capture$fit_calls, 1L)
  expect_true("INTENSITY" %in% names(capture$data_seen))
  expect_false("LogIntensities" %in% names(capture$data_seen))

  expect_equal(capture$constants_seen, expected)
})

test_that("rejection messages name the tracer file, not the GROUP mapping upload", {
  # qc_mapping_group_errors' defaults name "GROUP mapping" and
  # "ProteinLevelData"; the GROUP mapping is a DIFFERENT upload on this same
  # page, so the default wording sends a user who mis-typed a condition here off
  # to edit an unrelated file. The labels are supplied only at the call site in
  # R/qc-server-turnover.R, so nothing else pins them.
  cases <- list(
    list(fixture = "tracer_unknown.csv", reference = TRUE),
    list(fixture = "tracer_partial.csv", reference = TRUE),
    # The duplicate-rows message names the subject only, not the reference.
    list(fixture = "tracer_dup.csv", reference = FALSE))

  for (case in cases) {
    ctx <- with_stubbed_fit()
    capture <- ctx$capture
    capture$messages <- character(0)
    server <- tracer_run_server(capture)
    # Only the upload observer runs here (Run is never pressed), so the fit is
    # never reached and must NOT be stubbed as well: two mockery::stub calls on
    # the same function do not compose, and stubbing the fit first silently
    # discards this one.
    mockery::stub(
      server, "showNotification",
      function(ui, ...) {
        capture$messages <- c(capture$messages,
                              paste(as.character(ui), collapse = " "))
        NULL
      }, depth = 2)

    testServer(server, {
      session$setInputs(tracer_constants_file =
                          tracer_file_input(test_path("data/tracer", case$fixture)))
      capture$state <- capture$returned$tracer_upload()$state
    })

    msg <- paste(capture$messages, collapse = " ")
    expect_identical(capture$state, "rejected", info = case$fixture)
    expect_true(grepl("The tracer constants file", msg, fixed = TRUE),
                info = case$fixture)
    if (case$reference) {
      expect_true(grepl("the experimental conditions", msg, fixed = TRUE),
                  info = case$fixture)
    }
    # The two strings that would send the user to the wrong file.
    expect_false(grepl("GROUP mapping", msg, fixed = TRUE), info = case$fixture)
    expect_false(grepl("ProteinLevelData", msg, fixed = TRUE), info = case$fixture)
  }
})

test_that("Run is refused server-side while an upload is pending or rejected", {
  # toggleState disables the button on the CLIENT, and shinyjs::onevent needs a
  # server round trip to set "pending", so a click in that window reaches the
  # server with the button still live.
  for (fixture in c("tracer_partial.csv", "tracer_nonnumeric.csv")) {
    ctx <- with_stubbed_fit()
    capture <- ctx$capture
    server <- tracer_run_server(capture)
    mockery::stub(server, "calculateTurnoverRatios", ctx$fit, depth = 2)

    testServer(server, {
      session$setInputs(tracer_constants_file =
                          tracer_file_input(test_path("data/tracer", fixture)))
      session$setInputs(run = 1)
      capture$state <- capture$returned$tracer_upload()$state
      capture$snapshot <- capture$returned$tracer_constants()
    })

    expect_identical(capture$state, "rejected", info = fixture)
    # Neither run with 1s nor record a snapshot: the fit is never reached.
    expect_identical(capture$fit_calls, 0L, info = fixture)
    expect_null(capture$snapshot, info = fixture)
  }
})

test_that("the snapshot never outlives the ratios it describes", {
  # A snapshot left behind by a run that produced nothing would have the
  # generated script cite tracer constants for an empty table.
  ctx <- with_stubbed_fit()
  capture <- ctx$capture
  template <- reactiveVal(TEMPLATES$protein_turnover)
  metadata <- reactiveVal(data.frame(Condition = turnover_conditions,
                                     stringsAsFactors = FALSE))
  pld <- data.frame(GROUP = rep(turnover_conditions, each = 2),
                    Protein = "P1", LogIntensities = 1, LABEL = "H",
                    stringsAsFactors = FALSE)
  pld$RUN <- seq_len(nrow(pld))

  fn <- function(input, output, session) {
    capture$returned <- register_qc_turnover(
      input, output, session,
      app_template = template,
      get_data = reactive(data.frame(x = 1)),
      get_condition_metadata = metadata,
      preprocess_data = reactive(list(ProteinLevelData = pld)))
  }
  mockery::stub(fn, "register_qc_turnover", register_qc_turnover)
  mockery::stub(fn, "calculateTurnoverRatios", ctx$fit, depth = 2)

  testServer(fn, {
    session$setInputs(tracer_constants_file =
                        tracer_file_input(test_path("data/tracer", "tracer_good.csv")))
    session$setInputs(run = 1)
    capture$after_run <- capture$returned$tracer_constants()

    # Leaving turnover makes the ratios eventReactive's own req() fail, and
    # shiny caches that error for the rest of the session.
    template(TEMPLATES$default)
    session$flushReact()
    capture$after_switch <- capture$returned$tracer_constants()
  })

  expect_identical(capture$after_run$source, "upload")
  expect_null(capture$after_switch)
})

test_that("a refused re-Run clears the snapshot left by an earlier good Run", {
  # A good run banks a snapshot, then the user replaces the file with a bad one
  # and presses Run again. The re-Run is refused, so the ratios table still
  # shows the FIRST run's numbers; a surviving snapshot would have the generated
  # script keep citing a file the app has since rejected.
  ctx <- with_stubbed_fit()
  capture <- ctx$capture
  server <- tracer_run_server(capture)
  mockery::stub(server, "calculateTurnoverRatios", ctx$fit, depth = 2)

  testServer(server, {
    session$setInputs(tracer_constants_file =
                        tracer_file_input(test_path("data/tracer", "tracer_good.csv")))
    session$setInputs(run = 1)
    capture$after_good <- capture$returned$tracer_constants()

    session$setInputs(tracer_constants_file =
                        tracer_file_input(test_path("data/tracer", "tracer_partial.csv")))
    session$setInputs(run = 2)
    capture$after_bad <- capture$returned$tracer_constants()
  })

  expect_identical(capture$after_good$source, "upload")
  expect_identical(capture$after_good$file, "tracer_good.csv")
  # The refused re-Run never reaches the fit, and leaves nothing behind.
  expect_identical(capture$fit_calls, 1L)
  expect_null(capture$after_bad)
})

test_that("the snapshot is taken at Run and does not follow a later upload", {
  # A live view of the upload would make the ratios table show one set of
  # constants while the downloadable script emitted another.
  ctx <- with_stubbed_fit()
  capture <- ctx$capture
  server <- tracer_run_server(capture)
  mockery::stub(server, "calculateTurnoverRatios", ctx$fit, depth = 2)

  testServer(server, {
    session$setInputs(run = 1)
    capture$before <- capture$returned$tracer_constants()
    session$setInputs(tracer_constants_file =
                        tracer_file_input(test_path("data/tracer", "tracer_good.csv")))
    capture$after <- capture$returned$tracer_constants()
    capture$state <- capture$returned$tracer_upload()$state
  })

  expect_identical(capture$state, "valid")
  expect_identical(capture$after$source, "none")
  expect_equal(capture$after$values, capture$before$values)
})

test_that("the exposed constants reactive is NULL before Run and never errors", {
  # This value is read from the Download-code path on all four templates, so
  # reading it before the QC page has run must not raise shiny.silent.error.
  capture <- new.env()
  before <- "unset"
  testServer(tracer_upload_server(capture), {
    before <<- capture$returned$tracer_constants()
  })
  expect_null(before)
})

# ============================================================================
# The qcServer -> server.R hop: return shape
# ============================================================================

# Wraps the real qcServer so its return value can be inspected. A shape
# assertion rather than a behavioural one: this hop is plumbing, and it breaks
# by a renamed or dropped key.
qc_server_return_capture <- function(capture, template = TEMPLATES$protein_turnover) {
  function(input, output, session) {
    capture$returned <- qcServer(
      input, output, session,
      parent_session = session,
      loadpage_input = reactive(list(BIO = "Protein", DDA_DIA = "LType",
                                     filetype = "standard", proceed1 = 0)),
      get_data = reactive(NULL),
      app_template = reactive(template),
      get_condition_metadata = reactive(NULL)
    )
  }
}

test_that("qcServer exposes tracerConstants alongside the ratios", {
  capture <- new.env()
  keys <- NULL
  is_reactive <- NA
  value <- "unset"

  testServer(qc_server_return_capture(capture), {
    keys <<- names(capture$returned)
    is_reactive <<- is.function(capture$returned$tracerConstants)
    # Must be readable before anything has been run, on any template: server.R
    # hands it to statmodelServer, which reads it from the Download-code path
    # for all four templates.
    value <<- capture$returned$tracerConstants()
  })

  expect_true("tracerConstants" %in% keys,
              info = "server.R reads qc_values$tracerConstants by exactly this name")
  expect_true(all(c("input", "preprocessData", "turnoverRatios") %in% keys),
              info = "the pre-existing keys must not be disturbed")
  expect_true(is_reactive)
  expect_null(value)
})

test_that("qcServer's tracerConstants is readable on a non-turnover template", {
  capture <- new.env()
  value <- "unset"
  testServer(qc_server_return_capture(capture, template = TEMPLATES$default), {
    value <<- capture$returned$tracerConstants()
  })
  expect_null(value)
})

# ============================================================================
# Divergence regressions: the ratios table and the generated script must never
# disagree about which constants were used. Both cases below are read after the
# divergence, not at Run, when the two values necessarily agree.
# ============================================================================

test_that("a template round-trip clears the ratios table with its snapshot", {
  # turnover_ratios is an eventReactive, so only a fresh Run invalidates its
  # cache -- the template observer cannot, though it does clear the snapshot. So
  # switching away and back used to redraw a table carrying the uploaded
  # correction while the script emitted all-1s stamped "not run in this session".
  ctx <- with_stubbed_fit()
  capture <- ctx$capture
  template <- reactiveVal(TEMPLATES$protein_turnover)
  server <- tracer_run_server(capture, template = template)
  mockery::stub(server, "calculateTurnoverRatios", ctx$fit, depth = 2)

  testServer(server, {
    session$setInputs(tracer_constants_file =
                        tracer_file_input(test_path("data/tracer/tracer_good.csv")))
    session$setInputs(run = 1)
    expect_identical(capture$returned$tracer_constants()$source, "upload")
    expect_s3_class(capture$returned$ratios(), "data.frame")

    template(TEMPLATES$default)
    session$flushReact()
    template(TEMPLATES$protein_turnover)
    session$flushReact()

    # No snapshot, so no table: the user is asked to run again rather than
    # shown ratios the downloadable script would not reproduce.
    expect_null(capture$returned$tracer_constants())
    expect_error(capture$returned$ratios(), class = "shiny.silent.error")
  })
})

test_that("a Run that bails before the fit leaves no stale snapshot", {
  # The snapshot is cleared at the very top of the eventReactive, ahead of its
  # req() guards. With the clear placed after them, a second Run that bailed --
  # here because summarization returned NULL -- emptied the table while the
  # first run's constants stayed on record for the script.
  ctx <- with_stubbed_fit()
  capture <- ctx$capture
  prep <- reactiveVal(list(ProteinLevelData = turnover_pld()))
  server <- tracer_run_server(capture, preprocess = function() prep())
  mockery::stub(server, "calculateTurnoverRatios", ctx$fit, depth = 2)

  testServer(server, {
    session$setInputs(tracer_constants_file =
                        tracer_file_input(test_path("data/tracer/tracer_good.csv")))
    session$setInputs(run = 1)
    expect_identical(capture$returned$tracer_constants()$source, "upload")

    prep(NULL)
    session$setInputs(run = 2)
    session$flushReact()

    expect_null(capture$returned$tracer_constants())
    expect_error(capture$returned$ratios(), class = "shiny.silent.error")
    # The bailed Run must not have reached the fit at all.
    expect_identical(capture$fit_calls, 1L)
  })
})

test_that("the snapshot is committed only after the fit returns", {
  # A reactiveVal write is not rolled back when the eventReactive body aborts,
  # and calculateTurnoverRatios throws on plausible data (ProteinLevelData with
  # no "H" rows raises "Element `H` doesn't exist"). Committing before the fit
  # therefore left the script citing constants for a table that never drew.
  #
  # The snapshot is read from INSIDE the fit rather than by making the fit throw:
  # an error there is unhandled in the observer forcing this eventReactive, which
  # destroys the session and leaves nothing readable afterwards.
  ctx <- with_stubbed_fit()
  capture <- ctx$capture
  ordering_fit <- function(data, ...) {
    capture$snapshot_during_fit <- capture$returned$tracer_constants()
    ctx$fit(data, ...)
  }
  server <- tracer_run_server(capture)
  mockery::stub(server, "calculateTurnoverRatios", ordering_fit, depth = 2)

  testServer(server, {
    session$setInputs(tracer_constants_file =
                        tracer_file_input(test_path("data/tracer/tracer_good.csv")))
    session$setInputs(run = 1)

    expect_identical(capture$fit_calls, 1L)
    # Nothing on record while the fit is still in flight and could throw...
    expect_null(capture$snapshot_during_fit)
    # ...and on record once it has returned.
    expect_identical(capture$returned$tracer_constants()$source, "upload")
    expect_identical(capture$returned$tracer_constants()$file, "tracer_good.csv")
  })
})
